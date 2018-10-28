;;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'project)
(require 'subr-x)

;; declare our devotion to speed at all costs
(cl-declaim (optimize (speed 3) (safety 0)))

(defcustom projek-recent-projects 10
  "Maximum number of projects to keep in memory."
  :type '(integer)
  :group 'projek)

(defconst projek-cache-dir
  (expand-file-name "emacs/projek/" (or (getenv "XDG_CACHE_HOME")  "~/.cache"))
  "Path to top-level cache directory for projek package")

(defvar projek--recent-projects (make-hash-table :test 'equal)
  "Map of project.el identifier (type . path) to project node")

(defvar projek--current-project nil
  "project.el identifier (type . path) of current project")

;; Utility functions
(defun projek--globs-to-regexp (globs)
  (concat "\\(?:"
          (mapconcat #'identity (mapcar #'dired-glob-regexp globs) "\\|" )
          "\\)"))

(defun projek--flatten-path (path)
  "Replace path delimiters with legal but unlikely file name characters"
  (replace-regexp-in-string "[/]" "!" path t t))

(defun projek--read-object(path)
  "Read object from PATH."
  (with-demoted-errors
      "Error during object read: %S"
    (when (file-exists-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (read (buffer-string))))))

(defun projek--write-object (object path)
  "Write lisp object OBJECT to PATH"
  (if (file-writable-p path)
      (with-temp-file path
        (insert (prin1-to-string object)))
    (message "Unable to write object to [%s]" path)))

;; End Utility functions

(cl-defstruct (projek--pnode (:constructor projek--pnode--create)
                             (:copier nil))
  roots project last-used last-saved)

(cl-defstruct (projek--dnode (:constructor projek--dnode--create)
                             (:copier nil))
  path mtime dirs files ignore-re keep-re)

(cl-defun projek--pnode-create (project &rest args)
  (when-let* ((pnode (apply #'projek--pnode--create
                            :project project :last-used (current-time) args))
              (roots (mapcar #'file-name-as-directory (project-roots project)))
              (roots-cache (projek--project-cache-dir pnode "roots"))
              (rnodes (mapcar
                       (lambda (path) (projek--rnode-create pnode roots-cache path))
                       roots)))
    (setf (projek--pnode-roots pnode) rnodes)
    ;; add project to recent projects and prune
    (puthash project pnode projek--recent-projects)
    (projek--prune-recent-projects)
    pnode))

;; TBD
(defun projek--prune-recent-projects ())

(defun projek--recent-projects ()
  (hash-table-keys projek--recent-projects))


(defun projek-activate-project (project)
  (unless (equal projek--current-project project)
    (let ((pnode (or (gethash project projek--recent-projects)
                     (projek--pnode-create project))))
      (projek--activate-pnode pnode))))

(defun projek-deactivate-project (project)
  (when-let ((pnode (gethash project projek--recent-projects)))
    (projek--save-project pnode)))

(defun projek--activate-pnode (pnode)
  (let ((project (projek--pnode-project pnode)))
    (puthash project pnode projek--recent-projects)
    (projek-deactivate-project projek--current-project)
    (setf (projek--pnode-last-used pnode) (current-time)
          projek--current-project project)
    (projek--prune-recent-projects)))

(defun projek--project-cache-dir (pnode &optional subdir)
  "Return the path of the project's cache dir or optionally a directory within it"
  (let* ((project-dir (cdr (projek--pnode-project pnode)))
         (project-file-name (projek--flatten-path project-dir))
         (project-cache-dir (expand-file-name project-file-name projek-cache-dir)))
    (if subdir
        (expand-file-name subdir project-cache-dir)
      project-cache-dir)))


(cl-defun projek--dnode-create (path &rest args)
  (apply #'projek--dnode--create
         :path path
         :dirs (make-hash-table :test 'equal)
         args))

(cl-defun projek--rnode-create (pnode roots-cache path)
  (let* ((iglobs (project-ignores (projek--pnode-project pnode) path))
         (ignore (projek--globs-to-regexp iglobs))
         (rpath-cache (expand-file-name (projek--flatten-path path) roots-cache)))
    (or (projek--read-object rpath-cache)
        (projek--dnode-create path
                              :ignore-re ignore
                              :keep-re "$^"))))

(defun projek--save-project (pnode)
  (let ((roots-cache (projek--project-cache-dir pnode "roots")))
    (make-directory roots-cache t)
    (projek--foreach-root rnode in pnode
      (let* ((rpath (projek--dnode-path rnode))
             (rpath-cache (expand-file-name (projek--flatten-path rpath) roots-cache)))
        (projek--write-object rnode rpath-cache)))))

(defun projek--find-rnode (pnode dpath)
  "Find the rnode corresponding to directory path DPATH
  PNODE is the project node
  DPATH is the path of a directory"
  (seq-find (lambda (rnode) (string-prefix-p (projek--dnode-path rnode) dpath))
            (projek--pnode-roots pnode)))

(defun projek--find-dnode (pnode rnode dpath)
  "Find dnode associated with directory path DPATH.
PNODE is the project node.
RNODE is the root dnode.
Returns dnode or nil if not found"
  (when-let* ((dnode rnode)
              (rootpath (projek--dnode-path rnode))
              (relpath (substring dpath (length rootpath))))
    (unless (string-empty-p relpath)
      (let ((dirs (split-string (directory-file-name relpath) "/")))
        (while (and dnode dirs)
          (setq dnode (gethash (car dirs) (projek--dnode-dirs dnode)))
          (setq dirs (cdr dirs)))))
    dnode))

(defun projek--should-ignore (pnode rnode name)
  (thread-yield)
  (and (string-match (projek--dnode-ignore-re rnode) name)
       (not (string-match (projek--dnode-keep-re rnode) name))))

(defun projek--traverse-dnode (dnode fun)
  "Traverse a directory.
DNODE directory node to traverse.
FUN function to call on each directory node"
  (funcall fun dnode)
  (dolist (dir (hash-table-values (projek--dnode-dirs dnode)))
    (projek--traverse-dnode dir fun)))

(defmacro projek--foreach-root (rvar in pnode body)
  "Traverse the root directories, setting RVAR to the root dnode of each"
  (declare (indent 3))
  `(dolist (,rvar (projek--pnode-roots ,pnode)) ,body))

(defmacro projek--foreach-dir (dvar in rnode body)
  (declare (indent 3))
  `(projek--traverse-dnode ,rnode (lambda (,dvar) ,body)))

(defun projek--print-tree (pnode)
  (projek--foreach-root rnode in pnode
    (let* ((rpath (projek--dnode-path rnode))
           (rdepth (length (split-string rpath "/")))
           (dcount 0)
           (tcount 0))
      (projek--foreach-dir dnode in rnode
        (let* ((fcount (length (projek--dnode-files dnode)))
               (dpath (projek--dnode-path dnode))
               (depth (- (length (split-string dpath "/")) rdepth))
               (name (file-name-nondirectory (directory-file-name dpath))))
          (setq dcount (1+ dcount))
          (setq tcount (+ tcount fcount))
          (message "%s%s -> %d files"
                   (make-string (* depth 2) ?\s)
                   name fcount )))
      (message "#### Dirs %d  Files %d ####" dcount tcount))))

(defun projek--dnode-changed-p (dnode dpath)
  ;;(message "path changed check %s" dpath)
  (let ((dattr (file-attributes dpath))
        (dmtime (projek--dnode-mtime dnode)))
    (and (eq 't (car dattr)) (not (equal dmtime (nth 5 dattr))))))

(defun projek--update-dnode (pnode rnode dnode dpath)
  ;;(message "Updating path %s" dpath)
  (let ((fattrs (directory-files-and-attributes dpath nil nil t))
        (olddirs (projek--dnode-dirs dnode))
        (newfiles '())
        (newdirs (make-hash-table :test 'equal)))

    (dolist (fattr fattrs)
      (let ((type (cadr fattr)))
        (cond
         ;; file
         ((null type)
          (unless (projek--should-ignore pnode rnode (car fattr))
            (push (concat dpath (car fattr)) newfiles)))
         ;; dir
         ((eq 't type)
          (cond
           ;; this dir
           ((equal (car fattr) ".")
            (setf (projek--dnode-mtime dnode) (nth 6 fattr)))
           ;; parent dir
           ((equal (car fattr) ".."))
           (t
            (let ((dirname (file-name-as-directory (car fattr))))
              (unless (projek--should-ignore pnode rnode dirname)
                (if-let ((oldnode (gethash (car fattr) olddirs)))
                    (puthash (car fattr) oldnode newdirs)
                  (puthash (car fattr)
                           (projek--dnode-create (concat dpath dirname))
                           newdirs)))))))
         ;; symbolic link
         ((stringp type)))))

    (setf (projek--dnode-dirs dnode) newdirs)
    (setf (projek--dnode-files dnode) newfiles)))

(defun projek--clean-project (pnode)
  (projek--foreach-root rnode in pnode
    (projek--foreach-dir dnode in rnode
      (progn
        (setf (projek--dnode-mtime dnode) nil)
        (clrhash (projek--dnode-dirs dnode))
        (setf (projek--dnode-files dnode) nil)))))

(defun projek--index-project (pnode)
  (projek--foreach-root rnode in pnode
    (projek--foreach-dir dnode in rnode
      (let ((dpath (projek--dnode-path dnode)))
        ;; (message "indexing path %s" dpath)
        (thread-yield)
        (when (projek--dnode-changed-p dnode dpath)
          (projek--update-dnode pnode rnode dnode dpath))))))

(defun projek--find-file-regex (pnode regex)
  (let ((matches))
    (projek--foreach-root rnode in pnode
      (let ((rlen (length (projek--dnode-path rnode))))
        (projek--foreach-dir dnode in rnode
          (cl-loop for file in (projek--dnode-files dnode)
                   when (string-match-p regex file rlen)
                   do (push file matches)))))
    matches))

(defun projek--all-files (pnode)
  (let ((files))
    (projek--foreach-root rnode in pnode
      (projek--foreach-dir dnode in rnode
        (progn
          (thread-yield)
          ;;(message "adding file from %s" (projek--dnode-path dnode))
          (setq files (append (projek--dnode-files dnode) files)))))
    files))

(provide 'projek)
