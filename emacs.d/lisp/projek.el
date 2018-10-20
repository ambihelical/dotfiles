;;; -*- lexical-binding: t; -*-

(require 'cl)
(require 'project)

;; declare our devotion to speed at all costs
(cl-declaim (optimize (speed 3) (safety 0)))

(defcustom projek-active-projects 10
  "Maximum number of projects to keep in memory."
  :type '(integer)
  :group 'projective)

(defvar projek--active-projects (make-hash-table :test 'equal)
  "Map of key-path to project node")


;; TBD
(defun projek--prune-active-projects () "Remove old projects")

(defun projek--globs-to-regexp (globs)
  (concat "\\(?:"
          (mapconcat #'identity (mapcar #'dired-glob-regexp globs) "\\|" )
          "\\)"))

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
              (rnodes (mapcar
                       (lambda (path) (projek--rnode-create pnode path))
                       roots)))
    (setf (projek--pnode-roots pnode) rnodes)
    ;; add project to active projects and prune
    (puthash (cdr project) pnode projek--active-projects)
    (projek--prune-active-projects)
    pnode))

(cl-defun projek--dnode-create (path &rest args)
  (apply #'projek--dnode--create
         :path path
         :dirs (make-hash-table :test 'equal)
         args))

(cl-defun projek--rnode-create (pnode path)
  (let* ((iglobs (project-ignores (projek--pnode-project pnode) path))
         (ignore (projek--globs-to-regexp iglobs)))
    (projek--dnode-create path
                          :ignore-re ignore
                          :keep-re "$^")))

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
          ;;(message "adding file from %s" (projek--dnode-path dnode))
          (setq files (append (projek--dnode-files dnode) files)))))
    files))

(provide 'projek)
