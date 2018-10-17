;;; -*- lexical-binding: t; -*-

(require 'cl)
(require 'project)

;; declare our devotion to speed at all costs
(cl-declaim (optimize (speed 3) (safety 0)))

(defcustom projective-max-active-projects 10
  "Maximum number of projects to keep in memory."
  :type '(integer)
  :group 'projective)

(defvar projet--active-projects (make-hash-table :test 'equal)
  "Map of key-path to project node")


;; TBD
(defun projet--prune-active-projects () "Remove old projects")

(defun projet--globs-to-regexp (globs)
  (concat "\\(?:"
          (mapconcat #'identity (mapcar #'dired-glob-regexp globs) "\\|" )
          "\\)"))

(cl-defstruct (projet--pnode (:constructor projet--pnode--create)
                             (:copier nil))
  roots project last-used last-saved)

(cl-defstruct (projet--dnode (:constructor projet--dnode--create)
                             (:copier nil))
  path mtime dirs files ignore-re keep-re)

(cl-defun projet--pnode-create (project &rest args)
  (when-let* ((pnode (apply #'projet--pnode--create
                            :project project :last-used (current-time) args))
              (roots (mapcar #'file-name-as-directory (project-roots project)))
              (rnodes (mapcar
                       (lambda (path) (projet--rnode-create pnode path))
                       roots)))
    (setf (projet--pnode-roots pnode) rnodes)
    ;; add project to active projects and prune
    (puthash (cdr project) pnode projet--active-projects)
    (projet--prune-active-projects)
    pnode))

(cl-defun projet--dnode-create (path &rest args)
  (apply #'projet--dnode--create
         :path path
         :dirs (make-hash-table :test 'equal)
         args))

(cl-defun projet--rnode-create (pnode path)
  (let* ((iglobs (project-ignores (projet--pnode-project pnode) path))
         (ignore (projet--globs-to-regexp iglobs)))
    (projet--dnode-create path
                          :ignore-re ignore
                          :keep-re "$^")))

(defun projet--find-rnode (pnode dpath)
  "Find the rnode corresponding to directory path DPATH
  PNODE is the project node
  DPATH is the path of a directory"
  (seq-find (lambda (rnode) (string-prefix-p (projet--dnode-path rnode) dpath))
            (projet--pnode-roots pnode)))

(defun projet--find-dnode (pnode rnode dpath)
  "Find dnode associated with directory path DPATH.
PNODE is the project node.
RNODE is the root dnode.
Returns dnode or nil if not found"
  (when-let* ((dnode rnode)
              (rootpath (projet--dnode-path rnode))
              (relpath (substring dpath (length rootpath))))
    (unless (string-empty-p relpath)
      (let ((dirs (split-string (directory-file-name relpath) "/")))
        (while (and dnode dirs)
          (setq dnode (gethash (car dirs) (projet--dnode-dirs dnode)))
          (setq dirs (cdr dirs)))))
    dnode))

(defun projet--should-ignore (pnode rnode name)
  (and (string-match (projet--dnode-ignore-re rnode) name)
       (not (string-match (projet--dnode-keep-re rnode) name))))

(defun projet--traverse-dnode (dnode fun)
  "Traverse a directory.
DNODE directory node to traverse.
FUN function to call on each directory node"
  (funcall fun dnode)
  (maphash (lambda (name dir)
             (projet--traverse-dnode dir fun))
           (projet--dnode-dirs dnode)))

(defmacro projet--foreach-root (pnode rvar body)
  "Traverse the root directories, setting RVAR to the root dnode of each"
  `(dolist (,rvar (projet--pnode-roots pnode)) ,body))

(defmacro projet--foreach-dir (rnode dvar body)
  `(projet--traverse-dnode rnode (lambda (,dvar) ,body)))

(defun projet--print-tree (pnode)
  (projet--foreach-root
   pnode rnode
   (let* ((rpath (projet--dnode-path rnode))
          (rdepth (length (split-string rpath "/")))
          (dcount 0)
          (tcount 0))
     (projet--foreach-dir
      rnode dnode
      (let* ((fcount (length (projet--dnode-files dnode)))
             (dpath (projet--dnode-path dnode))
             (depth (- (length (split-string dpath "/")) rdepth))
             (name (file-name-nondirectory (directory-file-name dpath))))
        (setq dcount (1+ dcount))
        (setq tcount (+ tcount fcount))
        (message "%s%s -> %d files"
                 (make-string (* depth 2) ?\s)
                 name fcount )))
     (message "#### Dirs %d  Files %d ####" dcount tcount))))

(defun projet--dnode-changed-p (dnode dpath)
  (let ((dattr (file-attributes dpath))
        (dmtime (projet--dnode-mtime dnode)))
    (and (eq 't (car dattr)) (not (equal dmtime (nth 5 dattr))))))

(defun projet--update-dnode (pnode rnode dnode rpath dpath)
  ;;(message "Updating path %s" dpath)
  (let ((fattrs (directory-files-and-attributes dpath nil nil t))
        (olddirs (projet--dnode-dirs dnode))
        (newfiles '())
        (newdirs (make-hash-table :test 'equal)))

    (dolist (fattr fattrs)
      (let ((type (cadr fattr)))
        (cond
         ;; file
         ((null type)
          (unless (projet--should-ignore pnode rnode (car fattr))
            (push (concat dpath (car fattr)) newfiles)))
         ;; dir
         ((eq 't type)
          (cond
           ;; this dir
           ((equal (car fattr) ".")
            (setf (projet--dnode-mtime dnode) (nth 6 fattr)))
           ;; parent dir
           ((equal (car fattr) ".."))
           (t
            (let ((dirname (file-name-as-directory (car fattr))))
              (unless (projet--should-ignore pnode rnode dirname)
                (if-let ((oldnode (gethash (car fattr) olddirs)))
                    (puthash (car fattr) oldnode newdirs)
                  (puthash (car fattr)
                           (projet--dnode-create (concat dpath dirname))
                           newdirs)))))))
         ;; symbolic link
         ((stringp type)))))

    (setf (projet--dnode-dirs dnode) newdirs)
    (setf (projet--dnode-files dnode) newfiles)))

(defun projet--clean-project (pnode)
  (projet--foreach-root
   pnode rnode
   (projet--foreach-dir
    rnode dnode
    (progn
      (setf (projet--dnode-mtime dnode) nil)
      (clrhash (projet--dnode-dirs dnode))
      (setf (projet--dnode-files dnode) nil)))))

(defun projet--index-project (pnode)
  (projet--foreach-root
   pnode rnode
   (let ((rpath (projet--dnode-path rnode)))
     (projet--foreach-dir
      rnode dnode
      (let ((dpath (projet--dnode-path dnode)))
        ;;(message "indexing path %s" dpath)
        (when (projet--dnode-changed-p dnode dpath)
          (projet--update-dnode pnode rnode dnode rpath dpath)))))))

(defun projet--find-file-regex (pnode regex)
  (let ((matches))
    (projet--foreach-root
     pnode rnode
     (let ((rlen (length (projet--dnode-path rnode))))
       (projet--foreach-dir
        rnode dnode
        (cl-loop for file in (projet--dnode-files dnode)
                 when (string-match-p regex file rlen)
                 do (push file matches)))))
    matches))

(provide 'projet)
