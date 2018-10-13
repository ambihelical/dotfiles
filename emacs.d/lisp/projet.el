;;; -*- lexical-binding: t; -*-

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


(defun projet--create-dnode (path)
  "Create a new directory node.
NAME is the path of the directory relative to parent."
  (list :path path                ;; name of directory relative to parent
        :mtime nil                ;; mtime of directory when last indexed
        :dirs (make-hash-table :test 'equal)   ;; child dirname -> dnode
        :files nil))              ;; list of child files

(defun projet--create-rnode (pnode path)
  "Create a new root dnode.
PATH is the absolute path of the root."
  (let ((iglobs (project-ignores (plist-get pnode :project) path))
        (rnode (projet--create-dnode path)))
    (plist-put rnode :ignore-re (projet--globs-to-regexp iglobs))
    (plist-put rnode :keep-re "$^")))

(defun projet--create-pnode (project)
  "Create a new project
PROJECT is a project.el project identifier
Returns the new project node"
  (let* ((roots (mapcar #'file-name-as-directory (project-roots project)))
         (pnode (list :project project                ;; project.el project identifier
                      :roots nil                      ;; list of root dnodes
                      :last-used (current-time)       ;; when used last (used to prune)
                      :last-saved nil)))                 ;; when last saved
    ;; create root dnode for each project root
    (let ((rnodes (mapcar
                   (lambda (path) (projet--create-rnode pnode path))
                   roots)))
      (plist-put pnode :roots rnodes)
      ;; add project to active projects and limit size of that hash
      (puthash (cdr project) pnode projet--active-projects)
      (projet--prune-active-projects)
      pnode)))

(defun projet--find-rnode (pnode dpath)
  "Find the rnode corresponding to directory path DPATH
  PNODE is the project node
  DPATH is the path of a directory"
  (seq-find (lambda (rnode) (string-prefix-p (plist-get rnode :path) dpath))
            (plist-get pnode :roots)))

(defun projet--find-dnode (pnode rnode dpath)
  "Find dnode associated with directory path DPATH.
PNODE is the project node.
RNODE is the root dnode.
Returns dnode or nil if not found"
  (when-let* ((dnode rnode)
              (rootpath (plist-get rnode :path))
              (relpath (substring dpath (length rootpath))))
    (unless (string-empty-p relpath)
      (let ((dirs (split-string (directory-file-name relpath) "/")))
        (while (and dnode dirs)
          (setq dnode (gethash (car dirs) (plist-get dnode :dirs)))
          (setq dirs (cdr dirs)))))
    dnode))

(defun projet--should-ignore (pnode rnode name)
  (let ((ignore-re (plist-get rnode :ignore-re))
        (keep-re (plist-get rnode :keep-re)))
    (and (string-match ignore-re name)
         (not (string-match keep-re name)))))

(defun projet--traverse-dnode (dnode fun)
  "Traverse a directory.
DNODE directory node to traverse.
FUN function to call on each directory node"
  (funcall fun dnode)
  (maphash (lambda (name dir)
             (projet--traverse-dnode dir fun))
           (plist-get dnode :dirs)))

(defmacro projet--foreach-root (pnode rvar body)
  "Traverse the root directories, setting RVAR to the root dnode of each"
  `(dolist (,rvar (plist-get pnode :roots))
     (progn ,body)))

(defmacro projet--foreach-dir (rnode dvar body)
  `(projet--traverse-dnode rnode
                               (lambda (,dvar) (progn ,body))))

(defun projet--print-tree (pnode)
  (projet--foreach-root
   pnode rnode
   (let* ((rpath (plist-get rnode :path))
          (rdepth (length (split-string rpath "/")))
          (count 0))
     (projet--foreach-dir
      rnode dnode
      (let* ((fcount (length (plist-get dnode :files)))
             (dpath (plist-get dnode :path))
             (depth (- (length (split-string dpath "/")) rdepth))
             (name (file-name-nondirectory (directory-file-name dpath))))
        (setq count (1+ count))
        (message "%s%s -> %d files"
                 (make-string (* depth 2) ?\s)
                 name fcount )))
     (message "#### Total %d ####" count))))

(defun projet--dnode-changed-p (dnode dpath)
  (let ((dattr (file-attributes dpath))
        (dmtime (plist-get dnode :mtime)))
    (and (eq 't (car dattr)) (not (equal dmtime (nth 5 dattr))))))

(defun projet--update-dnode (pnode rnode dnode rpath dpath)
  (message "Updating path %s" dpath)
  (let ((fattrs (directory-files-and-attributes dpath nil nil t))
        (olddirs (plist-get dnode :dirs))
        (newfiles '())
        (newdirs (make-hash-table :test 'equal))
        (relpath (string-remove-prefix rpath dpath)))
    (dolist (fattr fattrs)
      (cond
       ;; this dir
       ((equal (car fattr) ".")
        (plist-put dnode :mtime (nth 6 fattr)))
       ;; parent dir
       ((equal (car fattr) ".."))
       ;; symbolic link
       ((stringp (nth 1 fattr)))
       ;; file
       ((null (nth 1 fattr))
        (unless (projet--should-ignore pnode rnode (car fattr))
          (push (concat relpath (car fattr)) newfiles)))
       ;; directory
       ((eq 't (nth 1 fattr))
        (let ((dirname (file-name-as-directory (car fattr))))
          (unless (projet--should-ignore pnode rnode dirname)
            (if-let ((oldnode (gethash (car fattr) olddirs)))
                (puthash (car fattr) oldnode newdirs)
              (puthash (car fattr)
                       (projet--create-dnode (concat dpath dirname))
                       newdirs)))))
       (t nil)))
    (plist-put dnode :dirs newdirs)
    (plist-put dnode :files newfiles)))

(defun projet--clean-project (pnode)
  (projet--foreach-root
   pnode rnode
   (projet--foreach-dir
    rnode dnode
    (progn
      (plist-put dnode :mtime nil)
      (clrhash (plist-get dnode :dirs))
      (plist-put dnode :files nil)))))

(defun projet--index-project (pnode)
  (projet--foreach-root
   pnode rnode
   (let ((rpath (plist-get rnode :path)))
     (projet--foreach-dir
      rnode dnode
      (let ((dpath (plist-get dnode :path)))
        ;;(message "indexing path %s" dpath)
        (when (projet--dnode-changed-p dnode dpath)
          (projet--update-dnode pnode rnode dnode rpath dpath)))))))

(provide 'projet)
