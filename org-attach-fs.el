;;; org-attach-fs.el --- Mirror org heading heirarchy to store attachments

;; Version: 0.0
;; Author: Ihor Radchenko <yantar92@gmail.com>
;; Created: 14 March 2020

;;; Commentary:

;; This package aims to store symlinks to org attachments under folder
;; structure reflecting current org heading hierarchy.
;; The package started as my personal Emacs config and assumes that
;; all the attachments can be accessed from any org file. This
;; corresponds to the following config:
;; (setq org-attach-method 'mv)
;; (setq org-attach-id-dir "~/.data/")
;; (setq org-id-locations-file
;;       (f-join org-attach-id-dir ".org-id-locations"))

(require 'f)
(require 'org-attach)

;; (setq org-attach-file-list-property nil)

(defvar-local yant/org-attach-file-symlink-path nil
  "Path to directory where the symlink hierarchy is created for the current org buffer.
It is intended to be set as a file-local variable.
Use `default-directory' if nil.")
(put 'yant/org-attach-file-symlink-path 'safe-local-variable 'stringp)

(defvar yant/org-attach-attachments-symlink-directory "_data"
  "Name of the symlink to the attach file folder.")
(defvar yant/org-attach-symlinks-directory ".org.symlinks"
  "Name of the folder containing symlinks to the entry children attach folders.")

(define-advice org-attach-file-list (:filter-return (filelist) remove-boring-files)
  "Remove local variable file and boring symlinks from the attachment file list."
  (let ((symlinks-directory yant/org-attach-symlinks-directory))
    (remove "flycheck_.dir-locals.el" ;; not sure where this constant is defined
	    (remove dir-locals-file
		    (remove symlinks-directory
			    filelist)))))

(defun yant/outline-get-next-sibling (&optional subtree-end)
  "A faster version of `outline-get-next-sibling'.
Bound search by SUBTREE-END if non nil."
  (let* ((level (funcall outline-level))
	 (sibling-regex (concat "^\\*\\{" (format "%d" level) "\\}[^*]"))
         (bound (or subtree-end (point-max))))
    (re-search-forward sibling-regex bound 'noerror)))

(defun yant/org-entry-name-cleanup-for-dir ()
  "Format entry name to make a directory. Return nil if the entry name is empty."
  (org-with-wide-buffer
   (let* ((entry-name (replace-regexp-in-string "[/<>|:&/]" "-" ;; make sure that entry title can be used as a directory name
						(org-get-heading 'NO-TAGS 'NO-TODO 'NO-PRIORITY 'NO-COMMENT)))
          (entry-name (replace-regexp-in-string " +\\[.+\\]$" "" ;; remove statistics cookies
						entry-name
						))
          (entry-name (replace-regexp-in-string org-link-bracket-re "\\2" ;; only leave the link names
						entry-name
						)))
     (unless (seq-empty-p entry-name) ;; prevent empty folders
       (set-text-properties 0 (length entry-name) nil entry-name)
       entry-name))))

(defun yant/org-subtree-has-attachments-p ()
  "Return non nil if the subtree at point has an attached file."
  (org-with-wide-buffer
   (when (eq major-mode 'org-mode) (org-back-to-heading))
   (let ((subtree-end (save-excursion (org-end-of-subtree))))
     (re-search-forward (format "^\\*+ +.*?[ 	]+.*?:%s:.*?$" org-attach-auto-tag) subtree-end 'noerror))))

(defun yant/org-task-has-attachments-p ()
  "Return non nil if the task at point has an attached file."
  (org-with-wide-buffer
   (when (eq major-mode 'org-mode) (org-back-to-heading))
   (or (member org-attach-auto-tag (org-get-tags nil t))
       (let ((dir (let ((org-attach-dir-suppress-extra-checks t)) (org-attach-dir))))
	 (and dir
	      (org-attach-file-list dir))))))

(defvar yant/--processed-entry-ids nil
  "Variable used to store processed entry ids in `org-attach-dir@yant/org-attach-ensure-attach-dir-symlink'")

(define-advice org-attach-dir (:filter-return (dir) yant/org-attach-ensure-attach-dir-symlink)
  "Make sure that the attach DIR for the current entry has a link in the org structure based directory structure.
The DIR is ensured to be in the symlink mirror dir structure for the entry.
Do nothing if `org-attach-dir-suppress-extra-checks' is non-nil."
  (prog1
      (and dir
	   (f-slash dir))

    (when (and (equal major-mode 'org-mode)
	       dir
	       (not (bound-and-true-p org-attach-dir-suppress-extra-checks)) ;; an option to make `org-attach-dir' faster if needed
	       (f-exists-p dir)
	       (f-dir-p dir))
      (let* ((attach-path dir)
	     (symlinks-directory (f-slash (f-join dir
						  yant/org-attach-symlinks-directory)))
	     (attachments-symlink-directory (f-slash (f-join symlinks-directory
							     yant/org-attach-attachments-symlink-directory)))
	     (org-id (org-id-get nil 'create))
	     (entry-name (yant/org-entry-name-cleanup-for-dir))
	     (attach-dir-inherited-p (or (not (org-entry-get (point) "ID"))
					 (and  (org-entry-get-with-inheritance "DIR")
					       (not (org-entry-get (point) "DIR")))
					 (and (org-entry-get-with-inheritance "ATTACH_DIR_INHERIT")
					      (not (org-entry-get (point) "ATTACH_DIR_INHERIT" nil))))) ;; only consider if the entry is the child
	     (org-attach-dir-recursive-p (bound-and-true-p org-attach-dir-recursive-p))) ;; keep track if this is the initial call of the function
	(unless org-attach-dir-recursive-p (setq yant/--processed-entry-ids nil))
	(unless (member org-id yant/--processed-entry-ids)
	  (add-to-list 'yant/--processed-entry-ids org-id)
	  (unless attach-dir-inherited-p
	    (when (f-file-p symlinks-directory)
	      (error (format "File exist in place of dir: %s" symlinks-directory)))
	    (when (and (f-exists-p attachments-symlink-directory)
		       (not (f-symlink-p (directory-file-name attachments-symlink-directory))))
	      (error (format "Not a symlink: %s" attachments-symlink-directory)))

	    ;; update dirs
	    (unless (f-exists-p symlinks-directory)
	      (f-mkdir symlinks-directory))
	    (unless (or (f-exists-p attachments-symlink-directory)
			(not (yant/org-task-has-attachments-p)))
              ;;(debug)
	      (f-symlink attach-path (directory-file-name attachments-symlink-directory)))
	    (when (and (f-exists-p attachments-symlink-directory)
		       (not (yant/org-task-has-attachments-p)))
	      (f-delete (directory-file-name attachments-symlink-directory)))

	    ;; add to parent entry attachment dir
	    (unless (seq-empty-p entry-name) ;; prevent empty folder names
	      (org-with-wide-buffer
	       (let ((entry-symlink-name (if (org-up-heading-safe)
					     (directory-file-name (f-join (let ((org-attach-dir-recursive-p t))
									    (org-attach-dir 'CREATE))
									  yant/org-attach-symlinks-directory
									  entry-name))
					   (or yant/org-attach-file-symlink-path (hack-local-variables))
                                           (when yant/org-attach-file-symlink-path
                                             (unless (file-exists-p yant/org-attach-file-symlink-path) (f-mkdir yant/org-attach-file-symlink-path)))
					   (directory-file-name (f-join (or yant/org-attach-file-symlink-path
									    default-directory)
									entry-name)))))
		 (if (not (f-exists-p entry-symlink-name))
                     (progn
                       ;;(debug)
		       (f-symlink symlinks-directory (directory-file-name entry-symlink-name)))
		   (unless (f-symlink-p entry-symlink-name)
		     (error (format "File exists: %s" entry-symlink-name)))))))

	    ;; check children
            (when (yant/org-subtree-has-attachments-p)
	      (let ((dirs (delete (directory-file-name attachments-symlink-directory)
				  (f-directories symlinks-directory))))
		(org-with-wide-buffer
		 (org-back-to-heading)
		 (let ((subtree-end (save-excursion (org-end-of-subtree))))
		   (forward-line 1)
		   (when (re-search-forward org-heading-regexp subtree-end t)
		     (while (< (point) subtree-end)
		       (when (yant/org-entry-name-cleanup-for-dir)
			 (let ((child-dir (f-join symlinks-directory (yant/org-entry-name-cleanup-for-dir))))
			   (when (yant/org-subtree-has-attachments-p)
			     (unless (member child-dir dirs)
                               (let ((org-attach-dir-recursive-p t))
				 (org-attach-dir 'CREATE)))
			     (setq dirs (delete child-dir dirs)))))
		       (or (yant/outline-get-next-sibling subtree-end)
			   (goto-char subtree-end))))))
		(mapc (lambda (d)
			(let ((dir (f-long d)))
			  (when (f-symlink-p (directory-file-name dir))
			    (f-delete dir) ; delete the dirs, which do not point to children
			    )))
		      dirs)))))))))

(advice-remove 'org-attach-dir #'org-attach-dir@yant/org-attach-ensure-attach-dir-symlink)

(defun org-attach-dir-symlink (&optional create-if-not-exists-p no-fs-check no-data-dir)
  "Return symlink based path to the attach dir of current entry.
Do not append symlink to data directory if NO-DATA-dir is not nil."
  (org-with-point-at-org-buffer
   (when create-if-not-exists-p
     (let ((symlink (org-attach-dir-symlink nil nil no-data-dir)))
       (when (not (f-exists-p symlink))
	 (org-attach-dir 't))
       symlink))
   (let* ((entry-name (yant/org-entry-name-cleanup-for-dir))
	  (attach-dir-inherited-p (and (org-entry-get-with-inheritance "ATTACH_DIR_INHERIT")
				       (not (org-entry-get (point) "ATTACH_DIR_INHERIT" nil))));; only consider if the entry is the child
          (entry-path (and entry-name
			   (f-join entry-name (if no-data-dir "" yant/org-attach-attachments-symlink-directory)))))
     (if attach-dir-inherited-p
	 (org-with-wide-buffer
          (org-up-heading-safe) ;; if this is false, something went really wrong
	  (org-attach-dir-symlink create-if-not-exists-p nil no-data-dir))
       (unless (seq-empty-p entry-name) ;; prevent empty folders
	 (org-with-wide-buffer
	  (if (org-up-heading-safe)
	      (let ((head-path (org-attach-dir-symlink create-if-not-exists-p nil 't)))
		(when head-path (f-slash (f-join head-path entry-path))))
	    (f-slash (f-join (or yant/org-attach-file-symlink-path
				 default-directory)
			     entry-path)))))))))

(define-advice org-attach-reveal (:around (OLDFUN) reveal-symlink)
  "Go to symlink attach dir structure instead of an actual attach dir."
  (let ((dir (org-attach-dir-get-create))
	(attach-dir-inherited-p (and (org-entry-get-with-inheritance "ATTACH_DIR_INHERIT")
				     (not (org-entry-get (point) "ATTACH_DIR_INHERIT" nil))));; only consider if the entry is the child
	)
    ;; (org-attach-dir@yant/org-attach-ensure-attach-dir-symlink dir)
    (org-attach-sync)
    ;; (cl-letf (((symbol-function 'org-attach-dir-get-create) (if (yant/org-task-has-attachments-p)
    ;; 								(lambda (&rest args) (org-attach-dir-symlink 't nil nil))
    ;; 							      (lambda (&rest args)
    ;; 								(if (yant/org-subtree-has-attachments-p)
    ;; 								    (org-attach-dir-symlink 't nil 't)
    ;; 								  dir
    ;; 								  )))))
    ;;   (when attach-dir-inherited-p (org-attach-tag 'off))
    ;;   (funcall OLDFUN))
    (when attach-dir-inherited-p (org-attach-tag 'off))
    (funcall OLDFUN)
    ))
(advice-remove 'org-attach-reveal #'org-attach-reveal@reveal-symlink)
;; (advice-add 'org-attach-reveal-in-emacs :around #'org-attach-reveal@reveal-symlink)

(provide 'org-attach-fs)

;;; org-attach-fs.el ends here

(add-hook 'after-init-hook (lambda ()
    (advice-add 'find-file-noselect :around #'dired-find-file@disable-abbreviate-file-name)))
