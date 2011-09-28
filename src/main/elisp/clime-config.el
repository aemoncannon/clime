;;; clime-config.el
;;
;;;; License
;;
;;     Copyright (C) 2010 Aemon Cannon
;;
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.



(defvar clime-config-file-name ".clime"
  "The default file name for clime project configurations.")

(add-to-list 'auto-mode-alist '("\\.clime$" . emacs-lisp-mode))

(defun clime-config-fix-path (f root)
  ;; (clime-config-fix-path "/home/aemon/rabbits.txt" "/home/aemon/")
  ;; (clime-config-fix-path "~/rabbits.txt" "/home/aemon/dogs")
  (let ((rel (clime-relativise-path f root)))
    (if (integerp (string-match "^~" rel))
        (expand-file-name rel)
      rel)))


(defmacro clime-set-key (conf key val)
  `(setq ,conf (plist-put ,conf ,key ,val)))


(defun clime-config-find-file (file-name)
  "Search up the directory tree starting at file-name
   for a suitable config file to load, return it's path. Return nil if
   no such file found."
  (let* ((dir (file-name-directory file-name))
	 (possible-path (concat dir clime-config-file-name)))
    (if (file-directory-p dir)
	(if (file-exists-p possible-path)
	    possible-path
	  (if (not (equal dir (directory-file-name dir)))
	      (clime-config-find-file (directory-file-name dir)))))))

(defun clime-config-find-and-load (&optional default-dir)
  "Query the user for the path to a config file, then load it."
  (let* ((hint (or default-dir buffer-file-name))
	 (guess (if hint (clime-config-find-file hint)))
	 (file (if clime-prefer-noninteractive guess
		 (read-file-name
		  "CLIME Project file: "
		  (if guess (file-name-directory guess))
		  guess
		  nil
		  (if guess (file-name-nondirectory guess))
		  ))))

    ;; Should be ok to just give the project directory..
    (let ((file (if (and (file-directory-p file)
			 (file-exists-p (concat file "/"
						clime-config-file-name)))
		    (concat file "/" clime-config-file-name)
		  file)))

      (if (or (not (file-exists-p file))
	      (file-directory-p file))

	  ;; If doesn't exist, maybe create one on the spot
	  (progn (message (concat "Please see the CLIME manual for"
				  " instructions on how to write or"
				  " generate a config file."))
		   nil)

	;; If does exist, load it.
	(clime-config-load file))

      )))


(defun clime-config-load (file-name)
  "Load and parse a project config file. Return the resulting plist.
   The :root-dir setting will be deduced from the location of the project file."
  (let ((dir (expand-file-name (file-name-directory file-name))))
    (save-excursion
      (let ((config
	     (let ((buf (find-file-read-only file-name clime-config-file-name))
		   (src (buffer-substring-no-properties
			 (point-min) (point-max))))
	       (kill-buffer buf)
	       (condition-case error
		   (read src)
		 (error
		  (error "Error reading configuration file, %s: %s" src error)
		  ))
	       )))
	;; We use the project file's location as the project root.
	(clime-set-key config :root-dir dir)
	config)
      )))



(provide 'clime-config)
