;;; veri-kompass.el --- verilog codebase navigation facility for Emacs  -*- lexical-binding:t -*-

;; Copyright (C) 2018 Andrea Corallo

;; Maintainer: andrea_corallo@yahoo.it
;; Package: veri-kompass

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Provide verilog codebase navigation facility for Emacs.
;; Including a hierarchy sidebar and functions to follow drivers and loads
;; within the design.

;;; Code:

(require 'helm)
(require 'org)

(defcustom vk-top ""
  "Default top module name")

(defcustom vk-extention-regexp ".+\\.s?v$"
  "Regexp matching project files")

(defcustom vk-skip-regexp "^.*CONFORMTO.*$"
  "Regexp matching files to be skip")

(defvar vk-module-list nil)

(defvar vk-module-hier nil)

(defvar vk-mod-str-hash nil
  "This hash contains module structure hashed per module name")

(defvar vk-helm-mods nil)

(defconst vk-ignore-keywords '("if" "task" "assert" "disable" "define" "posedge"
			       "negedge" "int" "for" "logic" "wire" "reg"))

(defconst vk-sym-regex "[0-9a-z_]+")

(defconst vk-ops-regex "[\]\[ ()|&\+-/%{}=<>]")

(defvar vk-hier
  "Holds the design hierarchy.")

(cl-defstruct (vk-mod-inst (:copier nil))
  "Holds a module instantiations."
  inst-name mod-name file-name line)

(defun vk-sym-classify-at-point ()
  (save-excursion
    (re-search-forward "[=;]" nil t)
    (pcase (aref (match-string-no-properties 0) 0)
      (?\= 'l-val)
      (?\; 'r-val))))

(defun vk-sym-at-point ()
  "Return an a-list containing (sym-name . 'r-val) or (sym-name . 'l-val)."
  (save-excursion
    (re-search-backward vk-ops-regex nil t)
    (re-search-forward vk-sym-regex nil t)
    (cons (match-string-no-properties 0) (vk-sym-classify-at-point))))

(defun vk-search-driver (sym)
  (save-excursion
    (goto-char (point-min))
    ;; First case is easy, in case is a module input.
    (if (re-search-forward (concat
			    "input +\\(wire +\\)?\\(logic +\\)?\\[*.*\] +\\("
			    sym
			    "\\)") nil t)
	(list (cons (match-string 0) (match-beginning 3)))
      (if (re-search-forward (concat "input +\\(wire +\\)?\\(logic +\\)?\\("
				     sym
				     "\\)") nil t)
	  (list (cons (match-string 0) (match-beginning 3)))
	(goto-char (point-max))
	;; Here we handle direct assignments.
	(let ((res ()))
	  (while (re-search-backward
		  (concat
		   "\\(\\<"
		   sym
		   "\\>\\)[[:space:]]*\\(\\[.*\\] +\\)?\\(=\\|<=\\)[^=].*")
		  nil t)
	    (push (cons (match-string 0)
			(match-beginning 0)) res))
	  (if res
	      res
	    ;; Otherwise is coming from e submodule. TODO: check input/output!
	    (while (re-search-backward
		    (concat
		     "\\..+([[:space:]]*\\("
		     sym
		     "\\)\\(\\[.*\\][[:space:]]*\\)?)") nil t)
	      (push (cons (match-string 0)
			  (match-beginning 1)) res))
	    res))))))

(defun vk-search-driver-at-point ()
  "Goto the driver for symbol at point."
  (interactive)
  (let ((res (vk-search-driver (car (vk-sym-at-point)))))
    (when res
      (if (equal (length res) 1)
	  (goto-char (cdar res))
	(goto-char (helm :sources (helm-build-sync-source "select driver line"
				    :candidates res)
			 :buffer "*helm-veri-kompass-driver-select*"))))))

(defun vk-search-load (sym)
  (save-excursion
    (let ((loads ())
	  (drivers (mapcar #'cdr (vk-search-driver sym))))
      (goto-char (point-max))
      (while (re-search-backward (concat "^.*\\(\\<" sym "\\>\\).*") nil t)
	(unless (member (match-beginning 1) drivers)
	  (push (cons (match-string 0) (match-beginning 1))
		loads)))
      loads)))

(defun vk-search-load-at-point ()
  "Goto the loads for symbol at point."
  (interactive)
  (let ((res (vk-search-load (car (vk-sym-at-point)))))
    (when res
      (if (equal (length res) 1)
	  (goto-char (cdar res))
	(goto-char (helm :sources (helm-build-sync-source "select load line"
				    :candidates res)
			 :buffer "*helm-veri-kompass-load-select*"))))))

(defun vk-follow-from-point ()
  "Follow symbol at point.
If is an l-val search for loads, if r-val search for drivers."
  (interactive)
  (let ((sym (vk-sym-at-point)))
    (pcase (cdr sym)
      ('l-val (vk-search-load-at-point))
      ('r-val (vk-search-driver-at-point)))))


(defun directory-files-recursively-with-symlink (dir regexp &optional include-directories)
  "This function is a variant of directory-files-recursively from files.el.
Return list of all files under DIR that have file names matching REGEXP.
This function works recursively following symlinks.
Files are returned in \"depth first\" order, and files from each directory are
 sorted in alphabetical order.
Each file name appears in the returned list in its absolute form.
Optional argument INCLUDE-DIRECTORIES non-nil means also include in the
output directories whose names match REGEXP."
  (let ((result nil)
	(files nil)
	;; When DIR is "/", remote file names like "/method:" could
	;; also be offered.  We shall suppress them.
	(tramp-mode (and tramp-mode (file-remote-p (expand-file-name dir)))))
    (dolist (file (sort (file-name-all-completions "" dir)
			'string<))
      (unless (member file '("./" "../"))
	(if (directory-name-p file)
	    (let* ((leaf (substring file 0 (1- (length file))))
		   (full-file (expand-file-name leaf dir)))
	      (setq result
		    (nconc result (directory-files-recursively
				   full-file regexp include-directories)))
	      (when (and include-directories
			 (string-match regexp leaf))
		(setq result (nconc result (list full-file)))))
	  (when (string-match regexp file)
	    (push (expand-file-name file dir) files)))))
    (nconc result (nreverse files))))

(defun vk-list-file-in-proj (dir)
  (remove nil
	  (mapcar (lambda (x)
		    (if (or (string-match "/\\." x)
			    (string-match vk-skip-regexp x))
			nil
		      x))
		  (directory-files-recursively-with-symlink
		   dir vk-extention-regexp))))

(defun vk-list-modules-in-file (file)
  (with-temp-buffer
    (insert-file-contents-literally file)
    (let ((mod-list))
      (while (re-search-forward
	      "^[[:space:]]*module[[:space:]]+\\([0-9a-z_]+\\)[[:space:]]*\n*[[:space:]]*\\((\\|#(\\|`\\|;\\)" nil t)
	(push (list
	       (match-string-no-properties 1)
	       file
	       (point)
	       (match-string-no-properties 0)) mod-list))
      mod-list)))

(defun vk-list-modules-in-proj (files)
  (remove nil
	  (cl-mapcan 'vk-list-modules-in-file files)))

(defun vk-mod-to-file-name-pos (name)
  (cdr (assoc name vk-module-list)))

(defun vk-mark-comments ()
  (interactive)
  (save-mark-and-excursion
    (goto-char (point-min))
    (while (re-search-forward "//.*" nil t) ;; TODO add other comment style
      (put-text-property (match-beginning 0) (point) 'comment t))))

(defun vk-mark-code-blocks ()
  "Mark all text within code blocks with property 'code."
  (interactive)
  (save-mark-and-excursion
    (vk-mark-comments)
    (goto-char (point-min))
    (while (search-forward "begin" nil t)
      (unless (get-char-property 0 'comment (match-string 0))
	(backward-word)
	(set-mark (point))
	(forward-word)
	(let ((nest 1))
	  (while (> nest 0)
	    (re-search-forward "\\(begin\\|end$\\|end \\)" nil t)
	    (setq nest (if (and (equal (match-string 1) "begin")
				(not (get-char-property
				      0
				      'comment
				      (match-string 0))))
			   (1+ nest)
			 (1- nest)))))
	(put-text-property (mark) (point) 'code t)))))

(defun vk-forward-balanced ()
  "After an opening parenthesys find the matching closing one."
  (save-match-data
    (let ((x 1))
      (while (and (> x 0)
		  (re-search-forward "\\((\\|)\\)" nil t))
	(if (equal (match-string 0) "(")
	    (setq x (1+ x))
	  (setq x (1- x)))))))

(defun vk-delete-parameters ()
  "Remove all #( ... )."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "#(" nil t)
      (vk-forward-balanced)
      (delete-region (match-beginning 0) (point)))))

(defun vk-remove-macros ()
  "Remove all `SOMETHIING ."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "`[a-z_0-9]+" nil t)
      (delete-region (match-beginning 0) (match-end 0)))))

(defun vk-retrive-original-line (inst-name mod-name file-name)
  "Given instance name module name and file name for the instantiation return
the matching line used to instantiate the module."
  (save-match-data
    (with-temp-buffer
      (insert-file-contents file-name)
      (or (re-search-forward
	   (format
	    "\\<%s\\>[ a-z-0-9_.#(),\n]*\\<%s\\>"
	    inst-name
	    mod-name) nil t)
	  (search-forward inst-name))
      (line-number-at-pos (match-beginning 0)))))

(defun vk-build-hier-rec (mod-name)
  (if (gethash mod-name vk-mod-str-hash) ;; some memoization is gonna help
      (gethash mod-name vk-mod-str-hash)
    (let ((target (vk-mod-to-file-name-pos mod-name))
	  (struct nil))
      (if target
	  (with-temp-buffer
	    (insert-file-contents-literally (car target))
	    (goto-char (cadr target))
	    (set-mark (point))
	    (re-search-forward "^[[:space:]]*endmodule" nil t)
	    (narrow-to-region (mark) (point))
	    (vk-delete-parameters)
	    (vk-remove-macros)
	    (vk-mark-code-blocks)
	    (goto-char (point-min))
	    (while (re-search-forward
		    "\\([0-9a-z_]+\\)[[:space:]]+\\([0-9a-z_]+\\)[[:space:]]*("  nil t)
	      (when (save-match-data
		      (vk-forward-balanced)
		      (looking-at "[[:space:]]*;"))
		(unless (or (get-char-property 0 'code (match-string 0))
			    (get-char-property 0 'comment (match-string 0))
			    (char-equal (aref (match-string-no-properties 1) 0)
					?\`)
			    (member (match-string-no-properties 1)
				    vk-ignore-keywords)
			    (member (match-string-no-properties 2)
				    vk-ignore-keywords))
		  (push (let ((curr-inst
			       (make-vk-mod-inst
				:mod-name (match-string-no-properties 1)
				:inst-name (match-string-no-properties 2)
				:file-name (car target)
				:line (vk-retrive-original-line (match-string 1)
								(match-string 2)
								(car target))))
			      (sub-hier
			       (vk-build-hier-rec
				(match-string-no-properties 1))))
			  (if sub-hier
			      (cons curr-inst
				    (list sub-hier))
			    curr-inst))
			struct))))
	    (puthash mod-name (reverse struct) vk-mod-str-hash))
	(message "cannot find module %s" mod-name)
	nil))))

(defun vk-build-hier (top)
  (let ((target (vk-mod-to-file-name-pos top)))
    (if target
	(list (make-vk-mod-inst
	       :inst-name top
	       :mod-name top
	       :file-name (car target)
	       :line (caddr target))
	      (vk-build-hier-rec top))
      (message "cannot find top module %s" top))))

(defun vk-visit-module-declaration (mod-name)
  (interactive (list
		(read-string (format "Module name (%s): " (thing-at-point 'word))
			     nil nil (thing-at-point 'word))))
  (unless mod-name
    (setq mod-name (thing-at-point 'word)))
  (let ((target (vk-mod-to-file-name-pos mod-name)))
    (if target
	(progn
	  (find-file (car target))
	  (goto-char (cadr target)))
      (message "Can't find module: %s" mod-name))))

(defun vk-orgify-link (inst)
  (let ((coords (vk-mod-to-file-name-pos (vk-mod-inst-mod-name inst))))
    (if coords
	(format "[[%s::%s][%s]] [[%s::%s][%s]]"
		(vk-mod-inst-file-name inst)
		(vk-mod-inst-line inst)
		(vk-mod-inst-inst-name inst)
		(nth 0 coords)
		(with-temp-buffer
		  (insert (nth 2 coords))
		  (re-search-backward "module.*$" nil t)
		  (match-string 0))
		(vk-mod-inst-mod-name inst))
      (vk-mod-inst-inst-name inst))))

(defun vk-orgify-hier (hier nest)
  (mapconcat (lambda (h)
	       (if (consp h)
		   (vk-orgify-hier h (1+ nest))
		 (format "%s %s" (let ((x ""))
				   (dotimes (_ nest)
				     (setq x (concat x "*")))
				   x)
			 (vk-orgify-link h)))) hier "\n"))

(defun veri-kompass (dir)
  (interactive "D")
  (setq vk-mod-str-hash (make-hash-table :test 'equal))
  (setq vk-module-list
	(vk-list-modules-in-proj
	 (vk-list-file-in-proj dir)))
  (setq vk-helm-mods (helm-build-sync-source "specify top module"
		       :candidates (mapcar (lambda (x)
					     (car x)) vk-module-list)))
  (setq vk-hier (vk-build-hier
		 (helm :sources vk-helm-mods
		       :default vk-top
		       :buffer "*helm-veri-kompass-module-top-select*")))
  (switch-to-buffer-other-window "veri-kompass-bar")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "*")
    (insert (vk-orgify-hier vk-hier 0)))
  (read-only-mode)
  (veri-kompass-mode)
  (whitespace-turn-off))

(defun vk-open-at-point (&rest _)
  (interactive)
  (org-open-at-point))


(define-minor-mode veri-kompass-minor-mode
  "Minor mode to be used into verilog files."
  :lighter " VK"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c d") 'vk-search-driver-at-point)
	    (define-key map (kbd "C-c l") 'vk-search-load-at-point)
            map))

(defvar veri-kompassmode-map nil "Keymap for `veri-kompass-mode'")

(progn
  (setq veri-kompass-mode-map (make-sparse-keymap))
  (define-key veri-kompass-mode-map (kbd "RET") 'vk-open-at-point)
  (define-key veri-kompass-mode-map (kbd "S-<right>") 'windmove-right)
  (define-key veri-kompass-mode-map (kbd "S-<left>") 'windmove-left)
  (define-key veri-kompass-mode-map (kbd "S-<up>") 'windmove-up)
  (define-key veri-kompass-mode-map (kbd "S-<down>") 'windmove-down)
  (define-key veri-kompass-mode-map (kbd "C-S-<right>")
    'enlarge-window-horizontally)
  (define-key veri-kompass-mode-map (kbd "C-S-<left>")
    'shrink-window-horizontally)
  )

(define-derived-mode
  veri-kompass-mode
  org-mode
  "Veri-Kompass"
  "Handle verilog project hierarchy.")

(provide 'veri-kompass-mode)

(add-to-list 'auto-mode-alist '("veri-kompass-bar" . veri-kompass-mode))

(add-hook 'verilog-mode-hook 'veri-kompass-minor-mode)
