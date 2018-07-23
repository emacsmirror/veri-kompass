;;; -*- lexical-binding: t; -*-

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
			       "negedge" "int"))

(defconst vk-sym-regex "[0-9a-z_]+")

(defconst vk-ops-regex "[\]\[ ()|&\+-/%{}=<>]")

(defvar vk-hier
  "Holds the design hierarchy.")

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
		   "\\( *"
		   sym
		   "\\) *\\(\\[.*\\] +\\)?\\(=\\|<=\\)[^=].*") nil t)
	    (push (cons (match-string 0)
			(match-beginning 0)) res))
	  (if res
	      res
	    ;; Otherwise is coming from e submodule. TODO: check input/output!
	    (while (re-search-backward
		    (concat
		     "\\..+( *\\("
		     sym
		     "\\)\\(\\[.*\\] *\\)?)") nil t)
	      (push (cons (match-string 0)
			  (match-beginning 1)) res))
	    res))))))

(defun vk-search-driver-at-point ()
  "Goto the driver for symbol at point"
  (interactive)
  (let ((res (vk-search-driver (car (vk-sym-at-point)))))
    (when res
      (if (equal (length res) 1)
	  (goto-char (cdar res))
	(goto-char (helm :sources (helm-build-sync-source "select driver line"
				    :candidates res)
			 :buffer "*helm-veri-kompass-driver-select*"))))))

(defun vk-search-load-at-point ()
  )

(defun vk-follow-from-point ()
  "Follow symbol at point.
If is an l-val search for loads, if r-val search for drivers."
  (interactive)
  (let ((sym (vk-sym-at-point)))
    (pcase (cdr sym)
      ('l-val (vk-search-load-at-point))
      ('r-val (vk-search-driver-at-point)))))

(defun vk-list-file-in-proj (dir)
  (remove nil
	  (mapcar (lambda (x)
		    (if (or (string-match "/\\." x)
			    (string-match vk-skip-regexp x))
			nil
		      x))
		  (directory-files-recursively dir vk-extention-regexp))))

(defun vk-list-modules-in-file (file)
  (with-temp-buffer
    (insert-file-contents-literally file)
    (let ((mod-list))
      (while (re-search-forward
	      "^ *module +\\([0-9a-z_]+\\) *\n* *\\((\\|#(\\|;\\)" nil t)
	(add-to-list (list
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
	    (re-search-forward "^ *endmodule" nil t)
	    (narrow-to-region (mark) (point))
	    (vk-mark-code-blocks)
	    (goto-char (point-min))
	    (while (re-search-forward
		    "\\([0-9a-z_]+\\) +\\([0-9a-z_]+\\) *\\((\\|#(\\)"  nil t)
	      (unless (or (get-char-property 0 'code (match-string 0))
			  (get-char-property 0 'comment (match-string 0))
			  (char-equal (aref (match-string-no-properties 1) 0)
				      ?\`)
			  (member (match-string-no-properties 1)
				  vk-ignore-keywords)
			  (member (match-string-no-properties 2)
				  vk-ignore-keywords))
		(add-to-list (cons (list (match-string-no-properties 2)
					 (match-string-no-properties 1)
					 (match-beginning 0)
					 (car target)
					 (match-string-no-properties 0))
				   (vk-build-hier-rec
				    (match-string-no-properties 1))) struct)))
	    (puthash mod-name (reverse struct) vk-mod-str-hash))
	(message "cannot find module %s" mod-name)
	nil))))

(defun vk-build-hier (top)
  (let ((target (vk-mod-to-file-name-pos top)))
    (if target
	(list (list top top (cadr target) (car target) (caddr target))
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

(defun vk-orgify-link (l)
  (let ((coords (vk-mod-to-file-name-pos (cadr l))))
    (if coords
	(format "[[%s::%s][%s]] [[%s::%s][%s]]"
		(nth 3 l)
		(nth 4 l)
		(nth 0 l)
		(nth 0 coords)
		(car (split-string (nth 2 coords) "\n"))
		(nth 1 l))
      (car l))))

(defun vk-orgify-hier (hier nest)
  (mapconcat (lambda (h)
	       (if (consp (car h))
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
