;;; mythryl-mode.el --- Major mode and support code for Mythryl
 
;; Copyright (C) 2009 Phil Rand <philrand@gmail.com>
;; Copyright (C) 2010, 2011, 2012 Michele Bini <michele.bini@gmail.com> aka Rev22

;; Version: 2.5.38
;; Maintainer: Michele Bini <michele.bini@gmail.com>

;; mythryl.el is not part of Emacs
 
;; This is is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
 
;;; Commentary:
 
;; A major mode for editing the Mythryl programming language.
;;
;; Mythryl is a safe, functional programming language, derived by
;; Cynbe ru Taren from SML/NJ, but featuring a clear, more
;; programmer-friendly syntax.  See http://mythryl.org for more.
;;
;; To use this mode, install this file in your elisp load path, then
;; insert the expression:
;;
;;   (load "mythryl-mode")
;;
;; in your .emacs file.

;; Other useful lines for your .emacs (to automatically activate
;; mythryl-mode)

;; ;; for .pkg and .api files
;; (setq auto-mode-alist
;;       (append '(("\\.pkg$" . mythryl-mode)
;;                 ("\\.api$" . mythryl-mode))
;;               auto-mode-alist))

;; ;; for scripts starting with #!/.../mythryl
;; (setq interpreter-mode-alist
;;       (append '(("mythryl" . mythryl-mode))
;;               interpreter-mode-alist))

;;; History:

;; Mythryl-mode was derived by Phil Rand from Stefan Monnier's
;; sml-mode.

;; Added comment-start-skip, makes 'uncomment-region'
;; possible.				--Rev22, 2010-02-18
;; Added run-mythryl.			--Rev22, 2010-02-21
;; Added configurable indent levels.	--Rev22, 2010-02-21

;; After the tragic loss of the original author and maintainer Phil
;; Rand in 2011 I decided to enlist as a maintainer.
;;					--Rev22, 2011-11-06

;; v2.2 Fixed electric keys support, improved indentation engine,
;; XEmacs support (tested on version 21.4).
;;					--Rev22, 2011-11-28

;; v2.4.7 Improved indentation within and after C-style block
;; comments.				--Rev22, 2011-12-06

;; v2.4.20 Add support for mythryld error messages, in
;; compilation-mode.			--Rev22, 2011-12-15

;; v2.5 Various fixes to the indentation engine.

;; v2.5.2 Added outline support

;;; Repositories:

;; You can find new versions of mythryl mode at the following locations:

;; http://github.com/rev22/mythryl-mode
;; EmacsWiki: http://www.emacswiki.org/emacs/MythrylMode

;;; Code:

;; This version of mythryl mode is derived from Stefan Monnier's
;; sml-mode. See http://www.iro.umontreal.ca/~monnier/elisp/, but
;; as of August 2009, the instructions on that page for accessing
;; the svn repository were incorrect.

;;; TODO

;; + fontification of the overloadable string and backticks operators
;; + indent records differently from braced statements
;; + mythryl-interaction-mode
;; + more indentation styles

(require 'custom)
(require 'font-lock)
(require 'derived)

;;;###autoload
(defgroup mythryl () "Group for customizing mythryl-mode"
  :prefix "mythryl-" :group 'languages)

(defvar mythryl-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\# "< b" st)
    ;; (modify-syntax-entry ?\! ". 2b" st)
    ;; (modify-syntax-entry ?\  "- 2b" st)
    ;; (modify-syntax-entry ?\t "- 2b" st)
    (modify-syntax-entry ?\n "> b" st)
    (modify-syntax-entry ?\/ ". 14" st)
    (modify-syntax-entry ?\* ". 23" st)
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\[ "(]" st)
    ;; (modify-syntax-entry "$:=&@^-.%+?>~" "." st)
    (modify-syntax-entry ?\' "w" st)
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\\ "\\" st)
    st)
  "Syntax table in use in mythryl-mode buffers.")

;; http://mythryl.org/my-Non-alphabetic_identifiers.html
(defvar mythryl-mode-op-face 'mythryl-mode-op-face)
(defface  mythryl-mode-op-face
  '((((class color) (background light)) (:foreground "blue"))
    (((class color) (background dark))  (:foreground "yellow"))
    (t                                  ()))
  "Face used for non-alphabetic identifiers in mythryl"
  :group 'mythryl)

(defvar mythryl-mode-pkg-face 'mythryl-mode-pkg-face)
(defface  mythryl-mode-pkg-face
  '((((class color) (background light)) (:foreground "#0af"))
    (((class color) (background dark))  (:foreground "#f50"))
    (t                                  ()))
  "Face used for package identifiers in mythryl

Example: pkg1::pkg2::val."
  :group 'mythryl)

(defvar mythryl-mode-structure-face 'mythryl-mode-structure-face)
(defface mythryl-mode-structure-face
  '((((class color) (background light)) (:foreground "black"))
    (((class color) (background dark))  (:foreground "white"))
    (t                                  ()))
  "Face used for structure characters in mythryl."
  :group 'mythryl)

(defvar mythryl-mode-type-variable-face 'mythryl-mode-type-variable-face)
(defface mythryl-mode-type-variable-face
  '(;;(((class color) (background light)) (:foreground "azure" :weight bold))
    ;;(((class color) (background dark))  (:foreground "aquamarine" :weight bold))
    (t                                  (:weight bold :inherit font-lock-type-face)))
  "Face used for type variables in mythryl."
  :group 'mythryl)

(defvar mythryl-mode-underscore-face 'mythryl-mode-underscore-face)
(defface  mythryl-mode-underscore-face
  '(;;(((class color) (background light)) (:foreground "black"))
    ;;(((class color) (background dark))  (:foreground "white"))
    (t                                  (:weight bold :inherit font-lock-constant-face)))
  "Face used for the underscore wildcard.
This is a bold character by default."
  :group 'mythryl)

(defvar mythryl-line-comment-regexp "#\\($\\|[\t #!]\\).*")
(defvar mythryl-block-comment-regexp
  "/[*]\\([^*]+\\|[*]+[^/*]+\\)*\\($\\|[*]+/\\)")

(defvar mythryl-comment-regexp
  (concat "\\(" mythryl-line-comment-regexp
	  "\\|" mythryl-block-comment-regexp
	  "\\)"))

(defvar mythryl-character-constant-regexp
  "\\<\\('\\)\\(\\\\.\\|[^']\\)\\('\\)"
  "Regexp matching character constants.")

(defvar mythryl-perl-match-regexps
  (list "[.]\\(/\\)\\(\\\\[^.]\\|\\\\.\\|[^/\\]\\)*\\(/\\)"
	"[.]\\(|\\)\\(\\\\[^.]\\|\\\\.\\|[^|\\]\\)*\\(|\\)")
  "Regexps matching ./.../ or .|...| syntaxes.")

(defvar mythryl-string-regexp
  (concat
   "\\(\"\\(\\\\[^.]\\|\\\\.\\|[^\"\\]\\)*\"\\|"
   mythryl-character-constant-regexp
   "\\)"))

(defvar mythryl-comment-or-string-regexp
  (concat "\\(" mythryl-comment-regexp
	  "\\|" mythryl-string-regexp
	  "\\)"))

(defvar mythryl-op-regexp "[\\!%&$+/:<=>?@~|*^-]+")

(defvar mythryl-record-name-regexp "[a-z][a-z0-9_']*"
  "Regexp matching Mythryl record names.")

(defvar mythryl-word-regexp "[A-Za-z0-9_']+"
  "A regexp matching every kind of mythryl 'word'.

It matches numbers identifiers, package names, operators, types, apis, type
constructors, pattern identifiers.")

(defvar mythryl-code-line-regexp "^[ \t]*\\([^#/* \t\n]\\|#[^# \t\n]\\|/[^*\n]\\)")

(defvar mythryl-mode-hook nil
  "*Run upon entering `mythryl-mode'.
This is a good place to put your preferred key bindings.")

;;; Indentation support code

(defun mythryl-skip-closing ()
  (and (looking-at
	(eval-when-compile
	  (concat
	   "\\(\\([]}); ]\\|"
	   (regexp-opt (mapcar 'symbol-name '(end fi esac)) 'words)
	   "\\) *\\)+")))
       (goto-char (match-end 0))))

(defun mythryl-skip-closing-2 ()
  (and (looking-at
	(eval-when-compile
	  (concat
	   "\\(\\("
	   "[ \t]*=>\\|"
	   "[a-z][a-z0-9_']*" ;; mythryl-record-name-regexp
	   "[ \t]*\\(=>\\|:\\($\\|\\<\\|[ \t]+\\)\\)\\|"
	   "[]}); ]\\|"
	   (regexp-opt (mapcar 'symbol-name '(end fi esac herein elif also else where)) 'words)
	   "\\) *\\)+")))
       (goto-char (match-end 0))))

(defgroup mythryl-indent () "Customizing indentation support for mythryl-mode"
  :group 'mythryl)

(defcustom mythryl-if-indent-level 5
  "Indentation level for if blocks."
  :group 'mythryl-indent :type 'integer)

(defcustom mythryl-case-indent-level 5
  "Indentation level for case blocks."
  :group 'mythryl-indent :type 'integer)

(defcustom mythryl-brace-indent-level 4
  "Indentation level for braced blocks."
  :group 'mythryl-indent :type 'integer)

(defcustom mythryl-paren-indent-level 2
  "Indentation level for open parenthesis"
  :group 'mythryl-indent :type 'integer)

(defcustom mythryl-block-indent-level 4
  "Indentation level for other blocks.

This includes \"fun..end\", \"where..end\",
\"except..end\", \"stipulate..herein..end\""
  :group 'mythryl-indent :type 'integer)

(defcustom mythryl-continued-line-indent-level 4
  "Indentation level for continued lines of statements.

This includes \"fun..end\", \"where..end\",
\"except..end\", \"stipulate..herein..end\""
  :group 'mythryl-indent :type 'integer)

(defcustom mythryl-continued-line-indent-braced-blocks t
  "Whether to additionally indent braced blocks in continued lines"
  :group 'mythryl-indent :type 'bool)

(defun mythryl-skip-whitespace ()
  (interactive)
  (save-match-data
    (while (or (looking-at "[ \t\n]+")
	       (looking-at mythryl-comment-regexp))
      (goto-char (match-end 0)))))

(defun mythryl-skip-tail-expressions ()
  (while
      (cond
       ((or (looking-at "[.][A-Za-z0-9_]+")
	    (looking-at mythryl-op-regexp)) 
	(goto-char (match-end 0))
	t)
       ((save-excursion
	  (mythryl-skip-whitespace)
	  (or
	   (looking-at "\\<where\\>")
	   (looking-at "\\<except\\>")))
	(mythryl-forward-expression)))))

(defun mythryl-forward-expression (&optional after-prefix)
  (interactive)
  (save-match-data
    (unless after-prefix
      (mythryl-skip-whitespace))
    (let (endrgx)
      (cond
       ((and after-prefix (looking-at "[ \t\n]")) nil)
       ((looking-at "\\<\\(fu?n\\|except\\)\\>")
	(goto-char (match-end 0))
	(mythryl-skip-whitespace)
	(let (ok pattern)
	  (while
	      (cond
	       ((and endrgx (looking-at endrgx))
		(goto-char (match-end 0))
		(setq ok t)
		nil)
	       ((and (not endrgx) (looking-at "\\([);]\\|\\<also\\>\\)"))
		(setq ok t)
		nil)
	       ((looking-at "=>")
		(goto-char (match-end 0))
		(setq endrgx "\\<end\\>")
		t)
	       ((and
		 (mythryl-forward-expression)
		 (progn (mythryl-skip-whitespace) t)))))
	  (when ok (mythryl-skip-tail-expressions))
	  ok))
       ((cond
	 ((looking-at "\\<case\\>")		(setq endrgx "\\<esac\\>") t)
	 ((looking-at "\\<if\\>")		(setq endrgx "\\<fi\\>")   t)
	 ((looking-at "\\<where\\>")		(setq endrgx "\\<end\\>")  t)
	 ((looking-at "\\<stipulate\\>")	(setq endrgx "\\<end\\>")  t)
	 ((looking-at "[[({]")			(setq endrgx "[])}]")  t))
	(goto-char (match-end 0))
	(mythryl-skip-whitespace)
	(let (ok)
	  (while
	      (cond
	       ((looking-at endrgx)
		(goto-char (match-end 0))
		(setq ok t)
		nil)
	       ((and
		 (mythryl-forward-expression)
		 (progn (mythryl-skip-whitespace) t)))))
	  (when ok (mythryl-skip-tail-expressions))
	  ok))
       ((and
	 (not after-prefix)
	 (looking-at mythryl-op-regexp)
	 (goto-char (match-end 0))
	 (let ((p
		(save-excursion
		  (and (mythryl-forward-expression t)
		       (point)))))
	   (when p
	     (goto-char p)
	     	(mythryl-skip-tail-expressions))
	   t)))
       ((or
	 (looking-at mythryl-word-regexp)
	 (looking-at mythryl-string-regexp)
	 (looking-at mythryl-character-constant-regexp)
	 (looking-at (car mythryl-perl-match-regexps))
	 (looking-at (cadr mythryl-perl-match-regexps))
	 )
	(goto-char (match-end 0))
	(mythryl-skip-tail-expressions)
	t)
       ((< (point) (point-max))
	(goto-char (+ (point) 1))
	(not after-prefix))))))

(defun mythryl-forward-statement ()
  (interactive)
  (mythryl-skip-whitespace)
  (let (ok)
    (while
	(cond
	 ((or (looking-at ";")
	      (looking-at "\\<also\\>"))
	  (goto-char (match-end 0))
	  (mythryl-skip-whitespace)
	  (setq ok t) 
	  nil)
	 ((or (looking-at "[]})]")
	      (looking-at "\\<\\(elif\\|else\\|herein\\|end\\|esac\\|fi\\)\\>")
	      )
	      nil)
	 ((and
	   (mythryl-forward-expression)
	   (progn
	     (mythryl-skip-whitespace)
	     t)))))
    ok))

(defun mythryl-end-of-next-expression (endrgx) ;; Return end of next mythryl expression
  (save-excursion
    (goto-char (match-end 0))
    (mythryl-skip-whitespace)
    (if (looking-at endrgx) (point)
      (and (mythryl-forward-expression) (point)))))

(defun mythryl-indent-comment-line ()
  (save-excursion
    (and
     (forward-to-indentation 0)
     (cond
      ((looking-at "/[*]") t) ;; indent freely first line of block comment
      ((looking-at "*") ;; align to previous line
       (let ((c (current-indentation)))
	   (let ((i
		  (save-excursion
		    ;; Go back until we find a non-empty line
		    (and
		     (re-search-backward
		      "^[ \t]*\\([^ \t]+\\)"
		      nil t)
		     ;; If it is a comment, align to it
		     (progn
		       (goto-char (match-beginning 1))
		       (cond
			((looking-at "[*]") (current-indentation))
			((looking-at "\\(.*/\\)[*]")
			 (+ (- (match-end 1) (match-beginning 1))
			    (current-indentation)))
			(t nil)
			))))))
	     (and i
		  (or (= c i)
		      (progn
			(kill-region
			 (point)
			 (save-excursion
			   (beginning-of-line)
			   (point)))
			(indent-to i)
			t))))))
      ((looking-at "#")
       (if (looking-at "##")
	   (progn
	     (kill-region
	      (point)
	      (save-excursion
		(beginning-of-line)
		(point)))
	     t)
	 (let ((c (current-indentation)))
	   (let ((i
		  (save-excursion
		    ;; Go back until we find a non-empty line
		    (and
		     (re-search-backward
		      "^[ \t]*\\([^/ \t]\\|/[^*]\\)"
		      nil t)
		     ;; If it is a comment, align to it
		     (progn
		       (goto-char (match-beginning 1))
		       (looking-at "#"))
		     (current-indentation)))))
	     (and i
		  (or (= c i)
		      (progn
			(kill-region
			 (point)
			 (save-excursion
			   (beginning-of-line)
			   (point)))
			(indent-to i)
			t)))))))
      (t nil)))))

;; Operations for tags

(defun mythryl-indent--add-tags (sct &rest tags)
  (while tags
    (or (memq (car tags) (car sct))
	(setcar sct (cons (car tags) (car sct))))
    (setq tags (cdr tags))))

(defun mythryl-indent--del-tags (sct &rest tags)
  (while tags
    (and (memq (car tags) (car sct))
	(setcar sct (delq (car tags) (car sct))))
    (setq tags (cdr tags))))

(defun mythryl-indent--has-tag (sct tag)
  (memq tag (car sct)))

(defun mythryl-indent--set-tag (sct tag v)
  (if v (mythryl-indent--add-tags sct tag)
    (mythryl-indent--del-tags sct tag)))

(defun mythryl-indent--any-tags (sct &rest tags)
  (setq sct (car sct))
  (let (found)
    (while (and sct (not found))
      (setq found (memq (car sct) tags))
      (setq sct (cdr sct)))
    found))

(defun mythryl-indent--search-backward-outside-block-comment (rgx)
  (let ((initial (point))
	(front (point))
	done
	result) ;; match a code line
    (while (not done)
      (setq result (re-search-backward rgx nil t))
      ;; (y-or-n-p "code line")
      (if
	  (and
	   result
	   (save-excursion
	     ;; keep looking if we were inside a block comment
	     (and (re-search-forward "\\([/][*]\\|[*][/]\\)" front t)
		  (progn
		    (goto-char (match-beginning 0))
		    ;; (y-or-n-p "block comment marker")
		    (looking-at "[*][/]")))))
	  (if (re-search-backward "[/][*]" nil t)
	      (setq front (point))
	    (setq result nil)
	    (setq done t))
	(setq done t)))
    (unless result (goto-char initial))
    result))

(defun mythryl-indent--back-to-anchor ()
  (interactive)
  (forward-to-indentation 0)
  (let ((front (point)))
    (end-of-line 0)
    (if (= mythryl-continued-line-indent-level 0)
	(if (re-search-backward
	     (eval-when-compile
	       (concat
		"^[ \t]*\\(=\\|"
		"#\\($\\|[\t #!]\\).*" ;; mythryl-line-comment-regexp
		"\\|\\<where\\>\\)"
		))  ;; 'where' may be after a package or a code block
	     nil t)
	    (goto-char (match-beginning 1))
	  (goto-char (point-min)))
      (if (mythryl-indent--search-backward-outside-block-comment
	   mythryl-code-line-regexp)
	  (progn
	    (setq front (match-beginning 1))
	    ;; (y-or-n-p "initial position")
	    (mythryl-skip-whitespace)
	    (beginning-of-line 1)
	    ;; (y-or-n-p "corrected code line")
	    (if (mythryl-indent--search-backward-outside-block-comment
		 (if mythryl-continued-line-indent-braced-blocks
		     "^[^#/*\n]*\\(;\\|\\<also\\>\\)[ \t]*$"
		   "^[^#/*\n]*\\([;{]\\|\\<also\\>\\)[ \t]*$"))
		(progn
		  (goto-char (match-end 0))
		  (mythryl-skip-whitespace) ;; skip over any block comment
		  (beginning-of-line 1)
		  ;; (y-or-n-p "before a statement")
		  (if (re-search-forward
		       mythryl-code-line-regexp
		       (+ front 1) t)
		      (progn
			;; (y-or-n-p "final code line")
			(goto-char (match-beginning 1)))
		    (beginning-of-line 2)
		    (forward-to-indentation 0)))
	      (goto-char (point-min))
	      (forward-to-indentation 0)))
	(goto-char (point-min))
	(forward-to-indentation 0))))
  (mythryl-skip-closing))

(defun mythryl-line-has-opening-brace ()
  (save-excursion
    (save-restriction
      (narrow-to-region (point) (line-end-position))
      (let (ok)
	(while
	    (and (not (setq ok (looking-at "[ \t]*{")))
		 (mythryl-forward-expression)))
	ok))))

;; See also:
;; http://mythryl.org/my-Indentation.html
;; http://mythryl.org/my-If_statements.html
(defun mythryl-indent-line ()
  (interactive)
  (or
   (mythryl-indent-comment-line)
   (save-restriction
     (widen)
     (let ((case-fold-search nil)
	   (oi (current-indentation)) ;; Original indentation
	   (b (save-excursion
		;; Look for a previous line we can anchor the indentation to
		(mythryl-indent--back-to-anchor)
		(cons (current-indentation)
		      (point))))
	   (mp nil) ;; when set, point to restore after indenting
	   )
       (save-excursion
	 (let ((bl (cdr b))
	       (i ;; accumulated indentation level
		(save-excursion
		  (goto-char (cdr b))
		  (if (looking-at "\\<\\(else\\|elif\\|herein\\)\\>")
		      (let ((c (char-after (point))))
			(cond
			 ((eq c ?e)
			  mythryl-if-indent-level)
			 (t mythryl-block-indent-level)))
		    0)))
	       (li 0) ;; line-specific indentation level

	       (sct (list (list 'pst))) ;; scanning context tags
	       ;; Tags description
	       ;; pat: inside a pattern matching statement
	       ;; pkg: between the package word and '{'
	       ;; pst: before the beginning of a new statement
	       ;; fst: at the first line of a statement
	       brc ;; True when identing a line starting with a brace
	       )
	   (backward-to-indentation 0)
	   (mythryl-skip-closing-2)
	   (setq brc (mythryl-line-has-opening-brace))
	   (narrow-to-region bl (point))
	   (goto-char (point-min))
	   (save-excursion
	     (while (re-search-forward
		     (eval-when-compile
		       (concat
			"\\([][{}()\n\"\'#/;]"
			"\\|[\\!%&$+/:<=>?@~|*^-]+" ;; mythryl-op-regexp
			"\\|[A-Za-z0-9_']+" ;; mythryl-word-regexp
			"\\)"))
		     nil t)
	       (goto-char (match-beginning 0))
	       (let ((p (char-after (point)))
		     (mae (match-end 0)))
		 (setq li 0)
		 (setq i (+ i
			    (cond
			     ((let ((mal (- mae (point))))
				(cond
				 ((and
				   (eq p ?=)
				   (cond
				    ((= mal 1)
				     (mythryl-indent--del-tags sct 'pat)
				     0)
				    ((and (= mal 2) (eq (char-after (+ (point) 1)) ?>))
				     (if (mythryl-indent--has-tag sct 'pat)
					 (progn
					   (mythryl-indent--del-tags sct 'pat)
					   (mythryl-indent--add-tags sct 'pst)
					   mythryl-block-indent-level)
				       (mythryl-indent--add-tags sct 'pst)
				       0)))))
				 ((and (eq p ?:) (= mal 1)
				       (progn
					 (mythryl-indent--add-tags sct 'fst)
				 	 0)))
				 )))
			     ((eq p ?\n)
			      (mythryl-indent--set-tag
			       sct 'fst
			       (mythryl-indent--has-tag sct 'pst))
			      0)
			     ((memq p '(?\' ?\" ?# ?/))
			      (cond
			       ((looking-at mythryl-comment-regexp)
				(setq mae (match-end 0)))
			       ((looking-at mythryl-string-regexp)
				(mythryl-indent--del-tags sct 'pst)
				(setq mae (match-end 0)))
			       (t nil))
			      0)
			     ((eq p ?\;)
			      (mythryl-indent--del-tags sct 'pat 'pkg)
			      (mythryl-indent--add-tags sct 'pst)
			      0)
			     ((eq p ?\{)
			      (setq sct (cons (list 'pst) sct))
			      mythryl-brace-indent-level)
			     ((eq p ?\})
			      (setq sct (or (cdr sct) (list nil)))
			      (mythryl-indent--add-tags sct 'fst)
			      (when mythryl-continued-line-indent-braced-blocks
				(mythryl-indent--add-tags sct 'pst))
			      (- mythryl-brace-indent-level))
			     ((eq p ?\()
			      (mythryl-indent--set-tag
			       sct 'pst
			       (mythryl-indent--has-tag sct 'pkg))
			      mythryl-paren-indent-level)
			     ((eq p ?\[)
			      (mythryl-indent--del-tags sct 'pst)
			      mythryl-paren-indent-level)
			     ((or (eq p ?\]) (eq p ?\)))
			      (mythryl-indent--del-tags sct 'pst)
			      (- mythryl-paren-indent-level))
			     ((and
			       (eq p ?a)
			       (cond
				((looking-at "\\<also\\>")
				 (mythryl-indent--add-tags sct 'pst)
				 0))))
			     ((and
			       (eq p ?c)
			       (cond
				((looking-at "\\<case\\>")
				 (mythryl-indent--add-tags sct 'pst)
				 (setq mae (or (mythryl-end-of-next-expression "\\<esac\\>") mae))
				 mythryl-case-indent-level))))
			     ((and
			       (eq p ?e)
			       (cond
				((looking-at "\\<end\\>") (- mythryl-block-indent-level))
				((looking-at "\\<else\\>")
				 (mythryl-indent--add-tags sct 'pst)
				 (setq li (* -1 mythryl-if-indent-level))
				 0)
				((looking-at "\\<elif\\>")
				 (mythryl-indent--add-tags sct 'pst)
				 (setq mae (or (mythryl-end-of-next-expression "\\<fi\\>") mae))
				 (setq li (* -1 mythryl-if-indent-level))
				 0)
				((looking-at "\\<esac\\>") (- mythryl-case-indent-level))
				((looking-at "\\<except\\>")
				 (mythryl-indent--add-tags sct 'pat)
				 (mythryl-indent--del-tags sct 'pst)
				 0))))
			     ((and
			       (eq p ?f)
			       (cond
				((looking-at "\\<fu?n\\>")
				 (mythryl-indent--add-tags sct 'pat)
				 (mythryl-indent--del-tags sct 'pst)
				 0)
				((looking-at "\\<fi\\>") (- mythryl-if-indent-level)))))
			     ((and
			       (eq p ?h)
			       (cond
				((looking-at "\\<herein\\>")
				 (setq li (* -1 mythryl-block-indent-level))
				 (mythryl-indent--add-tags sct 'pst)
				 0))))
			     ((and
			       (eq p ?i)
			       (cond
				((looking-at "\\<if\\>")
				 (mythryl-indent--add-tags sct 'pst)
				 (setq mae (or (mythryl-end-of-next-expression "\\<fi\\>") mae))
				 mythryl-if-indent-level))))
			     ((and
			       (eq p ?p)
			       (cond
				((looking-at "\\<package\\>")
				 (mythryl-indent--add-tags sct 'pkg) 0))))
			     ((and
			       (eq p ?s)
			       (cond
				((looking-at "\\<stipulate\\>")
				 (mythryl-indent--add-tags sct 'pst)
				 mythryl-block-indent-level))))
			     ((and
			       (eq p ?w)
			       (cond
				((looking-at "\\<where\\>")
				 (if (mythryl-indent--has-tag sct 'pkg) 0
				   (mythryl-indent--add-tags sct 'pst)
				   (setq li (* -1 mythryl-block-indent-level))
				   mythryl-block-indent-level)))))
			     (t
			      (mythryl-indent--del-tags sct 'pst)
			      0))))
		 (goto-char mae))))
	   (goto-char (point-max)) (widen)
	   (setq b (car b))
	   (backward-to-indentation 0)
	   (setq i (+ (if (or brc (mythryl-indent--any-tags sct 'fst 'pst)) 0 4) li b i))
	   (unless (= oi i)
	     (delete-region
	      (point)
	      (save-excursion (beginning-of-line) (point)))
	     (indent-to i)
	     (setq mp (point)))))
       (if (and mp (< (point) mp)) (goto-char mp))))))

;; Used by font-lock-defaults
(defun mythryl-beginning-of-syntax ()
  (unless (re-search-backward mythryl-code-line-regexp nil t)
    (beginning-of-line)))

(defcustom mythryl-auto-indent t
  "Whether to use automatic indentation in `mythryl-mode'"
  :type 'boolean :group 'mythryl :group 'mythryl-indent)

(defcustom mythryl-electric-keys t
  "Whether to enable some electric keys in `mythryl-mode'.

Currently, \";\" and \"}\" are defined as electric keys."
  :type 'boolean :group 'mythryl)

(defcustom mythryl-syntax-highlighting t
  "Whether to enable syntax highlighting in `mythryl-mode'."
  :type 'boolean :group 'mythryl)

(defun mythryl-electric-key (arg)
  "Insert a key (\\{self-insert-command}) and indent line."
  (interactive "P*")
  (self-insert-command (prefix-numeric-value arg))
  (apply indent-line-function ()))

(defvar mythryl-mode-map nil
   "Keymap used for \\{mythryl-mode}")
(if mythryl-mode-map nil
  (setq mythryl-mode-map (make-sparse-keymap))
  (when mythryl-electric-keys
    (define-key mythryl-mode-map (kbd "}") 'mythryl-electric-key)
    (define-key mythryl-mode-map (kbd ";") 'mythryl-electric-key)))

(defvar mythryl-mode-font-lock-keywords
  (list
   (list (concat "\\(\\<package\\>\\)[ \t]+\\(" mythryl-word-regexp "\\)[ \t]+=[ \t]+\\(" mythryl-word-regexp "\\)") (list 1 font-lock-keyword-face) (list 2 mythryl-mode-pkg-face) (list 3 mythryl-mode-pkg-face))
   (list (concat "\\<\\(include\\|package\\)\\>[ \t]+\\(\\([a-z][a-z'_0-9]*::\\)*" mythryl-word-regexp "\\)") (list 1 font-lock-keyword-face) (list 2 mythryl-mode-pkg-face))
   (list (concat "\\<\\(fun\\)\\>[ \t]+\\(" mythryl-word-regexp "\\)") (list 1 font-lock-keyword-face) (list 2 font-lock-function-name-face))
   (list
    (eval-when-compile
      (concat "\\(#[0-9]+\\>\\|-[RWX]\\|"
             (regexp-opt
              (list
	       "print"

   	       ;;; From src/app/makelib/main/makelib-g.pkg
   	       "exit" "in" "bash" "system" "round" "atoi" "atod" "basename" "dirname" "trim" "die"

	       ;; maybe reconsider: "join'" and "shuffle'"
   	       "chomp" "chdir" "environ" "explode" "factors" "fields" "filter" "fscanf" "getcwd"
   	       "getenv" "getpid" "getppid" "getuid" "geteuid" "getgid" "getegid" "getgroups"
   	       "getlogin" "getpgrp" "setgid" "setpgid" "setsid" "setuid" "implode" "iseven"
   	       "isodd" "isprime" "join" "join'" "lstat" "mkdir" "now" "product" "rename" "rmdir"
   	       "shuffle" "shuffle'" "sleep" "sort" "sorted" "scanf" "sscanf" "stat" "strcat"
   	       "strlen" "strsort" "struniqsort" "sum" "symlink" "time" "tolower" "toupper"
   	       "tokens" "uniquesort" "unlink" "words"

   	       "arg0" "argv"

               "isfile" "isdir" "ispipe" "issymlink" "issocket" "ischardev" "isblockdev"

   	       "mayread" "maywrite" "mayexecute"

   	       "eval" "evali" "evalf" "evals" "evalli" "evallf" "evalls"

   	       ;; * from src/lib/src/lib/thread-kit/src/core-thread-kit/

   	       ;; TODO

   	       ;; ** from src/lib/src/lib/thread-kit/src/core-thread-kit/thread.pkg
   	       "make_thread" "reset" "notify_and_dispatch" "thread_done" "yield" ;; TODO: review
	       
   	       ) 'words)
             "\\)"))
    (list 1 font-lock-builtin-face))
   (list "^#DO\\>" 0 (list font-lock-preprocessor-face))
   (list
    (eval-when-compile
      (regexp-opt
       ;; Maybe add "before"
       (list "abstype" "also" "and" "api" "as" "case" "class" "elif"
	     "else" "end" "eqtype" "esac" "except" "exception" "fi"
	     "field" "fn" "for" "fprintf" "fun" "generic" "generic_api"
	     "herein" "if" "include" "infix" "infixr" "lazy" "method"
	     "my" "nonfix" "op" "or" "overload" "package" "printf"
	     "raise" "rec" "sharing" "sprintf" "stipulate" "type"
	     "val" "where" "with" "withtype") 'words))
    (list 1 font-lock-keyword-face))
   (list mythryl-character-constant-regexp  (list 0 font-lock-string-face))
   (list (car mythryl-perl-match-regexps) (list 0 font-lock-string-face))
   (list (cadr mythryl-perl-match-regexps) (list 0 font-lock-string-face))
   (list "\\(\\<[a-z][a-z'_0-9]*::+\\)" (list 1 mythryl-mode-pkg-face))
   ;; (list "\\((\\)\\([\\!%&$+/:<=>?@~|*^-]+\\)\\()\\)" 1 font-lock-variable-name-face 2 mythryl-mode-op-face 3 font-lock-variable-name-face) ;; Haskell style operator references
   (list "\\(\\<[a-z][a-zA-Z'_0-9]*\\|[ \t]+[.#][a-z][a-zA-Z'_0-9]*\\)\\>("
	 (list 1 font-lock-function-name-face))
   (list "\\(\\<[a-z][a-zA-Z'_0-9]*\\|[ \t]+[.#][a-z][a-zA-Z'_0-9]*\\)\\>"
	 (list 0 font-lock-variable-name-face))
   (list "\\<[A-Z]\\(_[A-Za-z'_0-9]+\\)?\\>"
	 (list 0 mythryl-mode-type-variable-face))
   (list "\\<[A-Z][A-Za-z'_0-9]*[a-z][A-Za-z'_0-9]*\\>"
	 (list 0 font-lock-type-face))
   (list "\\<_\\>"
	 (list 0 mythryl-mode-underscore-face))
   (list "\\<\\(_\\|[A-Z][A-Z'_0-9]*[A-Z][A-Z'_0-9]*\\)\\>"
	 (list 0 font-lock-constant-face))
   (list mythryl-op-regexp
	 (list 0 mythryl-mode-op-face))
   (list "[][(){};,.]+"
	 (list 0 mythryl-mode-structure-face))
   ))

;; Simple abbrevation table

;;   ifel -> if (_) ...; else ...; fi;
;;   stip -> stipulate _; herein ...; end;
;;    cas -> case (_) ... => ...; esac;

(define-abbrev-table 'mythryl-mode-abbrev-table '())

(defun mythryl-insert-ifelse ()
  "Generate if (...) ... else ... fi;"
  (interactive)
  (let ((s (point)))
    (let ((e
	   (progn
	     (insert "if ()\n \nelse\n \nfi;\n")
	     (point))))
      (goto-char (+ s 4))
      (indent-region s e))))
(put 'mythryl-insert-ifelse 'no-self-insert t)

(defun mythryl-insert-stipulate ()
  "Generate an empty stipulate ... herein ... end; block"
  (interactive)
  (let ((s (point)))
    (let ((e
	   (progn
	     (insert "stipulate\n \nherein\n \nend;")
	     (point))))
      (goto-char (+ s 9))
      (indent-region s e))))
(put 'mythryl-insert-stipulate 'no-self-insert t)

(defun mythryl-insert-case ()
  "Generate case (...) ... => ...; esac;"
  (interactive)
  (let ((s (point)))
    (let ((e
	   (progn
	     (insert "case ()\n => ;\nesac;")
	     (point))))
      (goto-char (+ s 6))
      (indent-region s e))))
(put 'mythryl-insert-case 'no-self-insert t)

(define-abbrev mythryl-mode-abbrev-table "ifel" "" (function mythryl-insert-ifelse))
(define-abbrev mythryl-mode-abbrev-table "stip" "" (function mythryl-insert-stipulate))
(define-abbrev mythryl-mode-abbrev-table "cas" "" (function mythryl-insert-case))

;; Maybe add more templates like:
;;    pkg -> package _;

;;    if (_) ...; fi;
;;    elif (_) ...;

;; define-derived-mode

;; XEmacs (at least some versions) requires quite different support code
(when (featurep 'xemacs) (require 'compile))

(if (functionp 'compilation-build-compilation-error-regexp-alist)
    (progn
      ;; for XEmacs
      (require 'compile)
      (add-to-list 'compilation-error-regexp-alist-alist
		   '(mythryld ("^\\([^ \n\t:]+\\):\\([0-9]+\\).* Error:" 1 2)))
      (compilation-build-compilation-error-regexp-alist))
  ;; for GNU Emacs
  (eval-after-load 'compile
    '(progn
       (add-to-list 'compilation-error-regexp-alist 'mythryld)
       (add-to-list 'compilation-error-regexp-alist-alist
		    '(mythryld "^\\([^ \n\t:]+\\):\\([0-9]+\\).* Error:" 1 2)))))

;; Outline support
(defvar mythryl-mode-outline-regexp
  (concat "[ \t{]*\\(\\<\\(\\(also[ \t]+\\)?\\(fun\\|my\\)\\|api\\|generic\\|package\\|stipulate\\|herein\\)\\>"
	  "\\([^;#]\\|#[^# ]\\)*" ; Match mythryl expression code (no comments)
	  "\\($\\|=>\\|#[# ]\\)\\|##\\)"))

;; Other version: "[ \t{]*\\<\\(fun\\|package\\|stipulate\\|herein\\|where\\)\\>"
(defun mythryl-mode-outline-level ()
  (save-match-data
    (looking-at "[ \t{]*")
    (string-width (match-string 0))))

(defcustom mythryl-mode-turn-on-outline t
  "Automatically turn `outline-minor-mode' on."
  :type 'boolean :group 'mythryl)

;;;###autoload
(define-derived-mode mythryl-mode fundamental-mode
  "Mythryl"
  "Major mode for the Mythryl programming language.

See also: `mythryl-mode-turn-on-outline'."
  :group 'mythryl
  :abbrev-table mythryl-mode-abbrev-table
  :syntax-table mythryl-mode-syntax-table
  (when mythryl-electric-keys
    (define-key mythryl-mode-map ";" (function mythryl-electric-key)))
  (when mythryl-auto-indent
    (set (make-local-variable 'indent-line-function) (function mythryl-indent-line)))

  (set (make-local-variable 'compile-command) "mythryld ")
  
  ;; Configure outlines
  (set (make-local-variable 'outline-regexp) mythryl-mode-outline-regexp)
  (set (make-local-variable 'outline-level) (function mythryl-mode-outline-level))
  (when mythryl-mode-turn-on-outline
    (when (functionp 'outline-minor-mode)
      (outline-minor-mode t)))

  (set (make-local-variable 'comment-use-syntax) t)
  ;; (set (make-local-variable 'comment-style) 'plain) ;; Would setting this up help?
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "\\(#[#!]*\\|/[*]+\\)\\([\t ]\\|$\\)")
  (set (make-local-variable 'comment-end-skip) "[\t ]*[*]+/") ;; Not sure how to use this variable yet or how it would help, font-lock-comment-end-skip is actually used by font-lock+.el.  --Rev22
  (set (make-local-variable 'comment-end) "") ;; "*/")

  (when mythryl-syntax-highlighting
    (set
     (make-local-variable 'font-lock-defaults)
     (list
      ;; KEYWORDS
      'mythryl-mode-font-lock-keywords
      ;; KEYWORDS-ONLY
      nil
      ;; CASE-FOLD
      nil
      ;; SYNTAX-ALIST
      nil
      ;; SYNTAX-BEGIN
      (function mythryl-beginning-of-syntax)))

    ;; Do not use a special syntax-table for font-lock
    (set (make-local-variable 'font-lock-syntax-table) nil) 

    (set (make-local-variable 'font-lock-comment-end-skip) comment-end-skip)
    (set (make-local-variable 'font-lock-syntactic-keywords)
    	 (list
    	  (list "#[^#! \t\n]" 0 "w")
    	  (list (car mythryl-perl-match-regexps)  '(1 (7 . ?/)) '(3 (7 . ?/)))
    	  (list (cadr mythryl-perl-match-regexps) '(1 (7 . ?|)) '(3 (7 . ?|)))
    	  (list mythryl-character-constant-regexp '(1 (7 . ?')) '(3 (7 . ?')))))))

;;; Mythryl interaction mode

;;;###autoload
(defun run-mythryl ()
  "Start an interactive Mythryl session."
  (interactive)
  (switch-to-buffer (make-comint "Mythryl session" "mythryld"))
  (when mythryl-syntax-highlighting
    (set
     (make-local-variable 'font-lock-defaults)
     (list
      ;; KEYWORDS
      mythryl-mode-font-lock-keywords
      ;; KEYWORDS-ONLY
      nil
      ;; CASE-FOLD
      nil
      ;; SYNTAX-ALIST
      nil
      ;; SYNTAX-BEGIN
      (function beginning-of-line)
      ;; OTHER-VARS
      ))
    (set
     (make-local-variable 'font-lock-keywords)
     mythryl-mode-font-lock-keywords)
    (set (make-local-variable 'font-lock-keywords-only) nil)
    (set (make-local-variable 'font-lock-syntax-table)
	 mythryl-mode-syntax-table)
    (set-syntax-table mythryl-mode-syntax-table)
    (font-lock-mode t)))

(provide 'mythryl-mode)
