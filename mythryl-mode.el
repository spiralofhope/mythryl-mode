;;; mythryl-mode.el --- Major mode and support code for Mythryl
 
;; Copyright (C) 2009 Phil Rand <philrand@gmail.com>
;; Copyright (C) 2010, 2011 Michele Bini <rev.22@hotmail.com> aka Rev22
;;
;; Largly cribbed from Stefan Monnier's sml-mode. See:
;; http://www.iro.umontreal.ca/~monnier/elisp/
;;
 
;; mythryl-mode.el is not part of emacs.
 
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
;; This version of mythryl mode is derived from Stefan Monnier's
;; sml-mode. See http://www.iro.umontreal.ca/~monnier/elisp/, but
;; as of August 2009, the instructions on that page for accessing
;; the svn repository were incorrect.
;;
;; To use this mode, install the elisp files from the sml-mode
;; suite from the above URL somewhere in your elisp load path,
;; along with this file. Insert the expression:
;; (load "mythryl-mode")
;; Somewhere in your .emacs file.

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

;; Indentation support:

;; The current indentation engine is rather unsophisticated and only
;; look at the previous line of code to determine the amount to indent
;; the current line to.

;; * TODO

;; + add mythryl-mode-map
;; + mythryl-interaction-mode
;; + support of outline
;; + support more indentation styles
;; + command (possibly tied to "electric keys")

;; * Changelog:

;; Added comment-start-skip, makes 'uncomment-region' possible. --Rev22, 2010-02-18
;; Added run-mythryl						--Rev22, 2010-02-21
;; Added configurable indent levels.				--Rev22, 2010-02-21

;; * BUGS

;; Trying to indent multi-line comments (and sometimes after them)
;; leads to incorrect behaviour. See:
;; mythryl-7.110.58/src/lib/src/random.api

;;; Code:
 
(defgroup mythryl () "Group for customizing mythryl-mode"
  :prefix "mythryl-" :group 'languages)

(defvar mythryl-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\# "< b" st)
    ;(modify-syntax-entry ?\! ". 2b" st)
    ;(modify-syntax-entry ?\  "- 2b" st)
    ;(modify-syntax-entry ?\t "- 2b" st)
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

Example: pkg1::pkg2::"
  :group 'mythryl)

(defvar mythryl-mode-structure-face 'mythryl-mode-structure-face)
(defface  mythryl-mode-structure-face
  '((((class color) (background light)) (:foreground "black"))
    (((class color) (background dark))  (:foreground "white"))
    (t                                  ()))
  "Face used for structure characters in mythryl."
  :group 'mythryl)

(defvar mythryl-mode-underscore-face 'mythryl-mode-underscore-face)
(defface  mythryl-mode-underscore-face
  '(;;(((class color) (background light)) (:foreground "black"))
    ;;(((class color) (background dark))  (:foreground "white"))
    (t                                  (:weight bold :inherit font-lock-constant-face)))
  "Face used for the underscore wildcard.
This is a bold character by default."
  :group 'mythryl)

(defconst mythryl-comment-line-regexp
  ;;"#\\(\n\\|\\($\\|[ #!]\\).*\\)"
  "#\\($\\|[ #!]\\).*"
  )

(defconst mythryl-comment-regexp
  ;; "\\(#\\($\\|[ #!]\\).*\\|/[*]\\([^*]\\|\\*[^/]\\)*[*]/\\)" ;; appears to be buggy
  (concat "\\(" mythryl-comment-line-regexp "\\|/[*]+\\([^*/]+/*[*]*\\)*[*]/\\)"))

(defconst mythryl-string-regexp "\"\\([^\"\\]\\|\n\\|\\\\.\\)*\"")

(defconst mythryl-comment-or-string-regexp
  (concat "\\(" mythryl-comment-regexp
	  "\\|" mythryl-string-regexp
	  "\\)"))

(defconst mythryl-op-regexp "[\\!%&$+/:<=>?@~|*^-]+")

(defvar mythryl-mode-hook nil
  "*Run upon entering `mythryl-mode'.
This is a good place to put your preferred key bindings.")

;;; * Indentation support code

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
	   "\\(\\([]}); ]\\|"
	   (regexp-opt (mapcar 'symbol-name '(end fi esac then herein elif else)) 'words)
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
(defcustom mythryl-brace-indent-level 3
  "Indentation level for braced blacks."
  :group 'mythryl-indent :type 'integer)
(defcustom mythryl-paren-indent-level 2
  "Indentation level for open parenthesis"
  :group 'mythryl-indent :type 'integer)
(defcustom mythryl-block-indent-level 4
  "Indentation level for other blocks.

This includes \"fun..end\", \"where..end\",
\"except..end\", \"stipulate..herein..end\""
  :group 'mythryl-indent :type 'integer)
(defcustom mythryl-fun-flexible-indent t
  "Allow some flexibility in the indentation of \"fun\" blocks

Support for this functionality is incomplete and buggy."
  :group 'mythryl-indent :type 'boolean)

;; See also:
;; http://mythryl.org/my-Indentation.html
;; http://mythryl.org/my-If_statements.html
(defun mythryl-indent-line ()
  (interactive)
  (save-restriction
    (widen)
    (let ((c (current-indentation))
	  (b (save-excursion
	       (let (p)
		 (while
		     (and
		      (backward-to-indentation 1)
		      (or (looking-at "=*[ \t]*$")
			  (looking-at mythryl-comment-line-regexp)
			  (looking-at "\\<where\\>")
			  ;; 'where' may be after a package or a code block
			  )
		      (or (not p) (< (point) p))
		      )
		   (setq p (point))))
	       (mythryl-skip-closing)
	       (cons (current-indentation)
		     (point))))
	  (mp nil))
      (save-excursion
	(let ((bl (cdr b)) (i 0) (ln 1) (fun '(nil)) (pkg '(nil)))
	  (backward-to-indentation 0)
	  (mythryl-skip-closing-2)
	  (narrow-to-region bl (point))
	  (goto-char (point-min))
	  (save-excursion
	    (while (re-search-forward
		    (eval-when-compile
		      (concat
		       "\\([][{}()\n\"#/;]\\|"
		       "[^\\!%&$+/:<=>?@~|*^-]\\(=>?\\)[^\\!%&$+/:<=>?@~|*^-]\\|"
		       (regexp-opt
			(mapcar
			 'symbol-name
			 '(where end case esac if fi then else elif
				 stipulate herein except
				 fun fn package))
			'words)
		       "\\)"))
		    nil t)
	      (goto-char (match-beginning 0))
	      (let ((p (char-after (point)))
		    (mae (match-end 0)))
		(setq i (+ i
			   (cond
			    ((let ((m (match-string 2))) ;; => and =
			       (when m
				 (setq mae (match-end 2))
				 (if (car fun) (progn (setcar fun nil)
						      (if (string= m "=>")
							  mythryl-block-indent-level
							0)) 0))))
			    ((eq p ?\n) (setq ln -1) 0)
			    ((or (eq p ?\")
				 (eq p ?#)
				 (eq p ?/))
			     (and (looking-at
				   mythryl-comment-or-string-regexp)
				  (setq mae (match-end 0)))
			     0)
			    ((eq p ?\;) (setcar fun nil) (setcar pkg nil) 0)
			    ((eq p ?\{)
			     (setq fun (cons nil fun)
				   pkg (cons nil pkg))
			     mythryl-brace-indent-level)
			    ((eq p ?\})
			     (setq fun (or (cdr fun) '(nil))
				   pkg (or (cdr pkg) '(nil)))
			     (- mythryl-brace-indent-level))
			    ((or (eq p ?\[) (eq p ?\()) mythryl-paren-indent-level)
			    ((or (eq p ?\]) (eq p ?\))) (- mythryl-paren-indent-level))
			    ((eq p ?c)
			     (cond
			      ((looking-at "\\<case\\>") mythryl-case-indent-level)
			      (t 0)))
			    ((eq p ?f)
			     (cond
			      ((looking-at "\\<fu?n\\>") (setcar fun t) 0)
			      ((looking-at "\\<fi\\>") (- mythryl-if-indent-level))
			      (t 0)))
			    ((eq p ?e)
			     (cond
			      ((looking-at "\\<end\\>") (- mythryl-block-indent-level))
			      ((looking-at "\\<else\\>")
			       (let ((n ln)) (setq ln 0) (* n mythryl-if-indent-level)))
			      ((looking-at "\\<elif\\>")
			       (let ((n ln)) (setq ln 0) (* n mythryl-if-indent-level)))
			      ((looking-at "\\<esac\\>") (- mythryl-case-indent-level))
			      ((looking-at "\\<except\\>")
			       (setcar fun t) 0)
			      (t 0)))
			    ((eq p ?h)
			     (cond
			      ((looking-at "\\<herein\\>")
			       (let ((n ln)) (setq ln 0)
				    (* n mythryl-block-indent-level)))
			      (t 0)))
			    ((eq p ?i)
			     (cond
			      ((looking-at "\\<if\\>")
			       (setq ln 0)
			       mythryl-if-indent-level)
			      (t 0)))
			    ((eq p ?p)
			     (cond
			      ((looking-at "\\<package\\>") (setcar pkg t) 0)
			      (t 0)))
			    ((eq p ?s)
			     (cond
			      ((looking-at "\\<stipulate\\>") mythryl-block-indent-level)
			      (t 0)))
			    ((eq p ?t)
			     (cond
			      ((looking-at "\\<then\\>")
			       (let ((n ln)) (setq ln 0) (* n mythryl-if-indent-level)))
			      (t 0)))
			    ((eq p ?w)
			     (cond
			      ((looking-at "\\<where\\>")
			       (if (car pkg) 0 mythryl-block-indent-level))
			      (t 0)))
			    (t (error
				(concat
				 "Unexpected char while scanning: "
				 (string p))))
			    )))
		(goto-char mae))))
	  (goto-char (point-max)) (widen)
	  (setq b (car b))
	  (backward-to-indentation 0)
	  (setq i (+ b i))
	  (if (and mythryl-fun-flexible-indent (car fun))
	      (if (< c b)
		  (setq i b)
		(if (> c (+ b mythryl-block-indent-level)) (setq i (+ b mythryl-block-indent-level))
		  (setq c b))))
	  (unless (= c i)
	    (delete-region
	     (point)
	     (save-excursion (beginning-of-line) (point)))
	    (indent-to i)
	    (setq mp (point)))))
    (if (and mp (< (point) mp)) (goto-char mp)))))

(defcustom mythryl-auto-indent t
  "Whether to use automatic indentation in `mythryl-mode'"
  :type 'boolean :group 'mythryl :group 'mythryl-indent)

(defcustom mythryl-electric-keys nil
  "Whether to enable some electric keys in `mythryl-mode'.

Currently, only <colon> is defined as an electric key."
  :type 'boolean :group 'mythryl)

(defcustom mythryl-syntax-highlighting t
  "Whether to enable syntax highlighting in `mythryl-mode'."
  :type 'boolean :group 'mythryl)

(defun mythryl-electric-key (arg)
  "Insert a key (\\{self-insert-command}) and indent line."
  (interactive "P*")
  (self-insert-command (prefix-numeric-value arg))
  (apply indent-line-function ()))

;; (defvar mythryl-mode-map nil
;;   "Keymap used for \\{mythryl-mode}")
;; (if mythryl-mode-map nil
;;   (setq mythryl-mode-map (copy-keymap 'fundamental-mode-map))
;;   )

(defconst mythryl-mode-font-lock-keywords
  (list
   (list "#[0-9]" 0 font-lock-builtin-face)
   (list "^#DO\\>" 0 font-lock-preprocessor-face)
   (list
    (eval-when-compile
      (regexp-opt
       ;; Maybe add "before", and remove "then"
       (list "abstype" "also" "and" "api" "as" "case" "class" "elif"
	     "else" "end" "eqtype" "esac" "except" "exception" "fi"
	     "field" "fn" "for" "fprintf" "fun" "generic" "generic_api"
	     "herein" "if" "include" "infix" "infixr" "lazy" "method"
	     "my" "nonfix" "op" "or" "overload" "package" "printf"
	     "raise" "rec" "sharing" "sprintf" "stipulate" "then" "type"
	     "val" "where" "with" "withtype") 'words))
    1 font-lock-keyword-face)
   (list "\\(\\<[a-z][a-z'_0-9]*::+\\)" 1 mythryl-mode-pkg-face)
   (list "(\\([\\!%&$+/:<=>?@~|*^-]+\\))" 0 font-lock-variable-name-face) ;; Haskell style operator references
   (list "\\(\\<[a-z][a-zA-Z'_0-9]*\\|[ \t]+[.#][a-z][a-zA-Z'_0-9]*\\)\\>" 0 font-lock-variable-name-face)
   (list "\\<[A-Z][A-Za-z'_0-9]*[a-z][A-Za-z'_0-9]*\\>" 0 font-lock-type-face)
   (list "\\<_\\>" 0 mythryl-mode-underscore-face)
   (list "\\<\\(_\\|[A-Z][A-Z'_0-9]*[A-Z][A-Z'_0-9]*\\)\\>" 0 font-lock-constant-face)
   (list mythryl-op-regexp 0 mythryl-mode-op-face)
   (list "[][(){};,.]+" 0 mythryl-mode-structure-face)
   ))

;;;###autoload
(define-derived-mode mythryl-mode fundamental-mode
  "Mythryl"
  "Major mode for the Mythryl programming language."
  :group 'mythryl
  ;; :abbrev-table (let ((a (make-abbrev-table)))
  :syntax-table mythryl-mode-syntax-table
  (when mythryl-electric-keys
    (define-key mythryl-mode-map ";" 'mythryl-electric-key))
  (when mythryl-auto-indent
    (set (make-local-variable 'indent-line-function) 'mythryl-indent-line))
  (set (make-local-variable 'compile-command) "mythryld ")
  (set (make-local-variable 'comment-start) "#\\( \\|$\\)")
  (set (make-local-variable 'comment-start-skip) "\\(#[#!]*\\|/[*]+\\)\\([\t ]+\\|$\\)")
  ;; (set (make-local-variable 'comment-end-skip) "[\t ]*[*]+/")
  (set (make-local-variable 'comment-end) " */")
  (set (make-local-variable 'font-lock-syntactic-keywords)
       (list (list "#[^#! \t\n]" 0 "w")
	     (list "[.][|/]/" 0 "\"")))
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
      'beginning-of-line
      ;; OTHER-VARS
      )))
  )

;;; * Mythryl interaction mode

;;;###autoload
(defun run-mythryl ()
  "Runs mythryl's interactive compiler."
  (interactive)
  (switch-to-buffer (make-comint "mythryl" "mythryld"))
  (mythryl-font-lock-mode 1)
  )

(defun mythryl-font-lock-mode (&rest r)
  (interactive)
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
      'beginning-of-line
      ;; OTHER-VARS
      ))
    (set
     (make-local-variable 'font-lock-keywords)
     mythryl-mode-font-lock-keywords)
    (set (make-local-variable 'font-lock-keywords-only) nil)
    (set (make-local-variable 'font-lock-syntax-table)
	 mythryl-mode-syntax-table)
    (set-syntax-table mythryl-mode-syntax-table)
    (apply 'font-lock-mode r)))

(provide 'mythryl-mode)
