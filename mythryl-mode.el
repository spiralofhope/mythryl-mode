;;; mythryl-mode.el --- Major mode and support code for Mythryl
 
;; Copyright (C) 2009 Phil Rand <philrand@gmail.com>
;; Copyright (C) 2010, 2011 Michele Bini <michele.bini@gmail.com> aka Rev22

;; Version: 2.4.7
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
;; To use this mode, install this file in your elisp load path; then
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

;;; TODO

;; + indent records differently from braced statements
;; + mythryl-interaction-mode
;; + support of outline
;; + more indentation styles
;; + command (possibly tied to "electric keys")
;; + run emacs lint
;; + improve indentation engine with syntax-table functions

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

;;; Repositories:

;; You can find new versions of mythryl mode at the following locations:

;; http://github.com/rev22/mythryl-mode
;; EmacsWiki: http://www.emacswiki.org/emacs/MythrylMode

;;; Code:

;; This version of mythryl mode is derived from Stefan Monnier's
;; sml-mode. See http://www.iro.umontreal.ca/~monnier/elisp/, but
;; as of August 2009, the instructions on that page for accessing
;; the svn repository were incorrect.

;;;###autoload
(defgroup mythryl () "Group for customizing mythryl-mode"
  :prefix "mythryl-" :group 'languages)

(require 'custom)
(require 'font-lock)
(require 'derived)

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

(defconst mythryl-line-comment-regexp "#\\($\\|[\t #!]\\).*")
(defconst mythryl-block-comment-regexp
  "/[*]\\([^*]+\\|[*]+[^/*]+\\)*\\($\\|[*]+/\\)") ;; (re-search-forward mythryl-block-comment-regexp)

(defconst mythryl-comment-regexp
  (concat "\\(" mythryl-line-comment-regexp
	  "\\|" mythryl-block-comment-regexp
	  "\\)"))

(defconst mythryl-string-regexp "\"\\([^\"\\]\\|\n\\|\\\\.\\)*\"")

(defconst mythryl-comment-or-string-regexp
  (concat "\\(" mythryl-comment-regexp
	  "\\|" mythryl-string-regexp
	  "\\)"))

(defconst mythryl-op-regexp "[\\!%&$+/:<=>?@~|*^-]+")

(defconst mythryl-word-regexp "[A-Za-z0-9_']+"
  "A regexp matching every kind of mythryl 'word'.

It matches numbers identifiers, package names, operators, types, apis, type
constructors, pattern identifiers.")

(defconst mythryl-code-line-regexp "^[ \t]*\\([^#/* \t\n]\\|#[^# \t\n]\\|/[^*\n]\\)")

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
	   (regexp-opt (mapcar 'symbol-name '(end fi esac then herein elif also else where)) 'words)
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

(defun mythryl-end-of-next-expression () ;; Return end of next mythryl expression
  (save-excursion
    (goto-char (match-end 0))
    (condition-case nil
	(progn (forward-sexp 1) (point))
      (error nil))))

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

;; See also:
;; http://mythryl.org/my-Indentation.html
;; http://mythryl.org/my-If_statements.html
(defun mythryl-indent-line ()
  (interactive)
  (or
   (mythryl-indent-comment-line)
   (save-restriction
     (widen)
     (let ((oi (current-indentation)) ;; Original indentation
	   (b (save-excursion
		;; Look for a previous line we can anchor the indentation to
		(end-of-line 0)
		(let ((front (point)))
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
		    (if (re-search-backward
			 mythryl-code-line-regexp
			 nil t)
			(progn
			  (setq front (match-beginning 1))
			  (if (re-search-backward
			       (if mythryl-continued-line-indent-braced-blocks
			       "^[^#/*\n]*\\(;\\|\\<also\\>\\)[ \t]*$"
			       "^[^#/*\n]*\\([;{]\\|\\<also\\>\\)[ \t]*$")
			       nil t)
			      (progn
				(goto-char (match-end 0))
				(if (re-search-forward
				     mythryl-code-line-regexp
				     (+ front 1) t)
				    (goto-char (match-beginning 1))
				  (beginning-of-line 2)
				  (forward-to-indentation 0)))
			    (goto-char (point-min))
			    (forward-to-indentation 0)))
		      (goto-char (point-min))
		      (forward-to-indentation 0))))
		(mythryl-skip-closing)
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
	       
	       ;; car of the stack is t when we are inside a pattern
	       ;; matching statement
	       (pat '(nil))
	       
	       ;; car of the stack is t when we are between the package
	       ;; word and '{'
	       (pkg '(nil))

	       ;; car of the stack is t when we are before the beginning of a new statement
	       (pst '(t))

	       ;; car of the stack is t when we are at the first line of a statement
	       (fst '(t))
	       )
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
			  '(where end case esac if fi else elif
				  stipulate also herein except
				  fun fn package))
			 'words)
			"\\|" "[A-Za-z0-9_']+" ;; mythryl-word-regexp
			"\\|" "[\\!%&$+/:<=>?@~|*^-]+" ;; mythryl-op-regexp
			"\\)"))
		     nil t)
	       (goto-char (match-beginning 0))
	       (let ((p (char-after (point)))
		     (mae (match-end 0)))
		 (setq li 0)
		 (setq i (+ i
			    (cond
			     ((let ((m (match-string 2))) ;; => and =
				(when m
				  (setq mae (match-end 2))
				  (if (car pat) (progn (setcar pat nil)
						       (if (string= m "=>")
							   mythryl-block-indent-level
							 0)) 0))))
			     ((eq p ?\n) (setcar fst (car pst)) 0)
			     ((or (eq p ?\")
				  (eq p ?#)
				  (eq p ?/))
			      (cond
			       ((looking-at mythryl-comment-regexp)
				(setq mae (match-end 0)))
			       ((looking-at mythryl-string-regexp)
				(setcar pst nil)
				(setq mae (match-end 0)))
			       (t nil))
			      0)
			     ((eq p ?\;) (setcar pat nil) (setcar pkg nil) (setcar pst t) 0)
			     ((eq p ?\{)
			      (setq pat (cons nil pat)
				    pkg (cons nil pkg)
				    pst (cons t pst)
				    fst (cons nil fst)
				    )
			      (unless mythryl-continued-line-indent-braced-blocks
				(setq li (* -2 mythryl-brace-indent-level)))
			      mythryl-brace-indent-level)
			     ((eq p ?\})
			      (setq pat (or (cdr pat) '(nil))
				    pkg (or (cdr pkg) '(nil))
				    pst (or (cdr pst) '(nil))
				    fst (or (cdr fst) '(nil))
				    )
			      (when mythryl-continued-line-indent-braced-blocks
				(setcar pst t))
			      (- mythryl-brace-indent-level))
			     ((or (eq p ?\[) (eq p ?\())
			      (setcar pst nil)
			      mythryl-paren-indent-level)
			     ((or (eq p ?\]) (eq p ?\)))
			      (setcar pst nil)
			      (- mythryl-paren-indent-level))
			     ((eq p ?a)
			      (cond
			       ((looking-at "\\<also\\>")
				(setcar pst t)
				;;(setq li (* -1 mythryl-block-indent-level))
				;;mythryl-block-indent-level
				0)
			       (t 0)))
			     ((eq p ?c)
			      (setcar pst nil)
			      (cond
			       ((looking-at "\\<case\\>")
				(setcar pst t)
				(setq mae (or (mythryl-end-of-next-expression) mae))
				mythryl-case-indent-level)
			       (t 0)))
			     ((eq p ?f)
			      (setcar pst nil)
			      (cond
			       ((looking-at "\\<fu?n\\>") (setcar pat t) 0)
			       ((looking-at "\\<fi\\>") (- mythryl-if-indent-level))
			       (t 0)))
			     ((eq p ?e)
			      (setcar pst nil)
			      (cond
			       ((looking-at "\\<end\\>") (- mythryl-block-indent-level))
			       ((looking-at "\\<else\\>")
				(setcar pst t)
				(setq li (* -1 mythryl-if-indent-level))
				0)
			       ((looking-at "\\<elif\\>")
				(setcar pst t)
				(setq mae (or (mythryl-end-of-next-expression) mae))
				(setq li (* -1 mythryl-if-indent-level))
				0)
			       ((looking-at "\\<esac\\>") (- mythryl-case-indent-level))
			       ((looking-at "\\<except\\>")
				(setcar pst t)
				(setcar pat t) 0)
			       (t 0)))
			     ((eq p ?h)
			      (setcar pst nil)
			      (cond
			       ((looking-at "\\<herein\\>") (setcar pst t) (setq li (* -1 mythryl-block-indent-level)) 0)
			       (t 0)))
			     ((eq p ?i)
			      (setcar pst nil)
			      (cond
			       ((looking-at "\\<if\\>")
				(setcar pst t)
				(setq mae (or (mythryl-end-of-next-expression) mae))
				mythryl-if-indent-level)
			       (t 0)))
			     ((eq p ?p)
			      (setcar pst nil)
			      (cond
			       ((looking-at "\\<package\\>") (setcar pkg t) 0)
			       (t 0)))
			     ((eq p ?s)
			      (setcar pst nil)
			      (cond
			       ((looking-at "\\<stipulate\\>") (setcar pst t) mythryl-block-indent-level)
			       (t 0)))
			     ((eq p ?w)
			      (setcar pst nil)
			      (cond
			       ((looking-at "\\<where\\>")
				(if (car pkg) 0
				  (setcar pst t)
				  (setq li (* -1 mythryl-block-indent-level))
				  mythryl-block-indent-level
				  ))
			       (t 0)))
			     (t (setcar pst nil) 0)
			     )))
		 (goto-char mae))))
	   (goto-char (point-max)) (widen)
	   (setq b (car b))
	   (backward-to-indentation 0)
	   (setq i (+ (if (or (car fst) (car pst)) 0 4) li b i))
	   (unless (= oi i)
	     (delete-region
	      (point)
	      (save-excursion (beginning-of-line) (point)))
	     (indent-to i)
	     (setq mp (point)))))
       (if (and mp (< (point) mp)) (goto-char mp))))))

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
   (list "[.]\\(|[^|]*|\\)" (list 0 font-lock-string-face))
   (list "[.]\\(/[^/]*/\\)" (list 0 font-lock-string-face))
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
    (list 1 font-lock-keyword-face))
   (list "\\(\\<[a-z][a-z'_0-9]*::+\\)" (list 1 mythryl-mode-pkg-face))
   ;; (list "\\((\\)\\([\\!%&$+/:<=>?@~|*^-]+\\)\\()\\)" 1 font-lock-variable-name-face 2 mythryl-mode-op-face 3 font-lock-variable-name-face) ;; Haskell style operator references
   (list "\\(\\<[a-z][a-zA-Z'_0-9]*\\|[ \t]+[.#][a-z][a-zA-Z'_0-9]*\\)\\>"
	 (list 0 font-lock-variable-name-face))
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

(define-abbrev mythryl-mode-abbrev-table "ifel" "" 'mythryl-insert-ifelse)
(define-abbrev mythryl-mode-abbrev-table "stip" "" 'mythryl-insert-stipulate)
(define-abbrev mythryl-mode-abbrev-table "cas" "" 'mythryl-insert-case)

;; Maybe add more templates like:
;;    pkg -> package _;

;;    if (_) ...; fi;
;;    elif (_) ...;

;; define-derived-mode

;;;###autoload
(define-derived-mode mythryl-mode fundamental-mode
  "Mythryl"
  "Major mode for the Mythryl programming language."
  :group 'mythryl
  :abbrev-table mythryl-mode-abbrev-table
  :syntax-table mythryl-mode-syntax-table
  (when mythryl-electric-keys
    (define-key mythryl-mode-map ";" 'mythryl-electric-key))
  (when mythryl-auto-indent
    (set (make-local-variable 'indent-line-function) 'mythryl-indent-line))
  (set (make-local-variable 'compile-command) "mythryld ")

  (set (make-local-variable 'comment-use-syntax) t)
  ;; (set (make-local-variable 'comment-style) 'plain) ;; Would setting up this up help?
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "\\(#[#!]*\\|/[*]+\\)\\([\t ]\\|$\\)")
  (set (make-local-variable 'comment-end-skip) "[\t ]*[*]+/") ;; Not sure how to use this variable yet or how it would help, font-lock-comment-end-skip is actually used by font-lock+.el.  --Rev22
  (set (make-local-variable 'comment-end) "") ;; "*/")

  (set (make-local-variable 'font-lock-comment-end-skip) comment-end-skip)
  (set (make-local-variable 'font-lock-syntactic-keywords)
       (list (list "#[^#! \t\n]" 0 "w")
	     (list "[.][|/]/" 0 "\"")))
  
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
      'beginning-of-line
      ;; OTHER-VARS
      )))
  )

;;; * Mythryl interaction mode

;;;###autoload
(defun run-mythryl ()
  "Start an interactive Mythryl session."
  (interactive)
  (switch-to-buffer (make-comint "Mythryl session" "mythryld"))
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
