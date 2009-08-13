;;; mythryl-mode.el --- Major mode for editing Mythryl code

;; Copyright (C) 2009 Phil Rand <philrand@gmail.com>

;; Mythryl-mode is not part of emacs.

;; Mythryl-mode is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; Mythryl-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with mythryl-mode; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; A major mode for editing the Mythryl programming language.
;;
;; The current release is not very useful, but it does recognise
;; mythryl comments beginning with # and ending with newline.
;;
;; This version of mythryl mode is derived from Stefan Monnier's 
;; sml-mode.  See http://www.iro.umontreal.ca/~monnier/elisp/, but
;; as of August 2009, the instructions on that page for accessing
;; the svn repository were incorrect.
;;
;; To use this mode, install the elisp files from the sml-mode
;; suite from the above URL somewhere in your elisp load path,
;; along with this file.  Insert the expression:
;;    (load "mythryl-mode")
;; Somewhere in your .emacs file.
;;
;; I've been invoking it with "M-x mythryl-mode" when editing a 
;; mythryl source file, but when it becomes actually useful for
;; more than debuging itself I'll need to remind myself how to
;; make emacs recognise mythryl source and enter the mode 
;; automatically.

;;; Code:

(require 'sml-mode)

(define-derived-mode mythryl-mode sml-mode
  "Mythryl"
  "Major mode for the Mythryl programming language."
  (modify-syntax-entry ?\# "<" mythryl-mode-syntax-table)
  (modify-syntax-entry ?\n ">#" mythryl-mode-syntax-table))
