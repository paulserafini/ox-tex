;;; ox-plaintex.el --- Plain TeX Back-End for Org Export Engine -*- lexical-binding: t; -*-

;;; Code:
(require 'cl-lib)
(require 'ox-latex)

(defvar org-latex-default-packages-alist)
(defvar org-latex-packages-alist)
(defvar orgtbl-exp-regexp)

(org-export-define-derived-backend 'plaintex 'latex
  :menu-entry
  '(?l 1
       ((?T "As plain TeX buffer" org-plaintex-export-as-latex)
	(?t "As plain TeX file" org-plaintex-export-to-latex)))
  :options-alist
  '((:latex-class "LATEX_CLASS" nil "plaintex" t)
    (:plaintex-text-markup-alist nil nil org-plaintex-text-markup-alist))
  :translate-alist
  '((bold . org-plaintex-bold)
    (code . org-plaintex-code)
    (footnote-reference . org-plaintex-footnote-reference)
    (italic . org-plaintex-italic)
    (latex-math-block . org-plaintex-math-block)
    (item . org-plaintex-item)
    (plain-list . org-plaintex-plain-list)
    (quote-block . org-plaintex-quote-block)
    (src-block . org-plaintex-src-block)
    (subscript . org-plaintex-subscript)
    (superscript . org-plaintex-superscript)
    (underline . org-plaintex-underline)
    (table . org-plaintex-table)
    (template . org-plaintex-template)
    (verbatim . org-plaintex-verbatim)))

;;; Add a minimal class with plain TeX style sections
(unless (assoc "plaintex" org-latex-classes)
  (add-to-list 'org-latex-classes
	       '("plaintex"
		 "[NO-DEFAULT-PACKAGES]
                  [NO-PACKAGES]"
		 ("\\section %s" . "\\section %s")
		 ("\\subsection %s" . "\\subsection %s"))))

(defgroup org-export-plaintex nil
  "Options specific for using the plaintex class in LaTeX export."
  :tag "Org plaintex"
  :group 'org-export
  :version "25.3")

;; Plain TeX file template
(defun org-plaintex-template (contents info)
  "Return complete document string after LaTeX conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let ((title (org-export-data (plist-get info :title) info))
        (spec (org-latex--format-spec info)))
    (concat
     ;; Time-stamp.
     (and (plist-get info :time-stamp-file)
          (format-time-string "%% Created %Y-%m-%d %a %H:%M\n"))
     ;; LaTeX compiler.
     (org-latex--insert-compiler info)
     ;; Document class and packages.
     (org-latex-make-preamble info)
     ;; Possibly limit depth for headline numbering.
     (let ((sec-num (plist-get info :section-numbers)))
       (when (integerp sec-num)
         (format "\\setcounter{secnumdepth}{%d}\n" sec-num)))
     ;; Author.
     (let ((author (and (plist-get info :with-author)
                        (let ((auth (plist-get info :author)))
                          (and auth (org-export-data auth info))))))
       (format "\\name{%s}{}\n" author))
     ;; Date.
     (let ((date (and (plist-get info :with-date) (org-export-get-date info))))
       (format "\\date{%s}\n" (org-export-data date info)))

     ;; Macro definitions
     "\\def\\beginquote{\\begingroup\\par\\narrower\\smallskip\\noindent}\n"
     "\\def\\endquote{\\smallskip\\endgroup\\noindent}\n"
     "\\newbox\\TestBox\n"
     "\\def\\sout#1{\\setbox\\TestBox=\\hbox{#1}%\n"
     "    \\leavevmode\\rlap{\\vrule height 2.5pt depth-1.75pt width\\wd\\TestBox}%\n"
     "    \\box\\TestBox\\ }\n"

     ;; Title and subtitle.
     (let* ((subtitle (plist-get info :subtitle))
            (formatted-subtitle
             (when subtitle
               (format (plist-get info :latex-subtitle-format)
                       (org-export-data subtitle info))))
            (separate (plist-get info :latex-subtitle-separate)))
       (concat
        (format "\\title{%s%s}\n" title
                (if separate "" (or formatted-subtitle "")))
        (when (and separate subtitle)
          (concat formatted-subtitle "\n"))))
     ;; Hyperref options.
     (let ((template (plist-get info :latex-hyperref-template)))
       (and (stringp template)
            (format-spec template spec)))
     ;; Title command.
     (let* ((title-command (plist-get info :latex-title-command))
            (command (and (stringp title-command)
                          (format-spec title-command spec))))
       (org-element-normalize-string
        (cond ((not (plist-get info :with-title)) nil)
              ((string= "" title) nil)
              ((not (stringp command)) nil)
              ((string-match "\\(?:[^%]\\|^\\)%s" command)
               (format command title))
              (t command))))
     ;; Document's body.
     contents
     ;; Creator.
     (and (plist-get info :with-creator)
          (concat (plist-get info :creator) "\n"))
     ;; Document end.
     "\\bye")))


;;; Markup
(defcustom org-plaintex-text-markup-alist '((bold . "{\\bf %s}")
					    (code . "{\\tt %s}")
					    (italic . "{\\it %s}")
					    (strike-through . "\\sout{%s}")
					    (underline . "{\\underline %s}")
					    (verbatim . "{\\tt %s}"))
  "Alist of LaTeX expressions to convert text markup."
  :group 'org-export-plaintex
  :version "26.1"
  :package-version '(Org . "8.3")
  :type 'alist
  :options '(bold code italic strike-through underline verbatim))


;;; Bold
(defun org-plaintex-bold (_bold contents info)
  "Transcode BOLD from Org to LaTeX.
CONTENTS is the text with bold markup.  INFO is a plist holding
contextual information."
  (org-plaintex--text-markup contents 'bold info))


;;; Code
(defun org-latex-code (code _contents info)
  "Transcode a CODE object from Org to LaTeX.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (org-plaintex--text-markup (org-element-property :value code) 'code info))


;;; Italic
(defun org-latex-italic (_italic contents info)
  "Transcode ITALIC from Org to LaTeX.
CONTENTS is the text with italic markup.  INFO is a plist holding
contextual information."
  (org-plaintex--text-markup contents 'italic info))


;;; Underline
(defun org-latex-underline (_underline contents info)
  "Transcode UNDERLINE from Org to LaTeX.
CONTENTS is the text with underline markup.  INFO is a plist
holding contextual information."
  (org-plaintex--text-markup contents 'underline info))


;;; Verbatim
(defun org-latex-verbatim (verbatim _contents info)
  "Transcode a VERBATIM object from Org to LaTeX.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (org-plaintex--text-markup
   (org-element-property :value verbatim) 'verbatim info))


;;; Apply text markup
(defun org-plaintex--text-markup (text markup info)
  "Format TEXT depending on MARKUP text markup.
INFO is a plist used as a communication channel.  See
`org-latex-text-markup-alist' for details."
  (let ((fmt (cdr (assq markup (plist-get info :plaintex-text-markup-alist)))))
    (cl-case fmt
      ;; No format string: Return raw text.
      ((nil) text)
      ;; Handle the `verb' special case: Find an appropriate separator
      ;; and use "\\verb" command.
      (verb
       (let ((separator (org-latex--find-verb-separator text)))
	 (concat "\\verb"
		 separator
		 (replace-regexp-in-string "\n" " " text)
		 separator)))
      ;; Handle the `protectedtexttt' special case: Protect some
      ;; special chars and use "\texttt{%s}" format string.
      (protectedtexttt
       (format "\\texttt{%s}"
	       (replace-regexp-in-string
		"--\\|[\\{}$%&_#~^]"
		(lambda (m)
		  (cond ((equal m "--") "-{}-")
			((equal m "\\") "\\textbackslash{}")
			((equal m "~") "\\textasciitilde{}")
			((equal m "^") "\\textasciicircum{}")
			(t (org-latex--protect-text m))))
		text nil t)))
      ;; Else use format string.
      (t (format fmt text)))))

;; Plain List
(defun org-plaintex-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to LaTeX.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  (let* ((type (org-element-property :type plain-list))
	 (attr (org-export-read-attribute :attr_latex plain-list))
	 (latex-type (let ((env (plist-get attr :environment)))
		       (cond (env (format "%s" env))
			     ((eq type 'ordered) "numberedlist")
			     ((eq type 'descriptive) "description")
			     (t "orderedlist")))))
    (org-latex--wrap-label
     plain-list
     (format "\\%s%s\n%s\\end%s"
	     latex-type
	     (or (plist-get attr :options) "")
	     contents
	     latex-type)
     info)))

;; List counters
(defun org-plaintex-item (item contents info)
  "Transcode an ITEM element from Org to LaTeX.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((counter
	  (let ((count (org-element-property :counter item))
		(level
		 ;; Determine level of current item to determine the
		 ;; correct LaTeX counter to use (enumi, enumii...).
		 (let ((parent item) (level 0))
		   (while (memq (org-element-type
				 (setq parent (org-export-get-parent parent)))
				'(plain-list item))
		     (when (and (eq (org-element-type parent) 'plain-list)
				(eq (org-element-property :type parent)
				    'ordered))
		       (cl-incf level)))
		   level)))
	    (and count
		 (< level 5)
		 (format "\\setcounter{enum%s}{%s}\n"
			 (nth (1- level) '("i" "ii" "iii" "iv"))
			 (1- count)))))
	 (checkbox (cl-case (org-element-property :checkbox item)
		     (on "$\\boxtimes$")
		     (off "$\\square$")
		     (trans "$\\boxminus$")))
	 (tag (let ((tag (org-element-property :tag item)))
		(and tag (org-export-data tag info))))
	 ;; If there are footnotes references in tag, be sure to add
	 ;; their definition at the end of the item.  This workaround
	 ;; is necessary since "\footnote{}" command is not supported
	 ;; in tags.
	 (tag-footnotes
	  (or (and tag (org-latex--delayed-footnotes-definitions
			(org-element-property :tag item) info))
	      "")))
    (concat counter
	    "\\li"
	    (cond
	     ((and checkbox tag)
	      (format "[{%s %s}] %s" checkbox tag tag-footnotes))
	     ((or checkbox tag)
	      (format "[{%s}] %s" (or checkbox tag) tag-footnotes))
	     ;; Without a tag or a check-box, if CONTENTS starts with
	     ;; an opening square bracket, add "\relax" to "\item",
	     ;; unless the brackets comes from an initial export
	     ;; snippet (i.e. it is inserted willingly by the user).
	     ((and contents
		   (string-match-p "\\`[ \t]*\\[" contents)
		   (not (let ((e (car (org-element-contents item))))
			  (and (eq (org-element-type e) 'paragraph)
			       (let ((o (car (org-element-contents e))))
				 (and (eq (org-element-type o) 'export-snippet)
				      (eq (org-export-snippet-backend o)
					  'latex)))))))
	      "\\relax ")
	     (t " "))
	    (and contents (org-trim contents)))))


;; Make halign the default table environment
(defcustom org-latex-default-table-environment "halign"
  "Default environment used to build tables."
  :group 'org-export-plaintex
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)

;; Plain TeX style table alignment
(defun org-plaintex--align-string (table info &optional math?)
  "Return an appropriate LaTeX alignment string.
TABLE is the considered table.  INFO is a plist used as
a communication channel.  When optional argument MATH? is
non-nil, TABLE is meant to be a matrix, where all cells are
centered."
  (or (org-export-read-attribute :attr_latex table :align)
      (let (align)
	;; Extract column groups and alignment from first (non-rule)
	;; row.
	(org-element-map
	    (org-element-map table 'table-row
	      (lambda (row)
		(and (eq (org-element-property :type row) 'standard) row))
	      info 'first-match)
	    'table-cell
	  (lambda (cell)
	    (let ((borders (org-export-table-cell-borders cell info)))
	      ;; Check left border for the first cell only.
	      (when (and (memq 'left borders) (not align))
		(push "|" align))
	      (push (if math? "\\hfil#\\hfil"	;center cells in matrices
		      (cl-case (org-export-table-cell-alignment cell info)
			(left "\\quad#\\hfil")
			(right "\\hfil#\\quad")
			(center "\\hfil#\\hfil")))
		    align)
	      (when (memq 'right borders) (push "|" align))))
	  info)
	(string-join align " & "))))


(defun org-plaintex--org-table (table contents info)
  "Return appropriate LaTeX code for an Org table.

TABLE is the table type element to transcode.  CONTENTS is its
contents, as a string.  INFO is a plist used as a communication
channel.

This function assumes TABLE has `org' as its `:type' property and
`table' as its `:mode' attribute."
  (let* ((alignment (org-plaintex--align-string table info)))
    ;; Prepare the final format string for the table.
    (format "\\halign{\n%s\\cr\n%s}"
    	    alignment
    	    contents)))

(defun org-plaintex-table (table contents info)
  "Transcode a TABLE element from Org to LaTeX.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
  (if (eq (org-element-property :type table) 'table.el)
      ;; "table.el" table.  Convert it using appropriate tools.
      (org-latex--table.el-table table info)
    (let ((type (or (org-export-read-attribute :attr_latex table :mode)
		    (plist-get info :latex-default-table-mode))))
      (cond
       ;; Case 1: Verbatim table.
       ((string= type "verbatim")
	(format "\\begin{verbatim}\n%s\n\\end{verbatim}"
		;; Re-create table, without affiliated keywords.
		(org-trim (org-element-interpret-data
			   `(table nil ,@(org-element-contents table))))))
       ;; Case 2: Matrix.
       ((or (string= type "math") (string= type "inline-math"))
	(org-latex--math-table table info))
       ;; Case 3: Standard table.
       (t (concat (org-plaintex--org-table table contents info)
		  ;; When there are footnote references within the
		  ;; table, insert their definition just after it.
		  (org-latex--delayed-footnotes-definitions table info)))))))


;;; Quote
(defun org-plaintex-quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (org-latex--wrap-label
   quote-block (format "\\beginquote\n%s\\endquote" contents) info))

;;; Subscript
(defun org-plaintex-subscript (_subscript contents _info)
  "Transcode a SUBSCRIPT object from Org to LaTeX.
CONTENTS is the contents of the object."
  (format "_{%s}" contents))

;;; Superscript
(defun org-plaintex-superscript (_superscript contents _info)
  "Transcode a SUPERSCRIPT object from Org to LaTeX.
CONTENTS is the contents of the object."
  (format "^{%s}" contents))

;;; Footnote
(defun org-plaintex-footnote-reference (footnote-reference _contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((def (org-export-get-footnote-definition footnote-reference info)))
    (format "\\numberedfootnote{%s}" (org-trim (org-export-data def info)))))

;;;; Src Block
(defun org-plaintex-src-block (src-block _contents info)
  "Transcode a SRC-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (when (org-string-nw-p (org-element-property :value src-block))
    (let* ((lang (org-element-property :language src-block))
	   (caption (org-element-property :caption src-block))
	   (caption-above-p (org-latex--caption-above-p src-block info))
	   (label (org-element-property :name src-block))
	   (custom-env (and lang
			    (cadr (assq (intern lang)
					org-latex-custom-lang-environments))))
	   (num-start (org-export-get-loc src-block info))
	   (retain-labels (org-element-property :retain-labels src-block))
	   (attributes (org-export-read-attribute :attr_latex src-block))
	   (float (plist-get attributes :float))
	   (listings (plist-get info :latex-listings)))
      (concat (format "\\verbatim|%s|endverbatim"
		      (org-export-format-code-default src-block info))))))

;;; Inline math
(defun org-plaintex-math-block (_math-block contents _info)
  "Transcode a MATH-BLOCK object from Org to LaTeX.
CONTENTS is a string.  INFO is a plist used as a communication
channel."
  (when (org-string-nw-p contents)
    (format "$%s$" (org-trim contents))))

;;; Export to a buffer
(defun org-plaintex-export-as-latex
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a plain TeX buffer."
  (interactive)
  (org-export-to-buffer 'plaintex "*Org PLAIN TEX Export*"
    async subtreep visible-only body-only ext-plist (lambda () (LaTeX-mode))))


;;; Export to a .tex file
(defun org-plaintex-export-to-latex
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a plain TeX file."
  (interactive)
  (let ((file (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'plaintex file
      async subtreep visible-only body-only ext-plist)))


(provide 'ox-plaintex)
;;; ox-plaintex ends here
