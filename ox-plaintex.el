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
    (:abstract "ABSTRACT" nil nil parse)
    (:plaintex-text-markup-alist nil nil org-plaintex-text-markup-alist))
  :translate-alist
  '((bold . org-plaintex-bold)
    (center-block . org-plaintex-center-block)
    (code . org-plaintex-code)
    (example-block . org-plaintex-example-block)
    (footnote-reference . org-plaintex-footnote-reference)
    (headline . org-latex-headline)
    (italic . org-plaintex-italic)
    (item . org-plaintex-item)
    (latex-math-block . org-plaintex-math-block)
    (plain-list . org-plaintex-plain-list)
    (quote-block . org-plaintex-quote-block)
    (src-block . org-plaintex-src-block)
    (subscript . org-plaintex-subscript)
    (superscript . org-plaintex-superscript)
    (underline . org-plaintex-underline)
    (table . org-plaintex-table)
    (table-row . org-plaintex-table-row)
    (template . org-plaintex-template)
    (verbatim . org-plaintex-verbatim)))

;;; Add a minimal class with plain TeX style sections
(unless (assoc "plaintex" org-latex-classes)
  (add-to-list 'org-latex-classes
	       '("plaintex"
		 "[NO-DEFAULT-PACKAGES]
                  [NO-PACKAGES]"
		 ("\n\\section %s" . "\n\\section %s")
		 ("\n\\subsection %s" . "\n\\subsection %s")
		 ("\n\\subsubsection %s" . "\n\\subsubsection %s"))))

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

     "\\input eplain.tex\n\n"

     "\\def\\begincenter{%\n"
     "\\par\n"
     "\\begingroup\n"
     "\\leftskip=0pt plus 1fil\n"
     "\\rightskip=\\leftskip\n"
     "\\parindent=0pt\n"
     "\\parfillskip=0pt\n"
     "}\n"
     "\\def\\endcenter{%\n"
     "\\par\n"
     "\\endgroup\n"
     "}\n"
     "\\long\\def\\centerpar#1{\\begincenter#1\\endcenter}\n\n"

     "\\font\\fourteenrm= cmr10 at 14pt%\n"
     "\\def\\title#1{\\centerpar{\\fourteenrm#1}\\medskip}\n"
     "\\def\\author#1{\\centerline{#1}\\medskip}\n"
     "\\def\\date#1{\\centerline{#1}}\n"
     "\\def\\abstract#1{\\medskip{\\narrower\\smallskip\\noindent Abstract: #1\\par}}\n"
     "\\def\\keywords#1{{\\narrower\\smallskip\\noindent Key words: \\it #1\\par}}\n\n"

     "\\def\\beginquote{\\begingroup\\par\\narrower\\smallskip\\noindent}\n"
     "\\def\\endquote{\\smallskip\\endgroup\\noindent}\n\n"

     "\\def\\toprule{\\noalign{\\hrule height 1pt}}\n"
     "\\def\\midrule{\\noalign{\\vskip 0.25em \\hrule height 0.5pt}}\n"
     "\\def\\bottomrule{\\noalign{\\vskip 0.25em \\hrule height 1pt}}\n"
     "\\def\\tstrut{\\vrule height 12pt depth3pt width0pt}\n\n"

     ;; Strike-through macro
     ;; https://tex.stackexchange.com/a/93794
     "\\newbox\\TestBox\n"
     "\\def\\sout#1{\\setbox\\TestBox=\\hbox{#1}%\n"
     "    \\leavevmode\\rlap{\\vrule height 2.5pt depth-1.75pt width\\wd\\TestBox}%\n"
     "    \\box\\TestBox\\ }\n\n"

     "\\newcount\\sectioncount\n"
     "\\def\\section #1\n"
     "\\par{\\bigskip\n"
     "  \\subsectioncount=0\n"
     "  \\advance \\sectioncount by 1\n"
     "  {\\noindent\\the\\sectioncount.\\ #1}\n"
     "  \\smallskip\\noindent\n"
     "}\n\n"

    "\\newcount\\subsectioncount\n"
    "\\def\\subsection #1\n"
    "\\par{\\smallskip\n"
    "  \\advance \\subsectioncount by 1\n"
    "  \\subsubsectioncount=0\n"
    "  {\\noindent\\the\\sectioncount.\\the\\subsectioncount\\ #1}\n"
    "  \\smallskip\\noindent\n"
    "}\n\n"

    "\\newcount\\subsubsectioncount\n"
    "\\def\\subsubsection #1\n"
    "\\par{\\smallskip\n"
    "  \\advance \\subsubsectioncount by 1\n"
    "  {\\noindent\\the\\sectioncount.\\the\\subsectioncount.\\the\\subsubsectioncount\\ #1}\n"
    "  \\smallskip\\noindent\n"
    "}\n\n"

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

     ;; Author.
     (let ((author (and (plist-get info :with-author)
                        (let ((auth (plist-get info :author)))
                          (and auth (org-export-data auth info))))))
       (format "\\author{%s}\n" author))

     ;; Date.
     (let ((date (and (plist-get info :with-date) (org-export-get-date info))))
       (format "\\date{%s}\n" (org-export-data date info)))

     ;; Hyperref options.
     (let ((template (plist-get info :latex-hyperref-template)))
       (and (stringp template)
            (format-spec template spec)))

     ;; ;; Table of contents.
     ;; (let ((depth (plist-get info :with-toc)))
     ;;   (when depth
     ;; 	 (concat (when (integerp depth)
     ;; 		   (format "\\setcounter{tocdepth}{%d}\n" depth))
     ;; 		 (plist-get info :latex-toc-command))))

     ;; Add abstract if defined
     (let ((abstract (plist-get info :abstract)))
       (when abstract
         (format "\\abstract{%s}\n"
		 (org-export-data (plist-get info :abstract) info))))

     ;; Add abstract if defined
     (let ((keywords (plist-get info :keywords)))
       (when keywords
         (format "\\keywords{%s}\n"
		 (org-export-data (plist-get info :keywords) info))))

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
    (format "$$\\vbox{\\halign{\n\\tstrut%s\\cr\n%s}}$$"
    	    alignment
    	    contents)))

(defun org-latex-matrices (matrices contents _info)
  "Transcode a MATRICES element from Org to LaTeX.
CONTENTS is a string.  INFO is a plist used as a communication
channel."
  (format (cl-case (org-element-property :markup matrices)
	    (inline "\\(%s\\)")
	    (equation "\\begin{equation}\n%s\\end{equation}")
	    (t "$$\n%s$$"))
	  contents))

(defun org-plaintex--math-table (table info)
  "Return appropriate LaTeX code for a matrix.

TABLE is the table type element to transcode.  INFO is a plist
used as a communication channel.

This function assumes TABLE has `org' as its `:type' property and
`inline-math' or `math' as its `:mode' attribute."
  (let* ((attr (org-export-read-attribute :attr_latex table))
	 (env (or (plist-get attr :environment)
		  (plist-get info :latex-default-table-environment)))
	 (contents
	  (mapconcat
	   (lambda (row)
	     (if (eq (org-element-property :type row) 'rule) "\\hline"
	       ;; Return each cell unmodified.
	       (concat
		(mapconcat
		 (lambda (cell)
		   (substring (org-element-interpret-data cell) 0 -1))
		 (org-element-map row 'table-cell #'identity info) "&")
		(or (cdr (assoc env org-latex-table-matrix-macros)) "\\\\")
		"\n")))
	   (org-element-map table 'table-row #'identity info) "")))
    (concat
     ;; Prefix.
     (plist-get attr :math-prefix)
     ;; Environment.  Also treat special cases.
     (cond ((member env '("array" "tabular"))
	    (format "\\begin{%s}{%s}\n%s\\end{%s}"
		    env (org-latex--align-string table info t) contents env))
	   ((assoc env org-latex-table-matrix-macros)
	    (format "\\%s%s{\n%s}"
		    env
		    (or (plist-get attr :math-arguments) "")
		    contents))
	   (t (format "\\%s{\n%s}" env contents)))
     ;; Suffix.
     (plist-get attr :math-suffix))))

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
	(org-plaintex--math-table table info))
       ;; Case 3: Standard table.
       (t (concat (org-plaintex--org-table table contents info)
		  ;; When there are footnote references within the
		  ;; table, insert their definition just after it.
		  (org-latex--delayed-footnotes-definitions table info)))))))

(defun org-plaintex-table-row (table-row contents info)
  "Transcode a TABLE-ROW element from Org to LaTeX.
CONTENTS is the contents of the row.  INFO is a plist used as
a communication channel."
  (let* ((attr (org-export-read-attribute :attr_latex
					  (org-export-get-parent table-row)))
	 (booktabsp (if (plist-member attr :booktabs) (plist-get attr :booktabs)
		      (plist-get info :latex-tables-booktabs)))
	 (longtablep
	  (member (or (plist-get attr :environment)
		      (plist-get info :latex-default-table-environment))
		  '("longtable" "longtabu"))))
    (if (eq (org-element-property :type table-row) 'rule)
	(cond
	 ((not (org-export-get-previous-element table-row info)) "\\toprule")
	 ((not (org-export-get-next-element table-row info)) "\\bottomrule")
	 (t "\\midrule"))
      (concat
       ;; When BOOKTABS are activated enforce top-rule even when no
       ;; hline was specifically marked.
       (and booktabsp (not (org-export-get-previous-element table-row info))
	    "\\toprule\n")
       contents "\\cr\n"
       (cond
	;; Special case for long tables.  Define header and footers.
	((and longtablep (org-export-table-row-ends-header-p table-row info))
	 (let ((columns (cdr (org-export-table-dimensions
			      (org-export-get-parent-table table-row) info))))
	   (format "%s
\\endfirsthead
\\multicolumn{%d}{l}{%s} \\\\
%s
%s \\\\\n
%s
\\endhead
%s\\multicolumn{%d}{r}{%s} \\\\
\\endfoot
\\endlastfoot"
		   (if booktabsp "\\midrule" "\\hline")
		   columns
		   (org-latex--translate "Continued from previous page" info)
		   (cond
		    ((not (org-export-table-row-starts-header-p table-row info))
		     "")
		    (booktabsp "\\toprule\n")
		    (t "\\hline\n"))
		   contents
		   (if booktabsp "\\midrule" "\\hline")
		   (if booktabsp "\\midrule" "\\hline")
		   columns
		   (org-latex--translate "Continued on next page" info))))
	;; When BOOKTABS are activated enforce bottom rule even when
	;; no hline was specifically marked.
	((and booktabsp (not (org-export-get-next-element table-row info)))
	 "\\bottomrule"))))))

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

;;; Example Block
(defun org-plaintex-example-block (example-block _contents info)
  "Transcode an EXAMPLE-BLOCK element from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (when (org-string-nw-p (org-element-property :value example-block))
      (org-latex--wrap-label
       example-block
       (format "\\verbatim|%s|\\endverbatim"
	       (org-export-format-code-default example-block info))
       info)))

;;; Inline math
(defun org-plaintex-math-block (_math-block contents _info)
  "Transcode a MATH-BLOCK object from Org to LaTeX.
CONTENTS is a string.  INFO is a plist used as a communication
channel."
  (when (org-string-nw-p contents)
    (format "$%s$" (org-trim contents))))

(defun org-plaintex--label (datum info &optional force full)
  "Return an appropriate label for DATUM.
DATUM is an element or a `target' type object.  INFO is the
current export state, as a plist.

Return nil if element DATUM has no NAME or VALUE affiliated
keyword or no CUSTOM_ID property, unless FORCE is non-nil.  In
this case always return a unique label.

Eventually, if FULL is non-nil, wrap label within \"\\label{}\"."
  (let* ((type (org-element-type datum))
	 (user-label
	  (org-element-property
	   (cl-case type
	     ((headline inlinetask) :CUSTOM_ID)
	     (target :value)
	     (otherwise :name))
	   datum))
	 (label
	  (and (or user-label force)
	       (if (and user-label (plist-get info :latex-prefer-user-labels))
		   user-label
		 (concat (cl-case type
			   (headline "sec:")
			   (table "tab:")
			   (latex-environment
			    (and (string-match-p
				  org-latex-math-environments-re
				  (org-element-property :value datum))
				 "eq:"))
			   (paragraph
			    (and (org-element-property :caption datum)
				 "fig:")))
			 (org-export-get-reference datum info))))))
    (cond ((not full) label)
	  (label (format ""
			 label
			 (if (eq type 'target) "" "\n")))
	  (t ""))))


(defun org-latex-headline (headline contents info)
  "Transcode a HEADLINE element from Org to LaTeX.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((class (plist-get info :latex-class))
	   (level (org-export-get-relative-level headline info))
	   (numberedp (org-export-numbered-headline-p headline info))
	   (class-sectioning (assoc class (plist-get info :latex-classes)))
	   ;; Section formatting will set two placeholders: one for
	   ;; the title and the other for the contents.
	   (section-fmt
	    (let ((sec (if (functionp (nth 2 class-sectioning))
			   (funcall (nth 2 class-sectioning) level numberedp)
			 (nth (1+ level) class-sectioning))))
	      (cond
	       ;; No section available for that LEVEL.
	       ((not sec) nil)
	       ;; Section format directly returned by a function.  Add
	       ;; placeholder for contents.
	       ((stringp sec) (concat sec "\n%s"))
	       ;; (numbered-section . unnumbered-section)
	       ((not (consp (cdr sec)))
		(concat (funcall (if numberedp #'car #'cdr) sec) "\n%s"))
	       ;; (numbered-open numbered-close)
	       ((= (length sec) 2)
		(when numberedp (concat (car sec) "\n%s" (nth 1 sec))))
	       ;; (num-in num-out no-num-in no-num-out)
	       ((= (length sec) 4)
		(if numberedp (concat (car sec) "\n%s" (nth 1 sec))
		  (concat (nth 2 sec) "\n%s" (nth 3 sec)))))))
	   ;; Create a temporary export back-end that hard-codes
	   ;; "\underline" within "\section" and alike.
	   (section-back-end
	    (org-export-create-backend
	     :parent 'latex
	     :transcoders
	     '((underline . (lambda (o c i) (format "\\underline{%s}" c))))))
	   (text
	    (org-export-data-with-backend
	     (org-element-property :title headline) section-back-end info))
	   (todo
	    (and (plist-get info :with-todo-keywords)
		 (let ((todo (org-element-property :todo-keyword headline)))
		   (and todo (org-export-data todo info)))))
	   (todo-type (and todo (org-element-property :todo-type headline)))
	   (tags (and (plist-get info :with-tags)
		      (org-export-get-tags headline info)))
	   (priority (and (plist-get info :with-priority)
			  (org-element-property :priority headline)))
	   ;; Create the headline text along with a no-tag version.
	   ;; The latter is required to remove tags from toc.
	   (full-text (funcall (plist-get info :latex-format-headline-function)
			       todo todo-type priority text tags info))
	   ;; Associate \label to the headline for internal links.
	   (headline-label (org-plaintex--label headline info t t))
	   (pre-blanks
	    (make-string (org-element-property :pre-blank headline) ?\n)))
      (if (or (not section-fmt) (org-export-low-level-p headline info))
	  ;; This is a deep sub-tree: export it as a list item.  Also
	  ;; export as items headlines for which no section format has
	  ;; been found.
	  (let ((low-level-body
		 (concat
		  ;; If headline is the first sibling, start a list.
		  (when (org-export-first-sibling-p headline info)
		    (format "\\begin{%s}\n" (if numberedp 'enumerate 'itemize)))
		  ;; Itemize headline
		  "\\item"
		  (and full-text
		       (string-match-p "\\`[ \t]*\\[" full-text)
		       "\\relax")
		  " " full-text "\n"
		  headline-label
		  pre-blanks
		  contents)))
	    ;; If headline is not the last sibling simply return
	    ;; LOW-LEVEL-BODY.  Otherwise, also close the list, before
	    ;; any blank line.
	    (if (not (org-export-last-sibling-p headline info)) low-level-body
	      (replace-regexp-in-string
	       "[ \t\n]*\\'"
	       (format "\n\\\\end{%s}" (if numberedp 'enumerate 'itemize))
	       low-level-body)))
	;; This is a standard headline.  Export it as a section.  Add
	;; an alternative heading when possible, and when this is not
	;; identical to the usual heading.
	(let ((opt-title
	       (funcall (plist-get info :latex-format-headline-function)
			todo todo-type priority
			(org-export-data-with-backend
			 (org-export-get-alt-title headline info)
			 section-back-end info)
			(and (eq (plist-get info :with-tags) t) tags)
			info))
	      ;; Maybe end local TOC (see `org-latex-keyword').
	      (contents
	       (concat
		contents
		(let ((case-fold-search t)
		      (section
		       (let ((first (car (org-element-contents headline))))
			 (and (eq (org-element-type first) 'section) first))))
		  (org-element-map section 'keyword
		    (lambda (k)
		      (and (equal (org-element-property :key k) "TOC")
			   (let ((v (org-element-property :value k)))
			     (and (string-match-p "\\<headlines\\>" v)
				  (string-match-p "\\<local\\>" v)
				  (format "\\stopcontents[level-%d]" level)))))
		    info t)))))
	  (if (and opt-title
		   (not (equal opt-title full-text))
		   (string-match "\\`\\\\\\(.+?\\){" section-fmt))
	      (format (replace-match "\\1[%s]" nil nil section-fmt 1)
		      ;; Replace square brackets with parenthesis
		      ;; since square brackets are not supported in
		      ;; optional arguments.
		      (replace-regexp-in-string
		       "\\[" "(" (replace-regexp-in-string "\\]" ")" opt-title))
		      full-text
		      (concat headline-label pre-blanks contents))
	    ;; Impossible to add an alternative heading.  Fallback to
	    ;; regular sectioning format string.
	    (format section-fmt full-text
		    (concat headline-label pre-blanks contents))))))))

;;; Center block
(defun org-plaintex-center-block (center-block contents info)
  "Transcode a CENTER-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the center block.  INFO is a plist
holding contextual information."
  (org-latex--wrap-label
   center-block (format "\\begincenter\n%s\\endcenter" contents) info))

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
