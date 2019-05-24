;;; ox-tex.el --- Plain TeX Back-End for Org Export Engine -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'ox-latex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;			      Alignment                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make halign the default alignment environment
(defcustom org-tex-default-table-environment "halign"
  "Default environment used to build tables."
  :group 'org-export-tex
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)

;;; Convert table to halign or matrix
(defun org-tex-table (table contents info)
  "Transcode a TABLE element from Org to plain TeX.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
  (if (eq (org-element-property :type table) 'table.el)
      ;; "table.el" table.  Convert it using appropriate tools.
      (org-latex--table.el-table table info)
    (let ((type (or (org-export-read-attribute :attr_latex table :environment)
		    (plist-get info :latex-default-table-environment))))
      (cond
       ;; matrix
       ((string= type "matrix")
	(org-tex--math-table table info))
       ;; halign
       (t (org-tex--org-table table contents info))))))

(defun org-tex-matrices (matrices contents _info)
  "Transcode a MATRICES element from Org to plain TeX.
CONTENTS is a string.  INFO is a plist used as a communication
channel."
  (format (cl-case (org-element-property :markup matrices)
	    (inline "$%s$")
	    (t "$$\n%s$$"))
	  contents))

(defun org-tex--math-table (table info)
  "Return appropriate plain TeX code for a matrix.

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
		"\\cr"
		"\n")))
	   (org-element-map table 'table-row #'identity info) "")))
    (format "\\left(\\matrix{\n%s}\\right)" contents)))

;; Plain TeX style table alignment
(defun org-tex--align-string (table info &optional math?)
  "Return an appropriate plain TeX alignment string.
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
	      (push (if math? "\\enskip\\hfil#\\hfil\\enskip" ; center cells in matrices
		      (cl-case (org-export-table-cell-alignment cell info)
			(left "\\enskip#\\hfil\\enskip")
			(right "\\enskip\\hfil#\\enskip")
			(center "\\enskip\\hfil#\\hfil\\enskip")))
		    align)
	      (when (memq 'right borders) (push "|" align))))
	  info)
	(string-join align " & "))))

;; https://stackoverflow.com/a/11848341
(defun how-many-str (regexp str)
  "Return the number of occurrences of a substring in a string.

REGEXP is a string of interest and STR is a substring to be counted."

  (loop with start = 0
        for count from 0
        while (string-match regexp str start)
        do (setq start (match-end 0))
        finally return count))

(defun org-tex--org-table (table contents info)
  "Return appropriate plain TeX code for an Org table.

TABLE is the table type element to transcode.  CONTENTS is its
contents, as a string.  INFO is a plist used as a communication
channel.

This function assumes TABLE has `org' as its `:type' property and
`table' as its `:mode' attribute."
  (let* ((alignment (org-tex--align-string table info))
	 (label (org-export-get-reference table info))
	 (caption (org-export-data (org-export-get-caption table) info)))
    (format "\\advance \\tablecount by 1
$$\\vbox{
\\label{tab:%s}\\halign{\\offinterlineskip
\\tstrut%s\\cr
\\multispan %s \\hfil Table \\the\\tablecount %s \\hfil \\cr
%s}}$$"
	    label
    	    alignment
	    (how-many-str "#" alignment)
	    (if (string= caption "") "" (concat ": " caption))
    	    contents)))

;; Concatenate rows with rules + \cr at the end of each line
(defun org-tex-table-row (table-row contents info)
  "Transcode a TABLE-ROW element from Org to plain TeX.
CONTENTS is the contents of the row.  INFO is a plist used as
a communication channel."
  (if (eq (org-element-property :type table-row) 'rule)
      (cond
       ((not (org-export-get-previous-element table-row info)) "\\toprule")
       ((not (org-export-get-next-element table-row info)) "\\bottomrule")
       (t "\\midrule"))
    (concat
     contents "\\cr\n")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;				Blocks                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Center
(defun org-tex-center-block (center-block contents info)
  "Transcode a CENTER-BLOCK element from Org to plain TeX.
CONTENTS holds the contents of the center block.  INFO is a plist
holding contextual information."
  (format "\\begincenter\n%s\\endcenter" contents))

;;; Example
(defun org-tex-example-block (example-block _contents info)
  "Transcode an EXAMPLE-BLOCK element from Org to plain TeX.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "\\verbatim| %s|\\endverbatim"
	  (org-export-format-code-default example-block info)))

;;; Quote
(defun org-tex-quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to plain TeX.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (format "\\beginquote\n%s\\endquote" contents))

;;;; Source
(defun org-tex-src-block (src-block _contents info)
  "Transcode a SRC-BLOCK element from Org to plain TeX.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (format "\\verbatim| %s|endverbatim"
	  (org-export-format-code-default src-block info)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;				Export                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(org-export-define-derived-backend 'tex 'latex
  :menu-entry
  '(?l 1
       ((?T "As plain TeX buffer" org-tex-export-as-tex)
	(?t "As plain TeX file" org-tex-export-to-tex)))
  :options-alist
  '((:abstract "ABSTRACT" nil nil parse)
    (:macros "MACROS" nil nil parse)
    (:tex-class "TEX_CLASS" nil org-tex-default-class t)
    (:tex-classes nil nil org-tex-classes)
    (:tex-text-markup-alist nil nil org-tex-text-markup-alist))
  :translate-alist
  '((bold . org-tex-bold)
    (center-block . org-tex-center-block)
    (code . org-tex-code)
    (example-block . org-tex-example-block)
    (footnote-reference . org-tex-footnote-reference)
    (headline . org-tex-headline)
    (italic . org-tex-italic)
    (item . org-tex-item)
    (latex-math-block . org-tex-math-block)
    (latex-matrices . org-tex-matrices)
    (latex-default-table-environment org-tex-default-table-environment)
    (plain-list . org-tex-plain-list)
    (quote-block . org-tex-quote-block)
    (src-block . org-tex-src-block)
    (subscript . org-tex-subscript)
    (superscript . org-tex-superscript)
    (underline . org-tex-underline)
    (table . org-tex-table)
    (table-row . org-tex-table-row)
    (template . org-tex-template)))

(defcustom org-tex-classes
  '(("article"
     "[NO-DEFAULT-PACKAGES]
      [NO-PACKAGES]"
     ("\n\\section %s" . "\n\\section %s")
     ("\n\\subsection %s" . "\n\\subsection %s")
     ("\n\\subsubsection %s" . "\n\\subsubsection %s"))
    ("book"
     "[NO-DEFAULT-PACKAGES]
      [NO-PACKAGES]"
     ("\n\\chapter %s" . "\n\\chapter %s")
     ("\n\\section %s" . "\n\\section %s")
     ("\n\\subsection %s" . "\n\\subsection %s")
     ("\n\\subsubsection %s" . "\n\\subsubsection %s")))
  "Alist of plain TeX classes and associated header and structure.
If #+TEX_CLASS is set in the buffer, use its value and the
associated information."
  :group 'org-export-tex
  :type '(repeat
	  (list (string :tag "LaTeX class")
		(string :tag "LaTeX header")
		(repeat :tag "Levels" :inline t
			(choice
			 (cons :tag "Heading"
			       (string :tag "  numbered")
			       (string :tag "unnumbered"))
			 (list :tag "Environment"
			       (string :tag "Opening   (numbered)")
			       (string :tag "Closing   (numbered)")
			       (string :tag "Opening (unnumbered)")
			       (string :tag "Closing (unnumbered)"))
			 (function :tag "Hook computing sectioning"))))))

(defgroup org-export-tex nil
  "Options specific for using the tex class in plain TeX export."
  :tag "Org tex"
  :group 'org-export
  :version "25.3")

(defcustom org-tex-default-class "article"
  "The default plain TeX class."
  :group 'org-export-latex
  :type '(string :tag "LaTeX class"))

(defun download-macro (url)
  "Insert a file from a URL."
  (with-current-buffer
      (url-retrieve-synchronously url)
    (goto-char (point-min))
    (re-search-forward "^$")
    (delete-region (point) (point-min))
    (kill-whole-line)
    (buffer-string)))

(defun org-tex--format-spec (info)
  "Create a format-spec for document meta-data.
INFO is a plist used as a communication channel."
    `((?a . ,(org-export-data (plist-get info :author) info))
      (?A . ,(org-export-data (org-tex--wrap-tex-math-block
			       (plist-get info :abstract) info)
			      info))
      (?t . ,(org-export-data (plist-get info :title) info))
      (?k . ,(org-export-data (org-tex--wrap-tex-math-block
			       (plist-get info :keywords) info)
			      info))
      (?d . ,(org-export-data (org-tex--wrap-tex-math-block
			       (plist-get info :description) info)
			      info))
      (?c . ,(plist-get info :creator))
      (?D . ,(org-export-get-date info))))

(defun org-tex--wrap-tex-math-block (data info)
  "Merge contiguous math objects in a pseudo-object container.
DATA is a parse tree or a secondary string.  INFO is a plist
containing export options.  Modify DATA by side-effect and return it."
  (let ((valid-object-p
	 ;; Non-nil when OBJECT can be added to a latex math block.
	 (lambda (object)
	   (pcase (org-element-type object)
	     (`entity (org-element-property :latex-math-p object))
	     (`latex-fragment
	      (let ((value (org-element-property :value object)))
		(or (string-prefix-p "\\(" value)
		    (string-match-p "\\`\\$[^$]" value))))))))
    (org-element-map data '(entity latex-fragment)
      (lambda (object)
	;; Skip objects already wrapped.
	(when (and (not (eq (org-element-type
			     (org-element-property :parent object))
			    'latex-math-block))
		   (funcall valid-object-p object))
	  (let ((math-block (list 'latex-math-block nil))
		(next-elements (org-export-get-next-element object info t))
		(last object))
	    ;; Wrap MATH-BLOCK around OBJECT in DATA.
	    (org-element-insert-before math-block object)
	    (org-element-extract-element object)
	    (org-element-adopt-elements math-block object)
	    (when (zerop (or (org-element-property :post-blank object) 0))
	      ;; MATH-BLOCK swallows consecutive math objects.
	      (catch 'exit
		(dolist (next next-elements)
		  (unless (funcall valid-object-p next) (throw 'exit nil))
		  (org-element-extract-element next)
		  (org-element-adopt-elements math-block next)
		  ;; Eschew the case: \beta$x$ -> \(\betax\).
		  (org-element-put-property last :post-blank 1)
		  (setq last next)
		  (when (> (or (org-element-property :post-blank next) 0) 0)
		    (throw 'exit nil)))))
	    (org-element-put-property
	     math-block :post-blank (org-element-property :post-blank last)))))
      info nil '(latex-math-block) t)
    ;; Return updated DATA.
    data))

;; Plain TeX file template
(defun org-tex-template (contents info)
  "Return complete document string after plain TeX conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let ((title (org-export-data (plist-get info :title) info))
        (spec (org-tex--format-spec info)))
    (concat

     ;; If a macros file is specified, \input that
     ;; Otherwise download macros from github
     (cond
      ((plist-get info :macros)
       (format "\\input %s\n"
	       (org-export-data (plist-get info :macros) info)))
      ((string= (plist-get info :tex-class) "article")
       (download-macro "https://raw.githubusercontent.com/paulserafini/ox-tex/master/article.tex"))
      (t (download-macro "https://raw.githubusercontent.com/paulserafini/ox-tex/master/book.tex")))

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

     ;; Add abstract if defined
     (let ((abstract (plist-get info :abstract)))
       (when abstract
         (format "\\abstract{%s}\n"
		 (org-export-data (plist-get info :abstract) info))))

     ;; Add keywords if defined
     (let ((keywords (plist-get info :keywords)))
       (when keywords
         (format "\\keywords{Key words: \\it %s}\n"
		 (org-export-data (plist-get info :keywords) info))))

     (if (string= (plist-get info :tex-class) "book")
	 (format "\\maketitle\n"))

     (if (and (string= (plist-get info :tex-class) "book")
	      (plist-get info :abstract))
	 (format "\\makeabstract\n"))

     (if (string= (plist-get info :tex-class) "book")
	 (format "\\readtocfile\n"))


     "\\def\\printlistinglineno{\\llap{\\sevenrm\\the\\lineno\\quad \\vrule height 8pt width0.7pt depth2pt\\quad}\\offinterlineskip}\n"

     ;; Document's body.
     contents

     ;; Document end.
     "\n\\bye")))

;;; Export to a buffer
(defun org-tex-export-as-tex
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a plain TeX buffer."
  (interactive)
  (org-export-to-buffer 'tex "*Org PLAIN TEX Export*"
    async subtreep visible-only body-only ext-plist (lambda () (plain-TeX-mode))))

;;; Export to a .tex file
(defun org-tex-export-to-tex
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a plain TeX file."
  (interactive)
  (let ((file (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'tex file
      async subtreep visible-only body-only ext-plist)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;			     Enumeration                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Plain List
(defun org-tex-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to plain TeX.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  (let* ((type (org-element-property :type plain-list))
	 (attr (org-export-read-attribute :attr_latex plain-list))
	 (list-type (let ((env (plist-get attr :environment)))
		       (cond (env (format "%s" env))
			     ((eq type 'ordered) "numberedlist")
			     ((eq type 'descriptive) "description")
			     (t "orderedlist")))))
     (format "\\%s%s\n%s\\end%s"
	     list-type
	     (or (plist-get attr :options) "")
	     contents
	     list-type)))

;; List counters
(defun org-tex-item (item contents info)
  "Transcode an ITEM element from Org to plain TeX.
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

(defun org-tex-headline (headline contents info)
  "Transcode a HEADLINE element from Org to plain TeX.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((class (plist-get info :tex-class))
	   (level (org-export-get-relative-level headline info))
	   (numberedp (org-export-numbered-headline-p headline info))
	   (class-sectioning (assoc class (plist-get info :tex-classes)))
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
	   (text (org-export-data (org-element-property :title headline) info)))
      (if section-fmt
	  (concat (format section-fmt text "") "\n" contents)
	  (concat "{\\bf" text "}\n\n" contents)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;				Markup                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Bold
(defun org-tex-bold (_bold contents info)
  "Transcode BOLD from Org to plain TeX.
CONTENTS is the text with bold markup.  INFO is a plist holding
contextual information."
  (org-tex--text-markup contents 'bold info))

;;; Code
(defun org-tex-code (code _contents info)
  "Transcode a CODE object from Org to plain TeX.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (org-tex--text-markup (org-element-property :value code) 'code info))

;;; Footnote
(defun org-tex-footnote-reference (footnote-reference _contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to plain TeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((def (org-export-get-footnote-definition footnote-reference info)))
    (format "\\numberedfootnote{%s}" (org-trim (org-export-data def info)))))

;;; Inline math
(defun org-tex-math-block (_math-block contents _info)
  "Transcode a MATH-BLOCK object from Org to plain TeX.
CONTENTS is a string.  INFO is a plist used as a communication
channel."
  (when (org-string-nw-p contents)
    (format "$%s$" (org-trim contents))))

;;; Italic
(defun org-tex-italic (_italic contents info)
  "Transcode ITALIC from Org to plain TeX.
CONTENTS is the text with italic markup.  INFO is a plist holding
contextual information."
  (org-tex--text-markup contents 'italic info))

;;; Subscript
(defun org-tex-subscript (_subscript contents _info)
  "Transcode a SUBSCRIPT object from Org to plain TeX.
CONTENTS is the contents of the object."
  (format "_{%s}" contents))

;;; Superscript
(defun org-tex-superscript (_superscript contents _info)
  "Transcode a SUPERSCRIPT object from Org to plain TeX.
CONTENTS is the contents of the object."
  (format "^{%s}" contents))

;;; Underline
(defun org-tex-underline (_underline contents info)
  "Transcode UNDERLINE from Org to plain TeX.
CONTENTS is the text with underline markup.  INFO is a plist
holding contextual information."
  (org-tex--text-markup contents 'underline info))

;;; Markup
(defcustom org-tex-text-markup-alist '((bold . "{\\bf %s}")
				       (code . "{\\tt %s}")
				       (italic . "{\\it %s}")
				       (strike-through . "\\sout{%s}")
				       (underline . "$\\underline{\\rm %s}$")
				       (verbatim . "{\\tt %s}"))
  "Alist of plain TeX expressions to convert text markup."
  :group 'org-export-tex
  :version "26.1"
  :package-version '(Org . "8.3")
  :type 'alist
  :options '(bold code italic strike-through underline verbatim))

;;; Apply text markup
(defun org-tex--text-markup (text markup info)
  "Format TEXT depending on MARKUP text markup.
INFO is a plist used as a communication channel.  See
`org-tex-text-markup-alist' for details."
  (let ((fmt (cdr (assq markup (plist-get info :tex-text-markup-alist)))))
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
			((equal m "\\") "$\\backslash$")
			((equal m "~") "$\\sim$")
			((equal m "^") "$\\hat{}$")
			(t (org-latex--protect-text m))))
		text nil t)))
      ;; Else use format string.
      (t (format fmt text)))))


(provide 'ox-tex)
;;; ox-tex ends here
