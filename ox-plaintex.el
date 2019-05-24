;;; ox-plaintex.el --- Plain TeX Back-End for Org Export Engine -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'ox-latex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;			      Alignment                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make halign the default alignment environment
(defcustom org-plaintex-default-table-environment "halign"
  "Default environment used to build tables."
  :group 'org-export-plaintex
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)

;;; Convert table to halign or matrix
(defun org-plaintex-table (table contents info)
  "Transcode a TABLE element from Org to LaTeX.
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
	(org-plaintex--math-table table info))
       ;; halign
       (t (org-plaintex--org-table table contents info))))))

(defun org-plaintex-matrices (matrices contents _info)
  "Transcode a MATRICES element from Org to LaTeX.
CONTENTS is a string.  INFO is a plist used as a communication
channel."
  (format (cl-case (org-element-property :markup matrices)
	    (inline "$%s$")
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
		"\\cr"
		"\n")))
	   (org-element-map table 'table-row #'identity info) "")))
    (format "\\left(\\matrix{\n%s}\\right)" contents)))

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
  (loop with start = 0
        for count from 0
        while (string-match regexp str start)
        do (setq start (match-end 0))
        finally return count))

(defun org-plaintex--org-table (table contents info)
  "Return appropriate LaTeX code for an Org table.

TABLE is the table type element to transcode.  CONTENTS is its
contents, as a string.  INFO is a plist used as a communication
channel.

This function assumes TABLE has `org' as its `:type' property and
`table' as its `:mode' attribute."
  (let* ((alignment (org-plaintex--align-string table info))
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
(defun org-plaintex-table-row (table-row contents info)
  "Transcode a TABLE-ROW element from Org to LaTeX.
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
(defun org-plaintex-center-block (center-block contents info)
  "Transcode a CENTER-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the center block.  INFO is a plist
holding contextual information."
  (format "\\begincenter\n%s\\endcenter" contents))

;;; Example
(defun org-plaintex-example-block (example-block _contents info)
  "Transcode an EXAMPLE-BLOCK element from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "\\verbatim| %s|\\endverbatim"
	  (org-export-format-code-default example-block info)))

;;; Quote
(defun org-plaintex-quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (format "\\beginquote\n%s\\endquote" contents))

;;;; Source
(defun org-plaintex-src-block (src-block _contents info)
  "Transcode a SRC-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (format "\\verbatim| %s|endverbatim"
	  (org-export-format-code-default src-block info)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;				Export                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(org-export-define-derived-backend 'plaintex 'latex
  :menu-entry
  '(?l 1
       ((?T "As plain TeX buffer" org-plaintex-export-as-latex)
	(?t "As plain TeX file" org-plaintex-export-to-latex)))
  :options-alist
  '((:plaintex-class "LATEX_CLASS" nil org-plaintex-default-class t)
    (:plaintex-classes nil nil org-plaintex-classes)
    (:macros "MACROS" nil nil parse)
    (:abstract "ABSTRACT" nil nil parse)
    (:plaintex-text-markup-alist nil nil org-plaintex-text-markup-alist))
  :translate-alist
  '((bold . org-plaintex-bold)
    (center-block . org-plaintex-center-block)
    (code . org-plaintex-code)
    (example-block . org-plaintex-example-block)
    (footnote-reference . org-plaintex-footnote-reference)
    (headline . org-plaintex-headline)
    (italic . org-plaintex-italic)
    (item . org-plaintex-item)
    (latex-math-block . org-plaintex-math-block)
    (latex-matrices . org-plaintex-matrices)
    (latex-default-table-environment org-plaintex-default-table-environment)
    (plain-list . org-plaintex-plain-list)
    (quote-block . org-plaintex-quote-block)
    (src-block . org-plaintex-src-block)
    (subscript . org-plaintex-subscript)
    (superscript . org-plaintex-superscript)
    (underline . org-plaintex-underline)
    (table . org-plaintex-table)
    (table-row . org-plaintex-table-row)
    (template . org-plaintex-template)))

(defcustom org-plaintex-classes
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
  "Alist of LaTeX classes and associated header and structure.
If #+LATEX_CLASS is set in the buffer, use its value and the
associated information."
  :group 'org-export-plaintex
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

(defgroup org-export-plaintex nil
  "Options specific for using the plaintex class in LaTeX export."
  :tag "Org plaintex"
  :group 'org-export
  :version "25.3")

(defcustom org-plaintex-default-class "article"
  "The default LaTeX class."
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

(defun org-plaintex--format-spec (info)
  "Create a format-spec for document meta-data.
INFO is a plist used as a communication channel."
  (let ((language (let ((lang (plist-get info :language)))
		    (or (cdr (assoc-string lang org-latex-babel-language-alist t))
			(nth 1 (assoc-string lang org-latex-polyglossia-language-alist t))
			lang))))
    `((?a . ,(org-export-data (plist-get info :author) info))
      (?A . ,(org-export-data (org-latex--wrap-latex-math-block
			       (plist-get info :abstract) info)
			      info))
      (?t . ,(org-export-data (plist-get info :title) info))
      (?k . ,(org-export-data (org-latex--wrap-latex-math-block
			       (plist-get info :keywords) info)
			      info))
      (?d . ,(org-export-data (org-latex--wrap-latex-math-block
			       (plist-get info :description) info)
			      info))
      (?c . ,(plist-get info :creator))
      (?l . ,language)
      (?L . ,(capitalize language))
      (?D . ,(org-export-get-date info)))))


;; Plain TeX file template
(defun org-plaintex-template (contents info)
  "Return complete document string after LaTeX conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let ((title (org-export-data (plist-get info :title) info))
        (spec (org-plaintex--format-spec info)))
    (concat

     ;; If a macros file is specified, \input that
     ;; Otherwise download macros from github
     (cond
      ((plist-get info :macros)
       (format "\\input %s\n"
	       (org-export-data (plist-get info :macros) info)))
      ((string= (plist-get info :latex-class) "article")
       (download-macro "https://raw.githubusercontent.com/paulserafini/ox-plaintex/master/article.tex"))
      (t (download-macro "https://raw.githubusercontent.com/paulserafini/ox-plaintex/master/book.tex")))

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

     (if (string= (plist-get info :latex-class) "book")
	 (format "\\maketitle\n"))

     (if (and (string= (plist-get info :latex-class) "book")
	      (plist-get info :abstract))
	 (format "\\makeabstract\n"))

     (if (string= (plist-get info :latex-class) "book")
	 (format "\\readtocfile\n"))

     ;; Document's body.
     contents

     ;; Document end.
     "\\bye")))

;;; Export to a buffer
(defun org-plaintex-export-as-latex
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a plain TeX buffer."
  (interactive)
  (org-export-to-buffer 'plaintex "*Org PLAIN TEX Export*"
    async subtreep visible-only body-only ext-plist (lambda () (plain-TeX-mode))))

;;; Export to a .tex file
(defun org-plaintex-export-to-latex
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a plain TeX file."
  (interactive)
  (let ((file (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'plaintex file
      async subtreep visible-only body-only ext-plist)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;			     Enumeration                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun org-plaintex-headline (headline contents info)
  "Transcode a HEADLINE element from Org to LaTeX.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((class (plist-get info :latex-class))
	   (level (org-export-get-relative-level headline info))
	   (numberedp (org-export-numbered-headline-p headline info))
	   (class-sectioning (assoc class (plist-get info :plaintex-classes)))
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
(defun org-plaintex-bold (_bold contents info)
  "Transcode BOLD from Org to LaTeX.
CONTENTS is the text with bold markup.  INFO is a plist holding
contextual information."
  (org-plaintex--text-markup contents 'bold info))

;;; Code
(defun org-plaintex-code (code _contents info)
  "Transcode a CODE object from Org to LaTeX.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (org-plaintex--text-markup (org-element-property :value code) 'code info))

;;; Footnote
(defun org-plaintex-footnote-reference (footnote-reference _contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((def (org-export-get-footnote-definition footnote-reference info)))
    (format "\\numberedfootnote{%s}" (org-trim (org-export-data def info)))))

;;; Inline math
(defun org-plaintex-math-block (_math-block contents _info)
  "Transcode a MATH-BLOCK object from Org to LaTeX.
CONTENTS is a string.  INFO is a plist used as a communication
channel."
  (when (org-string-nw-p contents)
    (format "$%s$" (org-trim contents))))

;;; Italic
(defun org-plaintex-italic (_italic contents info)
  "Transcode ITALIC from Org to LaTeX.
CONTENTS is the text with italic markup.  INFO is a plist holding
contextual information."
  (org-plaintex--text-markup contents 'italic info))

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

;;; Underline
(defun org-plaintex-underline (_underline contents info)
  "Transcode UNDERLINE from Org to LaTeX.
CONTENTS is the text with underline markup.  INFO is a plist
holding contextual information."
  (org-plaintex--text-markup contents 'underline info))

;;; Markup
(defcustom org-plaintex-text-markup-alist '((bold . "{\\bf %s}")
					    (code . "{\\tt %s}")
					    (italic . "{\\it %s}")
					    (strike-through . "\\sout{%s}")
					    (underline . "$\\underline{\\rm %s}$")
					    (verbatim . "{\\tt %s}"))
  "Alist of LaTeX expressions to convert text markup."
  :group 'org-export-plaintex
  :version "26.1"
  :package-version '(Org . "8.3")
  :type 'alist
  :options '(bold code italic strike-through underline verbatim))

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
			((equal m "\\") "$\\backslash$")
			((equal m "~") "$\\sim$")
			((equal m "^") "$\\hat{}$")
			(t (org-latex--protect-text m))))
		text nil t)))
      ;; Else use format string.
      (t (format fmt text)))))




(provide 'ox-plaintex)
;;; ox-plaintex ends here
