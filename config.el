(setq doom-font (font-spec :family "Iosevka" :size 16)
      doom-big-font (font-spec :family "Iosevka" :size 30))
(setq global-prettify-symbols-mode t)
(setq fancy-splash-image "~/Pictures/china_h+s-removebg-preview.png")

(setq display-line-numbers-type 'relative)

(defun jan/org-capture-to-project-headling ()
    (let ((projects
            (org-map-entries `(lambda () (nth 4 (org-heading-components)))
                            "+LEVEL=3" '("~/OrgFiles/Agendas/College.org"))))
        (setq choice (ivy-completing-read "Project:" projects nil t nil nil))
        (org-capture-set-target-location (list 'file+headline "~/OrgFiles/Agendas/College.org" choice))
))

(after! org
  (setq org-startup-folded t
        org-pretty-entries t
        org-ellipsis " ▼"
        org-todo-keywords'((sequence "TODO(t)" "HOMEWORK(h)" "PROJECT(p)" "READING(r)" "WORK(w)" "ORGANIZING(o)" "NOTE(n)" "|" "DONE(d!)" "CANCELLED(c)")
                           (sequence "STARTED(s)" "HALFWAY(h!)" "NOTETAKEN(N)" "|" "COMPLETED(c)"))
        org-directory "~/OrgFiles"
        org-agenda-files '("Agendas/College.org"                             ;; Link to all my agenda files
                           "Agendas/Life.org"
                           "Agendas/Marx.org"
                           "Agendas/Work.org")
        org-refile-targets '(("~/OrgFiles/Archives.org" :maxlevel . 3)       ;; Set new refile targets
                             ("~/OrgFiles/Tasks.org" :maxlevel . 1))
        org-structure-template-alist '(("a" . "export ascii")                ;; Add extra structure templates
                                       ("c" . "src cpp")
                                       ("C" . "comment")
                                       ("e" . "example")
                                       ("E" . "export")
                                       ("h" . "export html")
                                       ("l" . "src emacs-lisp")
                                       ("q" . "quote")
                                       ("s" . "src")
                                       ("p" . "src python")
                                       ("v" . "verse"))
        org-capture-templates `(("n" "Notes for Assignments") ;; Create a capture template for when I finish with an assignment and want to note where I left off
                                    ("na" "Algorithms and Data Structures" entry (file+olp "~/OrgFiles/Agendas/College.org" "Algorithms and Data Structures" "Assignment Notes")
                                    "* NOTETAKEN %?\n %U\n %a\n %i" )
                                    ("nd" "Data Mining" entry (file+olp "~/OrgFiles/Agendas/College.org" "Data Mining" "Assignment Notes")
                                    "* NOTETAKEN %?\n %U\n %a\n %i" )
                                    ("no" "Operating Systems" entry (file+olp "~/OrgFiles/Agendas/College.org" "Operating Systems" "Assignment Notes")
                                    "* NOTETAKEN %?\n %U\n %a\n %i" )
                                    ("nf" "Functional Programming" entry (file+olp "~/OrgFiles/Agendas/College.org" "Functional Programming" "Assignment Notes")
                                    "* NOTETAKEN %?\n %U\n %a\n %i" )
                                    ("nc" "Intro to Crypto" entry (file+olp "~/OrgFiles/Agendas/College.org" "Intro to Crypto" "Assignment Notes")
                                    "* NOTETAKEN %?\n %U\n %a\n %i" )
                                )
))

(custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 1.35))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.15))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.125))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
)

(after! org
  (setq org-latex-compiler "xelatex"
        org-latex-listings t
        org-latex-default-packages-alist
      '(("" "graphicx" t)
        ("" "grffile" t)
        ("" "longtable" nil)
        ("" "wrapfig" nil)
        ("" "rotating" nil)
        ("normalem" "ulem" t)
        ("" "amsmath" t)
        ("" "textcomp" t)
        ("" "amssymb" t)
        ("" "capt-of" nil)
        ("" "hyperref" nil))
        org-latex-classes
        '(("article"
"\\RequirePackage{fix-cm}
\\PassOptionsToPackage{svgnames}{xcolor}
\\documentclass[11pt]{article}
\\usepackage{fontspec}
\\setmainfont{Linux Libertine}
\\setsansfont[Scale=MatchLowercase]{Raleway}
\\setmonofont[Scale=MatchLowercase]{Iosevka}
\\usepackage{sectsty}
\\allsectionsfont{\\sffamily}
\\usepackage{enumitem}
\\setlist[description]{style=unboxed,font=\\sffamily\\bfseries}
\\usepackage{listings}
\\lstset{frame=single,aboveskip=1em,
	framesep=.5em,backgroundcolor=\\color{AliceBlue},
	rulecolor=\\color{LightSteelBlue},framerule=1pt}
\\usepackage{xcolor}
\\newcommand\\basicdefault[1]{\\scriptsize\\color{Black}\\ttfamily#1}
\\lstset{basicstyle=\\basicdefault{\\spaceskip1em}}
\\lstset{literate=
	    {§}{{\\S}}1
	    {©}{{\\raisebox{.125ex}{\\copyright}\\enspace}}1
	    {«}{{\\guillemotleft}}1
	    {»}{{\\guillemotright}}1
	    {Á}{{\\'A}}1
	    {Ä}{{\\\"A}}1
	    {É}{{\\'E}}1
	    {Í}{{\\'I}}1
	    {Ó}{{\\'O}}1
	    {Ö}{{\\\"O}}1
	    {Ú}{{\\'U}}1
	    {Ü}{{\\\"U}}1
	    {ß}{{\\ss}}2
	    {à}{{\\`a}}1
	    {á}{{\\'a}}1
	    {ä}{{\\\"a}}1
	    {é}{{\\'e}}1
	    {í}{{\\'i}}1
	    {ó}{{\\'o}}1
	    {ö}{{\\\"o}}1
	    {ú}{{\\'u}}1
	    {ü}{{\\\"u}}1
	    {¹}{{\\textsuperscript1}}1
            {²}{{\\textsuperscript2}}1
            {³}{{\\textsuperscript3}}1
	    {ı}{{\\i}}1
	    {—}{{---}}1
	    {’}{{'}}1
	    {…}{{\\dots}}1
            {⮠}{{$\\hookleftarrow$}}1
	    {␣}{{\\textvisiblespace}}1,
	    keywordstyle=\\color{DarkGreen}\\bfseries,
	    identifierstyle=\\color{DarkRed},
	    commentstyle=\\color{Gray}\\upshape,
	    stringstyle=\\color{DarkBlue}\\upshape,
	    emphstyle=\\color{Chocolate}\\upshape,
	    showstringspaces=false,
	    columns=fullflexible,
	    keepspaces=true}
\\usepackage[a4paper,margin=1in,left=1.5in]{geometry}
\\usepackage{parskip}
\\makeatletter
\\renewcommand{\\maketitle}{%
  \\begingroup\\parindent0pt
  \\sffamily
  \\Huge{\\bfseries\\@title}\\par\\bigskip
  \\LARGE{\\bfseries\\@author}\\par\\medskip
  \\normalsize\\@date\\par\\bigskip
  \\endgroup\\@afterindentfalse\\@afterheading}
\\makeatother
[DEFAULT-PACKAGES]
\\hypersetup{linkcolor=Blue,urlcolor=DarkBlue,
  citecolor=DarkRed,colorlinks=true}
\\AtBeginDocument{\\renewcommand{\\UrlFont}{\\ttfamily}}
[PACKAGES]
[EXTRA]"
("\\section{%s}" . "\\section*{%s}")
("\\subsection{%s}" . "\\subsection*{%s}")
("\\subsubsection{%s}" . "\\subsubsection*{%s}")
("\\paragraph{%s}" . "\\paragraph*{%s}")
("\\subparagraph{%s}" . "\\subparagraph*{%s}"))

("report" "\\documentclass[11pt]{report}"
("\\part{%s}" . "\\part*{%s}")
("\\chapter{%s}" . "\\chapter*{%s}")
("\\section{%s}" . "\\section*{%s}")
("\\subsection{%s}" . "\\subsection*{%s}")
("\\subsubsection{%s}" . "\\subsubsection*{%s}"))

("book" "\\documentclass[11pt]{book}"
("\\part{%s}" . "\\part*{%s}")
("\\chapter{%s}" . "\\chapter*{%s}")
("\\section{%s}" . "\\section*{%s}")
("\\subsection{%s}" . "\\subsection*{%s}")
("\\subsubsection{%s}" . "\\subsubsection*{%s}")))))

(setq org-roam-directory (file-truename "~/OrgFiles/College"))

(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))
