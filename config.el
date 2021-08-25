(setq doom-font (font-spec :family "Iosevka" :size 16)
      doom-big-font (font-spec :family "Iosevka" :size 30))
(setq global-prettify-symbols-mode t)
(setq fancy-splash-image "~/Pictures/hammer_and_sickle6.png")

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

(package! org-gcal
  )
