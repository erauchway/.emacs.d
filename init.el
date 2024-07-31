;; attempt to setup plain vanilla emacs to my liking
;; borrowing some material from Wouter Spekkink https://github.com/WouterSpekkink/dotfiles/blob/master/emacs/init.el

;;;;;;;;;;;;;;;;;;;; 
;; packges, etc.  ;;
;;;;;;;;;;;;;;;;;;;;

;; Straight package management
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage)
  )
(setq package-enable-at-startup nil)
;; Use-package
(straight-use-package 'use-package)
(require 'use-package)
;; Undo
(use-package undo-fu
  :straight t)
(use-package undo-fu-session
  :straight t
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  :init
  (undo-fu-session-global-mode)
  )
;; Which key
(use-package which-key
  :straight t
  :config
  (which-key-mode)
  (setq which-key-popup-type 'minibuffer
	max-mini-window-height 0.5))
;; use GNU coreutils version of ls, which cooperates better with dired
(setq insert-directory-program "gls") 

;;;;;;;;;;
;; keys ;;
;;;;;;;;;;

(global-set-key [remap dabbrev-expand] 'hippie-expand)
(keymap-global-set "C-<tab>" 'hippie-expand)
(keymap-global-set "C-c f r" 'recentf-open)
(keymap-global-set "C-c n d" 'deft)
(keymap-global-set "C-c n a" 'org-agenda)
(keymap-global-set "C-c C-x C-c" 'citar-insert-citation)
(keymap-global-set "C-c l e b" 'eval-buffer)
(keymap-global-set "C-c n r d t" 'org-roam-dailies-goto-today)
(keymap-global-set "C-c n r d y" 'org-roam-dailies-goto-yesterday)
(keymap-global-set "C-c o t" 'vterm-other-window)

;;;;;;;;;;;;;;;;
;; Appearance ;;
;;;;;;;;;;;;;;;;

(use-package gruvbox-theme
  :straight t
  :config
  (load-theme 'gruvbox-dark-soft t))
;; Starting buffer
(setq inhibit-startup-message t) 
(setq initial-scratch-message nil)
(setq visible-bell t)
(setq warning-minimum-level :emergency)
(setq confirm-kill-processes nil)
(tool-bar-mode -1)
(global-visual-line-mode 1)
(set-fringe-mode 10)
(global-display-line-numbers-mode t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(use-package doom-modeline
  :straight t
  :init
  (doom-modeline-mode 1)
  (setq doom-modeline-enable-word-count t
	doom-mode-line-continuous-word-count-modes '(org-mode markdown-mode))
  )

;;;;;;;;;;;;;;;;;;;;;;;
;; startup by system ;;
;;;;;;;;;;;;;;;;;;;;;;;

(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(font . "IBM Plex Mono 14"))
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (set-face-attribute 'default t :font "IBM Plex Mono 14")
  (defun my-setup-initial-window-setup()
    "Do initial window setup"
    (interactive)
;;    (setq initial-frame-alist
;;	'((top . 0) (left . 0) (height . 65) (width . 80)))
    (set-face-attribute 'default nil :font "IBM Plex Mono 14")
    (org-agenda nil "z")
    )
  (add-hook 'emacs-startup-hook #'my-setup-initial-window-setup)
  )
(when (eq system-type 'gnu/linux)
  (add-to-list 'default-frame-alist '(font . "IBM Plex Mono 14"))
  (set-face-attribute 'default t :font "IBM Plex Mono 14")
  (defun my-setup-initial-window-setup()
    "Do initial window setup"
    (interactive)
    (setq initial-frame-alist
	'((top . 0) (left . 0) (height . 65) (width . 80)))
    (set-face-attribute 'default nil :font "IBM Plex Mono 14")
    (org-agenda nil "z")
    )
  (add-hook 'emacs-startup-hook #'my-setup-initial-window-setup)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;
;; various necessities ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; diary
(setq calendar-date-style 'iso)
(setq diary-file "~/OneDrive/org/diary")
(add-hook 'diary-list-entries-hook 'diary-sort-entries t)
;;completion
(use-package company
  :straight t
  :config
  (global-company-mode)
  (setq company-show-numbers t
	company-minimum-prefix-length 1
	company-idle-delay 1.5
	company-backends
	'((company-files
	   company-keywords
	   company-capf
	   company-yasnippet)
	  (company-abbrev company-dabbrev)))
  )
(setq hippie-expand-try-functions-list
	'(try-complete-file-name-partially
	  try-complete-file-name
	  yas-hippie-try-expand
	  try-expand-all-abbrevs
	  try-expand-dabbrev
	  try-expand-list
	  try-expand-dabbrev-all-buffers
	  try-expand-whole-kill
	  try-complete-lisp-symbol-partially
	  try-complete-lisp-symbol
	  )
	)
(use-package vertico
  :straight t
  :init (vertico-mode)
  :config
  (setq read-file-name-completion-ignore-case t
	read-buffer-completion-ignore-case t
	completion-ignore-case t
	vertico-resize nil)
  )
(use-package marginalia
  :straight t
  :after (vertico)
  :init (marginalia-mode)
  )
(use-package all-the-icons
  :straight t
  :if (display-graphic-p))
(use-package all-the-icons-completion
  :straight t
  :after marginalia
  :hook (marginalia-mode-hook . all-the-icons-completion-marginalia-setup)
  :init (all-the-icons-completion-mode))
(use-package all-the-icons-dired
  :straight t
  :after (all-the-icons)
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  )
;; delimiter highlighting
(use-package rainbow-delimiters
  :straight t
  :hook ((lisp.mode . rainbow-delimiters-mode)
	 (emacs-lisp-mode . rainbow-delimiters-mode)
	 (ess-r-mode . rainbow-delimiters-mode)
	 (inferior-ess-r-mode . rainbow-delimiters-mode)
	 (markdown-mode . rainbow-delimiters-mode))
  )
(use-package rainbow-mode
  :straight t
  )
;; terminal emulation
(use-package vterm
  :straight t
  )
;; recent files 
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 100)
(setq recentf-max-saved-items 100)
;; lsp
(use-package lsp-mode
  :straight t
  :commands lsp
  :config
  (setq gc-cons-threshold (* 100 1024 1024)
	read-process-output-max (* 1024 1024)
	treemacs-space-between-root-nodes nil
	company-idle-delay 0.0
	company-minimum-prefix-length 1
	lsp-idle-delay 0.1
	lsp-lens-enable nil) ;; attempt to address CPU intensity
  :hook ((ess-r-mode . lsp)
	 (inferior-ess-r-mode . lsp))
  )
(use-package lsp-ui
  :straight t
  :commands (lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable nil
	lsp-ui-doc-delay 0.5)
  )
(use-package lsp-treemacs
  :straight t
  :commands lsp-treemacs-errors-list
  )
;; snippets
(use-package yasnippet
  :straight t
  :hook ((lsp-mode . yas-minor-mode)))
;; deft for org search
(use-package deft
  :straight t
  :config
  (setq deft-extensions '("org" "md" "qmd" "rmd"))
  (setq deft-directory "~/OneDrive/org"
        deft-recursive t
	deft-use-filename-as-title t)
  )
;; pdf utilites
(use-package pdf-tools
  :straight t
  :hook (pdf-view-mode . (lambda () (display-line-numbers-mode 0)))
  :config
  (pdf-tools-install)
  (setq default pdf-view-display-size 'fit-width)
  :custom
  (pdf-annot-activate-created-annotations t "automatically annotate highlights")
  )

;;;;;;;;;
;; org ;;
;;;;;;;;;

;; make all org files possible refile targets
;; solution here: https://emacs.stackexchange.com/questions/64319/refile-to-non-agenda-files
(defun org-refile-candidates () (directory-files-recursively "~/OneDrive/" "^[[:alnum:]].*\\.org\\'"))
;; org and others
(use-package org
  :straight t (:type built-in)
  :hook ((org-mode . +org-enable-auto-reformat-tables-h)
	 (org-mdoe . org-indent-mode))
  :config
  (setq org-directory "~/OneDrive/org"
	org-agenda-files (quote("~/OneDrive/org/roam/daily"
				"~/OneDrive/org/roam"))
	org-refile-targets (quote (org-refile-candidates :maxlevel . 3))
	org-todo-keywords (quote ((sequence "TODO(t)"
					    "PROJ(p)"
					    "LOOP(r)"
					    "STARTED(s)"
					    "WAIT(w)"
					    "HOLD(h)"
					    "IDEA(i)"
					    "|"
					    "DONE(d)"
					    "KILL(k)")
				  (sequence "[ ](T)"
					    "[-](S)"
					    "[?](W)"
					    "|"
					    "[X](D)")
				  (sequence "|"
					    "OKAY(o)"
					    "YES(y)"
					    "NO(n)")))
	org-todo-keyword-faces (quote(("TODO" . "#F89C74")
				      ("IDEA" . "#8BE0A4")
				      ("STARTED" . "#b5b991")
				      ("DONE" . "#3d5941")
				      ("PROJ" . "#A16928"))))
  )
(use-package emacsql
  :straight t
  :defer nil)
(use-package emacsql-sqlite
  :after emacsql
  :straight t
  :defer nil)
(use-package org-roam
  :straight t
  :after org
  :hook (org-roam-mode . visual-line-mode)
  :config
  (setq org-roam-directory "~/OneDrive/org/roam/"
	org-roam-index-file "~/OneDrive/org/roam/index.org"
	org-roam-dailies-directory "~/OneDrive/org/roam/daily/"
	org-roam-dailies-capture-templates '(("d" "default" entry
         "* %?"
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n"
                            )))
	)
  (org-roam-db-autosync-mode)
  )
(setq org-agenda-window-setup (quote current-window))
(use-package org-super-agenda
  :straight t
  :after org-agenda
  :init
  (setq org-agenda-skip-scheduled-if-done t
                    org-agenda-timegrid-use-ampm t
                    org-agenda-skip-deadline-if-done t
                    org-agenda-include-diary t
                    org-agenda-include-deadlines t
                    org-deadline-warning-days 45
                    org-agenda-todo-ignore-deadlines (quote far)
                    org-agenda-todo-ignore-scheduled (quote far)
                    org-agenda-block-separator nil
                    org-agenda-compact-blocks t
                    org-agenda-start-day nil
                    org-agenda-start-with-log-mode t
                    org-agenda-span 7
                    org-agenda-view-columns-initially t
                    org-columns-default-format "%TODO %25Item %14Deadline %Clocksum_t"
                    org-agenda-start-on-weekday nil
                    org-agenda-prefix-format '(
                                               (agenda . " %s %t ")
                                               (timeline . " - ")
                                               (todo . "  %(let ((scheduled (org-get-scheduled-time (point)))) (if scheduled (format-time-string \"%Y-%m-%d %I:%M %p\" scheduled) \"\")) ")
                                               (tags . " %s %t ")
                                               (search . " - ")))
;; borrowed from Https://www.rousette.org.uk/archives/doom-emacs-tweaks-org-journal-and-org-super-agenda/
              (setq org-agenda-custom-commands
                    '(("z" "Super view"
                       (
                        (agenda "" ((org-agenda-overriding-header "")
                                    (org-super-agenda-groups
                                     '(
                                       (:name ""
                                        :time-grid t
                                        ;; :log t
                                        :date today
                                        :order 1
                                        :discard (:anything))
                                       ))))
                        (alltodo "" ((org-agenda-overriding-header "")
                                     (org-super-agenda-groups
                                      '(
                                         (:name "Due today"
                                                :deadline today
                                                :order 1)
                                        (:name "Due soon"
                                               :deadline future
                                               :scheduled future
                                               :order 0)
                                        (:name "Overdue"
                                               :deadline past
                                               :order 7)
                                        (:name "Soonest"
                                               :priority "A"
                                        )
                                        (:name "Secondary"
                                               :priority "B"
                                        )
                                        (:name "Someday"
                                               :priority "C"
                                        )
                                        ;;(:auto-group t)
                                        ;;(:auto-priority t)
                                        (:discard (:anything))
                                        ))))))))
              :config
              (org-super-agenda-mode)
	      )
;; org-noter
(use-package org-noter
  :straight t
  :after pdf-tools
  :config
  (setq org-noter-notes-search-path '("~/OneDrive/org/roam/")
	org-noter-hide-other nil
	org-noter-separate-notes-from-heading t
	org-noter-always-create-frame nil
	org-noter-auto-save-last-location t)
  )
;; org exporters
(require 'ox)
(defun sa-ignore-headline (contents backend info)
  "Ignore headlines with tag `ignoreheading'."
  (when (and (org-export-derived-backend-p backend 'latex 'odt 'pandoc 'docx)
          (string-match "\\`.*ignoreheading.*\n"
                (downcase contents)))
    (replace-match "" nil nil contents)
    )
  )
  (add-to-list 'org-export-filter-headline-functions 'sa-ignore-headline)
  ;; turns off tags for export to reveal, so "ignoreheading" doesn't appear there
  (defun turn-off-tags
    (orig &optional async subtreep visible-only body-only ext-plist)
  (let ((org-export-with-tags nil))
    (funcall orig async subtreep visible-only body-only ext-plist)))
  ;; tell org export to delete, among other temporary tex files, those marked bbl and tex
  (add-to-list 'org-latex-logfiles-extensions "bbl")
  (add-to-list 'org-latex-logfiles-extensions "tex")
;; for ox-latex
(with-eval-after-load 'ox-latex
(add-to-list 'org-latex-classes
             '("tufte-handout"
               "\\documentclass[nobib]{tufte-handout}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(add-to-list 'org-latex-classes
                '("letter"
                  "\\documentclass{letter}"
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
(add-to-list 'org-latex-classes
             '("koma-letter"
               "\\documentclass{scrlttr2}"))
(add-to-list 'org-latex-classes
                '("uc-own"
                  "\\documentclass{uc-own}"
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
(add-to-list 'org-latex-classes
             '("koma-article"
               "\\documentclass{scrartcl}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
)
;; for reveal presentations
(use-package ox-reveal
  :straight t
  :config
  (setq org-reveal-root "~/OneDrive/teaching/h174b/theme/reveal.js")
  )
;; citations
(use-package citar
  :straight t
  )
(setq org-cite-global-bibliography '("~/OneDrive/common/big_bib.bib"))
(setq org-cite-csl-styles-dir '("~/Zotero/styles"))
(setq citar-bibliography '("~/OneDrive/common/big_bib.bib"))
;; quarto essentials
(use-package quarto-mode
  :straight t
  )
(use-package markdown-mode
  :straight t
  )
(use-package ess
  :straight t
  :init
  (setq ess-style 'RStudio
	ess-use-flymake nil)
  (ess-r-mode . electric-pair-mode)
  (inferior-ess-r-mode . electric-pair-mode)
  :config
  (setq ess-ask-for-ess-director nil)
  (setq ess-R-font-lock-keywords
	'((ess-R-fl-keyword:modifiers . t)
                      (ess-R-fl-keyword:fun-defs . t)
                      (ess-R-fl-keyword:keywords . t)
                      (ess-R-fl-keyword:assign-ops . t)
                      (ess-R-fl-keyword:constants . t)
                      (ess-fl-keyword:fun-calls . t)
                      (ess-fl-keyword:numbers . t)
                      (ess-fl-keyword:operators . t)
                      (ess-fl-keyword:delimiters)
                      (ess-fl-keyword:= . t)
                      (ess-R-fl-keyword:F&T . t)
                      (ess-R-fl-keyword:%op% . t)))
  )
(use-package ess-view-data
  :straight t
  )
(use-package polymode
  :straight t
  )
(use-package poly-R
  :straight t
  :map (:localleader
	:map polymode-mode-map
	:desc "Export" "e" 'polymode-export
         :desc "Errors" "$" 'polymode-show-process-buffer
         :desc "Eval region or chunk" "v" 'polymode-eval-region-or-chunk
         :desc "Eval from top" "v" 'polymode-eval-buffer-from-beg-to-point
         :desc "Weave" "w" 'polymode-weave
         :desc "Next" "n" 'polymode-next-chunk
         :desc "Previous" "p" 'polymode-previous-chunk)
  )
(use-package poly-markdown
  :straight t
  )
;; R modes
(add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
(add-to-list 'auto-mode-alist '("\\.qmd" . poly-markdown+r-mode))
;; cribbed from doom
(defun +org-realign-table-maybe-h ()
  "Auto-align table under cursor."
  (when (and org-table-automatic-realign (org-at-table-p) org-table-may-need-update)
    (let ((pt (point))
          (inhibit-message t))
      (if org-table-may-need-update (org-table-align))
      (goto-char pt)))
  )
(defun +org-enable-auto-reformat-tables-h ()
  "Realign tables & update formulas when exiting insert mode (`evil-mode').
Meant for `org-mode-hook'."
  (when (featurep 'evil)
    (add-hook 'evil-insert-state-exit-hook #'+org-realign-table-maybe-h nil t)
    (add-hook 'evil-replace-state-exit-hook #'+org-realign-table-maybe-h nil t)
    (advice-add #'evil-replace :after #'+org-realign-table-maybe-a))
  )
(defun +org-realign-table-maybe-a (&rest _)
  "Auto-align table under cursor and re-calculate formulas."
  (when (eq major-mode 'org-mode)
    (+org-realign-table-maybe-h))
  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("871b064b53235facde040f6bdfa28d03d9f4b966d8ce28fb1725313731a2bcc8" "a5270d86fac30303c5910be7403467662d7601b821af2ff0c4eb181153ebfc0a" "ba323a013c25b355eb9a0550541573d535831c557674c8d59b9ac6aa720c21d3" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
