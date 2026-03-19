;; attempt to setup plain vanilla emacs to my liking

;;;;;;;;;;;;;;;;;;;; 
;; packages, etc. ;;
;;;;;;;;;;;;;;;;;;;;

;; Straight package management
(setq package-enable-at-startup nil)
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


;; Use-package
(straight-use-package 'use-package)
(require 'use-package)

;;recent files 
(use-package recentf
  :init
  (recentf-mode 1)
  (setq recentf-max-menu-items 50)
  (setq recentf-max-saved-items 50)
  (run-at-time (current-time) 300 'recentf-save-list)
  )

;; kill ring and macOS clipboard
(use-package xclip
  :straight t
  :config
  (xclip-mode 1)
  )

;; circumvent macOS dired
(require 'ls-lisp)
(setq ls-lisp-use-insert-directory-program nil)

;;;;;;;;;;
;; keys ;;
;;;;;;;;;;

(global-set-key [remap dabbrev-expand] 'hippie-expand)
(keymap-global-set "C-<tab>" 'hippie-expand)
(keymap-global-set "C-c C-x C-c" 'citar-insert-citation)
(keymap-global-set "C-c l e b" 'eval-buffer)
(keymap-global-set "C-c o t" 'vterm-other-window)
(keymap-global-set "C-x g" 'magit-status)
(keymap-global-set "C-x C-r" 'recentf-open)
(keymap-global-set "C-c s" 'yas-insert-snippet)


;;;;;;;;;;;;;;;;
;; Appearance ;;
;;;;;;;;;;;;;;;;



(defun toggle-light-dark-theme ()
  (interactive)
  (let* ((light-theme 'modus-operandi-tinted) ; Your preferred light theme name
         (dark-theme 'modus-vivendi)   ; Your preferred dark theme name
         (current-theme (car custom-enabled-themes))
         (next-theme (if (eq current-theme light-theme)
                         dark-theme
                       light-theme)))
    (load-theme next-theme t)
    (message "Switched to %s theme" next-theme)))

(global-set-key [f5] 'toggle-light-dark-theme)


;; Starting buffer
(setq inhibit-startup-message t) 
(setq initial-scratch-message nil)
(setq visible-bell t)
(setq ring-bell-function 'ignore)
(setq warning-minimum-level :emergency)
(setq confirm-kill-processes nil)
(tool-bar-mode -1)
(global-visual-line-mode 1)
(set-fringe-mode 0)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(use-package wc-mode
  :straight t
  )

;;;;;;;;;;;;;;;;;;;;;;;;;
;; various necessities ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

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
 	vertico-resize nil
	vertico-directory-mode +1)
   )
(with-eval-after-load 'vertico
  (define-key vertico-map (kbd "<backspace>") #'vertico-directory-delete-word))
(use-package consult
  :straight t
  :after (vertico)
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

;; git
(use-package git-auto-commit-mode
  :straight t
  :config
  (setq-default gac-automatically-add-new-files-p t)
  (setq-default gac-automatically-push-p t)
  (setq-default gac-ask-for-summary-p nil)
  )

;; swift
(use-package swift-mode
  :straight t
  )

;; json
(use-package json-mode
  :straight t
  )

;; sql
(use-package emacsql
  :straight t
  :defer nil
  )

;; terminal emulation
(use-package vterm
  :straight t
  :config
  (setq vterm-shell "/bin/zsh")
  )
(add-to-list 'display-buffer-alist
             '("\\*vterm\\*"
               (display-buffer-in-side-window)
               (side . bottom)
               (slot . 0)
               (window-height . 0.2))) ; 20% height

;; ido
(setq ido-enable-flex-matching t
      ido-everywhere t
      ido-file-extensions-order '(".org" ".qmd" ".docx")
      )

;; spelling
(with-eval-after-load 'flyspell
  (setq ispell-list-command "--list"
	ispell-dictionary "en_US"))
(use-package flyspell-correct-popup
  :straight t
  :after flyspell)  

;; snippets
(use-package yasnippet
  :straight t
  :config
  (yas-reload-all)
  :hook
  (markdown-mode . yas-minor-mode)
  )

;; code meant to relieve pdf-loading issues
(with-demoted-errors "Path Error: %s"
  (straight-use-package 'exec-path-from-shell)
  (when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)))

;; magit
(use-package magit
  :straight t
  )
(setq magit-display-buffer-function
      (lambda (buffer)
	(display-buffer buffer
			'(display-buffer-in-side-window
			  (side . bottom)
			  (window-height . 0.3)))))

;; minimalist writing setup
(use-package writeroom-mode
  :straight t
  )

;; pdf
;; 1. Setup the Homebrew path first
(setenv "PATH" (concat "/opt/homebrew/bin:/usr/local/bin:" (getenv "PATH")))
(add-to-list 'exec-path "/opt/homebrew/bin")

(straight-use-package 'pdf-tools)

(let* ((pdf-path (expand-file-name "straight/build/pdf-tools/" user-emacs-directory))
       (pdf-bin (expand-file-name "epdfinfo" pdf-path)))
  
  (setq pdf-info-epdfinfo-program pdf-bin)
  (add-to-list 'load-path pdf-path)

  ;; 2. Logic: If the binary is gone (because we deleted it), run a fresh install.
  ;; If it's there, just try to start it.
  (if (file-exists-p pdf-bin)
      (with-demoted-errors "PDF Load Error: %s"
        (require 'pdf-tools)
        (require 'pdf-view)
        (pdf-info-process-assert-running))
    (message "PDF Tools binary missing. Run M-x pdf-tools-install manually.")))

(setq auto-mode-alist (cons '("\\.pdf\\'" . pdf-view-mode) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;
;; org and markdown ;;
;;;;;;;;;;;;;;;;;;;;;;

;; most minimal org package, intended to use for tables only
(use-package org
  :straight t (:type built-in)
  :hook (;;(org-mode . +org-enable-auto-reformat-tables-h)
	 (org-mode . org-indent-mode)
	 (org-mode . flyspell-mode))
  )

;; citations
(use-package citar
  :straight t
  :config
  (setq citar-templates
      '((main . "${author editor:30%sn}     ${date year issued:4}     ${title:30}")
        (suffix . "          ${=key= id:30}    ${=type=:12}    ${tags keywords:*}")
        (preview . "${author editor:%etal} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
        (note . "Notes on ${author editor:%etal}, ${title}"))
      )
		)
(use-package citar-embark
  :after citar embark
  :no-require
  :config (citar-embark-mode)
  )
(setq citar-bibliography '("~/Dropbox/common/big_bib.json"))

;; markdown setup
(use-package markdown-mode
  :straight t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  )
(add-hook 'markdown-mode-hook 'variable-pitch-mode)
(add-hook 'markdown-mode-hook #'olivetti-mode)
(add-hook 'text-mode-hook (lambda ()
				(setq-local line-spacing 0.2)))
(with-eval-after-load 'markdown-mode
  (set-face-attribute 'markdown-header-face nil
		      :foreground "#7393B3"
		      :weight 'bold))




(use-package request
  :straight t
  )
(use-package yaml-mode
  :straight t
  )
(use-package ess
  :straight t
  :init
  (setq ess-style 'RStudio
	ess-use-flymake nil)
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
(use-package poly-markdown
  :straight t
  )

;; quarto essentials
(use-package quarto-mode
  :straight t
   )

(use-package olivetti
  :straight t
  )





;;;;;;;;;;;;;;;;;;;;;;;
;; startup by system ;;
;;;;;;;;;;;;;;;;;;;;;;;

(when (string= system-name "Erics-Mac-mini.local")
  (load-theme 'modus-vivendi)
  (setq initial-frame-alist '((top . 0) (left . 0) (height . 70) (width . 130)))
    (defun my-setup-initial-window-setup()
    "Do initial window setup"
    (interactive)
    (set-face-attribute 'default nil :font "Noto Sans Mono 14")
    ;; (org-agenda nil "z")
    )
  (add-hook 'emacs-startup-hook #'my-setup-initial-window-setup)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil)
  (setq mac-control-modifier 'control)
  (setq ispell-program-name "/opt/homebrew/bin/aspell")
  (set-face-attribute 'variable-pitch nil :family "Noto Sans" :height 160)
  )

(when (string= system-name "Erics-Macbook-Air.local")
  ;; base theme by time of day
  (setq calendar-latitude 38.5)
  (setq calendar-longitude -121.7)

  (use-package circadian
    :straight t
    :after solar
    :config
    (setq circadian-themes '((:sunrise . modus-operandi-tinted)
			     (:sunset . modus-vivendi)))
    (circadian-setup)
    )
  (setq initial-frame-alist '((top . 0) (left . 0) (height . 45) (width . 90)))
    (defun my-setup-initial-window-setup()
    "Do initial window setup"
    (interactive)
    (set-face-attribute 'default nil :font "Noto Sans Mono 14")
    )
  (add-hook 'emacs-startup-hook #'my-setup-initial-window-setup)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil)
  (setq mac-control-modifier 'control)
  (setq ispell-program-name "/opt/homebrew/bin/aspell")
  (set-face-attribute 'variable-pitch nil :family "Noto Sans" :height 140)
  )

(when (eq system-type 'gnu/linux)
  (use-package gruvbox-theme
   :straight t
   :config
   (load-theme 'gruvbox-dark-soft t))

  (defun my-setup-initial-window-setup()
    "Do initial window setup"
    (interactive)
     (setq initial-frame-alist
     	'((top . 0) (left . 0) (height . 68) (width . 80)))
     (set-face-attribute 'default nil :font "Noto Mono 14")
     ;; (org-agenda nil "z")
    )
  (add-hook 'emacs-startup-hook #'my-setup-initial-window-setup)
  )



;; for cleaning whisper transcripts
(defun transcript-polish ()
  "Unwraps choppy transcript lines and creates clean 80-char paragraphs."
  (interactive)
  (save-excursion
    ;; 1. Join all lines in the buffer (or region)
    (let ((beg (if (use-region-p) (region-beginning) (point-min)))
          (end (if (use-region-p) (region-end) (point-max))))
      (subst-char-in-region beg end ?\n ?\ )
      
      ;; 2. Inject double-newlines after every 6th sentence
      (goto-char beg)
      (let ((count 0))
        (while (re-search-forward "[.!?] " nil t)
          (setq count (1+ count))
          (when (= count 6)
            (replace-match (concat (match-string 0) "\n\n"))
            (setq count 0))))
      
      ;; 3. Clean up the wrapping
      (setq fill-column 80)
      (fill-region (point-min) (point-max)))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("9fb561389e5ac5b9ead13a24fb4c2a3544910f67f12cfcfe77b75f36248017d0"
     "871b064b53235facde040f6bdfa28d03d9f4b966d8ce28fb1725313731a2bcc8"
     "a5270d86fac30303c5910be7403467662d7601b821af2ff0c4eb181153ebfc0a"
     "ba323a013c25b355eb9a0550541573d535831c557674c8d59b9ac6aa720c21d3"
     default))
 '(ignored-local-variable-values
   '((gac-ask-for-summary-p) (gac-automatically-add-new-files-p . t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 )
