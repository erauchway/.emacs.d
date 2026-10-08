;; -*- lexical-binding: t; -*-

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

(straight-use-package 'transient)
(require 'transient)

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
(keymap-global-set "C-c f" 'toggle-frame-fullscreen)

;;;;;;;;;;;;;;;;
;; Appearance ;;
;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;
;; custom modeline ;;
;;;;;;;;;;;;;;;;;;;;;

(require 'battery)
(display-battery-mode 1)
(setq-default mode-line-format
  '("%e"
    ;; filename in bold
    (:eval (propertize (buffer-name) 'face 'bold))
    "  "
    ;; position
    (:eval (propertize "%l:%c"
		       ))
    "  "
    ;; major mode (stripped of "-mode" suffix for brevity)
    (:eval (propertize
            (string-replace "-mode" "" (symbol-name major-mode))
            'face 'italic))
    "  "
    ;; word count
    (:eval (propertize
            (format "W:%d" (count-words (point-min) (point-max)))
            ))
    ;; modified indicator
    (:eval (when (buffer-modified-p)
             (propertize "  ●" 'face '(:foreground "yellow"))))
    "  "
    ;; time-day-date
    ;;(:eval (propertize (current-time-string) 'face 'shadow))
    (:eval (propertize (format-time-string "%a %e %b %k:%M")
		       ))

    ;; UPDATED BATTERY BLOCK
    (:eval (when battery-status-function
	     (let* ((status (funcall battery-status-function))
		    (perc-str (cdr (assoc ?p status)))
		    (perc-num (if perc-str (string-to-number perc-str) 0))
		    (icon (if (< perc-num 20) "🪫" "🔋")))
	       (unless (or (not perc-str) (string= "N/A" perc-str))
		 (propertize (format " %s%s" icon perc-str)
			     ))))    
    )))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; various necessities ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;;completion


(use-package corfu
  :straight (:host github :repo "minad/corfu")
  :custom
  (corfu-auto t)
  (corfu-auto-delay 2)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-quit-no-match t)
  (corfu-left-margin-width 0.5)
  (corfu-right-margin-width 0.5)
  :config
  (global-corfu-mode)
  )




(use-package cape
  :straight t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev t)
  (add-to-list 'completion-at-point-functions #'cape-file    t)
  )

;; hippie-expansion 
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

;;vertical completions
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

(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
	      ("RET" . vertico-directory-enter)
	      ("DEL" . vertico-directory-delete-char)
	      ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package consult
  :straight t
  :after (vertico)
  )

(use-package marginalia
  :straight t
  :after (vertico)
  :init (marginalia-mode)
  )

(use-package orderless
  :straight t
  :after(vertico)
  :config
  (setq completion-styles '(orderless basic))
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

;; search plain-text files on disk
(use-package deft
  :straight t
  :config
  (setq deft-directory "~/writing/notes"
	deft-recursive t
	deft-use-filename-as-title t
	deft-extensions '("md" "qmd" "org" "txt" "docx")
	;; deft-default-extension "md"
	)
  :bind ("C-c d" . deft)
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

(defvar my/last-pulled-repo nil)

(defun my/git-pull-if-repo ()
  "Run git pull if in a git repo, at most once per repo per session."
  (let ((git-dir (locate-dominating-file default-directory ".git")))
    (when (and git-dir (not (equal git-dir my/last-pulled-repo)))
      (setq my/last-pulled-repo git-dir)
      (let ((default-directory git-dir))
        (message "Running git pull in %s..." git-dir)
	(start-process "git-pull" nil "git" "pull")
	;; (async-shell-command "git pull" "*git-pull*")
	))))

(add-hook 'find-file-hook #'my/git-pull-if-repo)

;; swift
(use-package swift-mode
  :straight t
  )

;; json
(use-package json-mode
  :straight t
  )

;; lua
(use-package lua-mode
  :straight t
  )

;; typst
(use-package typst-ts-mode
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

(defvar my/last-fetched-repo nil
   "Git repo most recently fetched in this session, to avoid repeated fetches.")

(defun my/magit-auto-fetch-before-display ()
   "Fetch in the current git repo before showing a magit status buffer."
   (let ((git-dir (locate-dominating-file default-directory ".git")))
     (when (and git-dir (not (equal git-dir my/last-fetched-repo)))
       (setq my/last-fetched-repo git-dir)
       (let ((default-directory git-dir))
         (message "Running git fetch in %s..." git-dir)
	(start-process "git-fetch" nil "git" "fetch")))))

(add-hook 'magit-pre-display-buffer-hook #'my/magit-auto-fetch-before-display)


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

  (if (file-exists-p pdf-bin)
      ;; Defer setup to after init — avoids the face_for_font crash at startup
      (add-hook 'after-init-hook
                (lambda ()
                  (with-demoted-errors "PDF Load Error: %s"
                    (require 'pdf-tools)
                    (require 'pdf-view)
                    (pdf-tools-install :no-query))))
    (message "PDF Tools binary missing. Run M-x pdf-tools-install manually.")))

;; Keep this outside the let — it's safe to set immediately
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))

;;;;;;;;;;;;;;;;;;;;;;
;; Org and markdown ;;
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

;; citar for set to current book bib
(setq citar-bibliography '("~/writing/bibliography/frontiers.json"))

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

(defun my/cape-file-with-underscore ()
  (with-syntax-table (copy-syntax-table (syntax-table))
    (modify-syntax-entry ?_ "_")
    (cape-file)))

(add-hook 'markdown-mode-hook
          (lambda ()
            (setq-local completion-at-point-functions
                        (cons #'my/cape-file-with-underscore
                              (remove #'cape-file completion-at-point-functions)))))

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

(use-package doric-themes
  :straight t)
(use-package gruvbox-theme
  :straight t)

;; ── Shared PDF-tools setup (both Macs) ─────────────────────────────────────
(when (eq system-type 'darwin)
  (use-package pdf-tools
    :straight t
    :defer t
    :config
    (pdf-tools-install)
    (setq pdf-view-midnight-colors '("#ebdbb2" . "#32302f"))))
    ;; (setq pdf-view-midnight-colors '("#d4c9a8" . "#1e1e1e"))))

(defun my/apply-pdf-theme (variant)
  "Apply midnight-mode or normal rendering to all open PDF buffers.
VARIANT is 'light or 'dark."
  (when (featurep 'pdf-tools)
    (let ((enable (eq variant 'dark)))
      (if enable
          (add-hook 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode)
        (remove-hook 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode))
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (derived-mode-p 'pdf-view-mode)
            (pdf-view-midnight-minor-mode (if enable 1 -1))))))))

;; ── Mac Mini: dark in GUI, light in TTY ────────────────────────────────────
(when (string= system-name "ermacmini.local")
  (defun load-my-themes ()
    "Load theme based on frame type, and sync PDF midnight mode."
    (interactive)
    (cond
     ((display-graphic-p)
      (disable-theme 'doric-light)
      (load-theme 'doric-dark t)
      (with-eval-after-load 'pdf-tools
        (my/apply-pdf-theme 'dark)))
     (t
      (disable-theme 'doric-dark)
      (load-theme 'doric-light t)
      (with-eval-after-load 'pdf-tools
        (my/apply-pdf-theme 'light)))))

  (add-hook 'after-make-frame-functions
            (lambda (frame) (with-selected-frame frame (load-my-themes))))
  ;; (load-my-themes)
  (load-theme 'gruvbox-dark-soft t)
  
  (setq initial-frame-alist '((top . 0) (left . 0) (height . 70) (width . 130)))
  (defun my-setup-initial-window-setup ()
    "Do initial window setup."
    (interactive)
    (set-face-attribute 'default nil :font "Noto Sans Mono 14"))
  (add-hook 'emacs-startup-hook #'my-setup-initial-window-setup)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil)
  (setq mac-control-modifier 'control)
  (setq ispell-program-name "/opt/homebrew/bin/aspell")
  (set-face-attribute 'variable-pitch nil :family "Noto Sans" :height 160))

;; ── MacBook Air ────────────────────────────────────────
(when (string= system-name "Erics-Macbook-Air.local")
  (load-theme 'gruvbox-dark-soft t)


  ;; Window / font setup
  (setq initial-frame-alist '((top . 0) (left . 0) (height . 45) (width . 90)))
  (defun my-setup-initial-window-setup ()
    "Do initial window setup."
    (interactive)
    (set-face-attribute 'default nil :font "Noto Sans Mono 14"))
  (add-hook 'emacs-startup-hook #'my-setup-initial-window-setup)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil)
  (setq mac-control-modifier 'control)
  (setq ispell-program-name "/opt/homebrew/bin/aspell")
  (set-face-attribute 'variable-pitch nil :family "Noto Sans" :height 160))

(when (eq system-type 'gnu/linux)
  (defun my-setup-initial-window-setup ()
    "Do initial window setup."
    (interactive)
    (setq initial-frame-alist
          '((top . 0) (left . 0) (height . 68) (width . 80)))
    (set-face-attribute 'default nil :font "Noto Mono 14"))
  (add-hook 'emacs-startup-hook #'my-setup-initial-window-setup))

;; ── CoreLocation helper (available on any Mac) ─────────────────────────────
(defun my/mac-get-location ()
  "Return (lat . lon) using macOS CoreLocation via pyobjc. No installs needed."
  (condition-case nil
      (let* ((script "
import objc, CoreLocation, time, sys
from PyObjCTools import AppHelper

class Delegate(CoreLocation.NSObject):
    location = None
    def locationManager_didUpdateLocations_(self, mgr, locs):
        self.location = locs[-1]
        AppHelper.stopEventLoop()
    def locationManager_didFailWithError_(self, mgr, err):
        AppHelper.stopEventLoop()

d = Delegate.alloc().init()
m = CoreLocation.CLLocationManager.alloc().init()
m.setDelegate_(d)
m.startUpdatingLocation()
AppHelper.runConsoleEventLoop(installInterrupt=True)
if d.location:
    c = d.location.coordinate()
    print(c.latitude, c.longitude)
")
             (raw (string-trim
                   (shell-command-to-string
                    (concat "python3 -c '" script "'"))))
             (_ (unless (string-match
                         "\\(-?[0-9.]+\\)\\s-+\\(-?[0-9.]+\\)" raw)
                  (error "parse fail"))))
        (cons (string-to-number (match-string 1 raw))
              (string-to-number (match-string 2 raw))))
    (error (cons 38.5 -121.7))))



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




;; Keybindings
(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "C-c a t") 'quarto-insert-alt-text-local)
  (define-key markdown-mode-map (kbd "C-c a b") 'quarto-insert-all-alt-texts))

(defun quarto-start-mlx-server ()
  "Start the local MLX vision server in a vterm buffer."
  (interactive)
  (let ((buf (get-buffer-create "*mlx-server*")))
    (with-current-buffer buf
      (vterm-mode)
      (vterm-send-string
       (format "python -m mlx_lm.server --model %s \n"
               quarto-mlx-model)))
    (display-buffer buf)))




(add-to-list 'display-buffer-alist
     '("\*vterm\*"
       (display-buffer-in-direction)
       (direction . below)
       (window . current)
       (window-height . 0.4)
       ))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("1e6afb4c31e36861e75c6339036ccc2ae193798b77cc1d6e953f476f1febad72"
     "366f7fb70999d739ff559568b011f8cfb05c88124e4c944e5e1008312e572137"
     "c5b2e31f3179e32468e15546526c127f9f60c05a3d84fd5fd7cb66f20871d2bd"
     "106fadeab4fb8cf50eeae1e1fd051ae00d0e71a40b5b0f0f5b9db85015398a61"
     "d288d79cf8d8a852ac7ffabdfe7a5cace9b4985565cb5a16dbf89bb4d6a00d8d"
     "4fd1e9da6ff4a6ab7ee4fdc147846f09ce68a543318dd840c7f68205257f32b8"
     "d445c7b530713eac282ecdeea07a8fa59692c83045bf84dd112dd738c7bcad1d"
     "4bc34187baf114f1f3de085ffe9510b3c43fe505d21bd52d75bd5aade8c7839e"
     "6b9fbe5d88424ac7283b8f36b6f184d1140fcd4bfcab1f72a3c58c48dc254bae"
     "9fb561389e5ac5b9ead13a24fb4c2a3544910f67f12cfcfe77b75f36248017d0"
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
