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
    (:eval (propertize "%l:%c" 'face 'shadow))
    "  "
    ;; major mode (stripped of "-mode" suffix for brevity)
    (:eval (propertize
            (string-replace "-mode" "" (symbol-name major-mode))
            'face 'italic))
    "  "
    ;; word count
    (:eval (propertize
            (format "W:%d" (count-words (point-min) (point-max)))
            'face 'shadow))
    ;; modified indicator
    (:eval (when (buffer-modified-p)
             (propertize "  ●" 'face '(:foreground "yellow"))))
    "  "
    ;; time-day-date
    ;;(:eval (propertize (current-time-string) 'face 'shadow))
    (:eval (propertize (format-time-string "%a %e %b %k:%M") 'face 'shadow))

    ;; UPDATED BATTERY BLOCK
    (:eval (when battery-status-function
	     (let* ((status (funcall battery-status-function))
		    (perc-str (cdr (assoc ?p status)))
		    (perc-num (if perc-str (string-to-number perc-str) 0))
		    (icon (if (< perc-num 20) "🪫" "🔋")))
	       (unless (or (not perc-str) (string= "N/A" perc-str))
		 (propertize (format " %s%s" icon perc-str)
			     'face 'shadow))))    
    )))

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
  :straight t
  )

(when (string= system-name "Erics-Mac-mini.local")
  (defun load-my-themes ()
  (interactive)
  (cond
   ((display-graphic-p)
    ;; Theme for GUI Emacs (e.g., when run locally or via X forwarding)
    (disable-theme 'doric-light) ;; Disable TTY theme if it was somehow active
    (load-theme 'doric-dark t))
   (t
    ;; Theme for Terminal Emacs (emacs -nw)
    (disable-theme 'doric-dark) ;; Disable GUI theme
    (load-theme 'doric-light t))))
  ;; Add a hook to run the function when Emacs starts up or a new frame is created
  (add-hook 'after-make-frame-functions (lambda (frame) (with-selected-frame frame (load-my-themes))))

;; Initial call for the first frame
(load-my-themes)
  ;; (load-theme 'gruvbox-dark-soft t)
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

  ;; ── Location ───────────────────────────────────────────────────────────────
  (defun my/set-calendar-location ()
    "Set calendar lat/long from CoreLocationCLI, falling back to defaults."
    (let ((output (shell-command-to-string "CoreLocationCLI -once -format \"%latitude %longitude\"")))
      (if (string-match "\\(-?[0-9]+\\.[0-9]+\\) \\(-?[0-9]+\\.[0-9]+\\)" output)
          (setq calendar-latitude  (string-to-number (match-string 1 output))
                calendar-longitude (string-to-number (match-string 2 output)))
        (setq calendar-latitude  38.5
              calendar-longitude -121.7))))
  (my/set-calendar-location)

  ;; ── Theme state ────────────────────────────────────────────────────────────
  (defvar my/current-theme-variant nil
    "Current theme variant: 'light or 'dark.")

  (defun my/apply-pdf-theme (variant)
    "Apply midnight-mode or normal rendering to all open PDF buffers."
    (when (featurep 'pdf-tools)
      (let ((enable (eq variant 'dark)))
        (if enable
            (add-hook 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode)
          (remove-hook 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode))
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (when (derived-mode-p 'pdf-view-mode)
              (pdf-view-midnight-minor-mode (if enable 1 -1))))))))

  ;; Called by the circadian hook (below) and by the manual toggle
  (defun my/sync-theme-variant ()
    "Detect which theme circadian just loaded and sync PDF + state var."
    (let ((variant (if (member 'doric-dark custom-enabled-themes) 'dark 'light)))
      (setq my/current-theme-variant variant)
      (my/apply-pdf-theme variant)))

  ;; ── Toggle ─────────────────────────────────────────────────────────────────
  (defvar my/theme-override nil
    "When non-nil, circadian hook is suppressed (manual toggle active).")

  (defun my/toggle-light-dark ()
    "Toggle between light and dark theme, suppressing circadian auto-switch."
    (interactive)
    (setq my/theme-override t)
    (let ((variant (if (eq my/current-theme-variant 'dark) 'light 'dark)))
      (setq my/current-theme-variant variant)
      (mapc #'disable-theme custom-enabled-themes)
      (load-theme (if (eq variant 'dark) 'doric-dark 'doric-light) t)
      (my/apply-pdf-theme variant)))

  ;; Cmd-Shift-T  (M = Meta = Cmd given your modifier settings)
  (global-set-key (kbd "M-T") #'my/toggle-light-dark)

  ;; ── Circadian ──────────────────────────────────────────────────────────────
  (require 'solar)
  (use-package circadian
    :straight t
    :after solar
    :config
    (setq circadian-themes '((:sunrise . doric-light)
                             (:sunset  . doric-dark)))
    (add-hook 'circadian-after-load-theme-hook
              (lambda (theme)
                (unless my/theme-override
                  (my/sync-theme-variant))))
    (circadian-setup))

  ;; ── PDF-tools ──────────────────────────────────────────────────────────────
  (use-package pdf-tools
    :straight t
    :defer t
    :config
    (pdf-tools-install)
    ;; Tweak these colors to match your doric-dark foreground/background:
    (setq pdf-view-midnight-colors '("#d4c9a8" . "#1e1e1e"))
    (add-hook 'pdf-view-mode-hook
              (lambda ()
                (when (eq my/current-theme-variant 'dark)
                  (pdf-view-midnight-minor-mode 1)))))

  ;; ── Window / font setup ────────────────────────────────────────────────────
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local alt text via MLX   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar quarto-mlx-server-url "http://localhost:8080/v1/chat/completions"
  "URL for the local MLX vision server.")

(defvar quarto-mlx-model "mlx-community/Qwen3-VL-8B-Instruct-4bit"
  "Model name to pass to the MLX server. Must match what was loaded.")

(defun quarto--image-to-base64 (path)
  "Return base64-encoded string of image at PATH."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (base64-encode-region (point-min) (point-max) t)
    (buffer-string)))

(defun quarto--media-type (path)
  "Return MIME type string for image PATH based on extension."
  (let ((ext (downcase (file-name-extension path))))
    (cond
     ((string= ext "png")  "image/png")
     ((string= ext "jpg")  "image/jpeg")
     ((string= ext "jpeg") "image/jpeg")
     ((string= ext "gif")  "image/gif")
     ((string= ext "webp") "image/webp")
     (t "image/png"))))

(defvar quarto-alt-text-script "~/bin/alt_text.py"
  "Path to the alt_text.py script.")

(defvar quarto-alt-text-python "/Users/earauchway/tmp/ocr_working/ocr_env/bin/python")

(defun quarto-insert-alt-text-local ()
  "Generate alt text for the Markdown image at point using local mlx_vlm.
Alt text is written to a {fig-alt=\"...\"} attribute block rather than
the square-bracket caption field."
  (interactive)
  (let* ((line (thing-at-point 'line t))
         (match (and line (string-match "!\\[[^]]*\\](\\([^)]+\\))" line)))
         (img-path-raw (and match (match-string 1 line)))
         (img-path (and img-path-raw
                        (expand-file-name img-path-raw
                                          (file-name-directory
                                           (or buffer-file-name default-directory))))))
    (unless match
      (user-error "No Markdown image syntax found on current line"))
    (unless (and img-path (file-exists-p img-path))
      (user-error "Image file not found: %s" img-path))
    (message "Generating alt text for %s (this may take a moment)..."
             (file-name-nondirectory img-path))
    (let* ((script (expand-file-name quarto-alt-text-script))
           (result (shell-command-to-string
                    (format "%s %s %s"
                            quarto-alt-text-python
                            (shell-quote-argument script)
                            (shell-quote-argument img-path))))
           (alt-text (replace-regexp-in-string "\"" "\\\\\"" (string-trim result))))
      (if (string-empty-p alt-text)
          (message "No alt text returned — check that mlx_vlm is installed")
        (save-excursion
          (beginning-of-line)
          ;; Match the image syntax, then optionally an existing {...} block
          (when (re-search-forward
                 "!\\[[^]]*\\](\\([^)]+\\))\\({[^}]*}\\)?"
                 (line-end-position) t)
            (let ((attr-start (match-beginning 2))
                  (attr-end   (match-end 2)))
              (if (and attr-start attr-end)
                  ;; A {...} block already exists — update or insert fig-alt inside it
                  (let ((attrs (match-string 2)))
                    (if (string-match "fig-alt=\"[^\"]*\"" attrs)
                        ;; Replace existing fig-alt value
                        (progn
                          (delete-region attr-start attr-end)
                          (insert (replace-regexp-in-string
                                   "fig-alt=\"[^\"]*\""
                                   (format "fig-alt=\"%s\"" alt-text)
                                   attrs)))
                      ;; Append fig-alt before the closing brace
                      (delete-region attr-start attr-end)
                      (insert (replace-regexp-in-string
                               "}"
                               (format " fig-alt=\"%s\"}" alt-text)
                               attrs))))
                ;; No {...} block — append one after the closing paren
                (goto-char (match-end 0))
                (insert (format "{fig-alt=\"%s\"}" alt-text))))))
        (message "Alt text inserted: %s" alt-text)))))


(defun quarto-insert-all-alt-texts ()
  "Generate and insert alt text for all images in the current buffer.
Alt text is written to a {fig-alt=\"...\"} attribute block rather than
the square-bracket caption field.  Only images that lack a fig-alt
attribute are processed."
  (interactive)
  (let* ((base-dir (file-name-directory (or buffer-file-name default-directory)))
         (images '()))
    ;; Scan buffer for image lines that have no fig-alt yet
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "!\\[[^]]*\\](\\([^)]+\\))\\({[^}]*}\\)?"
              nil t)
        (let* ((path-raw  (match-string 1))
               (attr-block (match-string 2))
               (path      (expand-file-name path-raw base-dir))
               (has-alt   (and attr-block
                               (string-match-p "fig-alt=\"" attr-block))))
          (when (and (not has-alt) (file-exists-p path))
            (push (list path path-raw) images)))))
    (if (null images)
        (message "No images without fig-alt found.")
      (message "Generating alt text for %d image(s)..." (length images))
      (let* ((script (expand-file-name quarto-alt-text-script))
             (paths (mapcar #'car images))
             (cmd (concat quarto-alt-text-python " "
                          (shell-quote-argument script) " "
                          (mapconcat #'shell-quote-argument paths " ")))
             (raw-output (shell-command-to-string cmd))
             (lines (seq-filter (lambda (l) (string-match-p "\t" l))
                                (split-string raw-output "\n"))))
        (let ((results (make-hash-table :test 'equal)))
          (dolist (line lines)
            (let* ((parts (split-string line "\t"))
                   (path  (car parts))
                   (alt   (replace-regexp-in-string "\"" "\\\\\"" (string-trim (cadr parts)))))
              (puthash path alt results)))
          ;; Insert alt texts into buffer
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward
                    "!\\[[^]]*\\](\\([^)]+\\))\\({[^}]*}\\)?"
                    nil t)
              (let* ((path-raw   (match-string 1))
                     (attr-block (match-string 2))
                     (path       (expand-file-name path-raw base-dir))
                     (has-alt    (and attr-block
                                      (string-match-p "fig-alt=\"" attr-block)))
                     (alt-text   (gethash path results)))
                (when (and (not has-alt) alt-text)
                  (let ((attr-start (match-beginning 2))
                        (attr-end   (match-end 2)))
                    (if (and attr-start attr-end)
                        ;; Append fig-alt into the existing {...} block
                        (let ((attrs (match-string 2)))
                          (delete-region attr-start attr-end)
                          (insert (replace-regexp-in-string
                                   "}"
                                   (format " fig-alt=\"%s\"}" alt-text)
                                   attrs)))
                      ;; No {...} block — append one
                      (goto-char (match-end 0))
                      (insert (format "{fig-alt=\"%s\"}" alt-text)))))))))
        (message "Done. Alt text inserted for %d image(s)." (length lines))))))

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

;;;;;;;;;;;;;;;;;;;;;;
;; Quarto Functions ;;
;;;;;;;;;;;;;;;;;;;;;;

;; for Zotero-annotated Word documents in and out of Quarto/markdown

(defun my/docx-to-md (docx-file)
  "Convert a Zotero-annotated docx to Quarto markdown."
  (interactive "fDocx file: ")
  (let* ((default-directory (file-name-directory docx-file))
         (md-file (concat (file-name-sans-extension docx-file) ".md"))
         (cmd (format "python3 /Users/earauchway/writing/zotero_docx_convert.py to-md %s %s"
                      (shell-quote-argument docx-file)
                      (shell-quote-argument md-file))))
    (shell-command cmd)
    (find-file md-file)
    (message "Converted to %s" md-file)))

(defun my/insert-quarto-front-matter ()
  "Insert a Quarto YAML block at the top of the buffer if absent."
  (unless (save-excursion (goto-char (point-min)) (looking-at "^---"))
    (goto-char (point-min))
    (insert "---\nbibliography: ~/path/to/library.bib\n---\n\n")))

(defun my/quarto-render-to-docx ()
  "Render current Zotero markdown buffer back to docx."
  (interactive)
  (save-buffer)
  (let* ((md-file (buffer-file-name))
         (default-directory (file-name-directory md-file))
         (docx-file (read-string "Output docx file: "
                                    default-directory
                                    nil nil
                                    (concat (file-name-base md-file) "_out.docx")))
         (cmd (format "python3 /Users/earauchway/writing/zotero_docx_convert.py to-docx %s %s"
                      (shell-quote-argument md-file)
                      (shell-quote-argument docx-file))))
    (compile cmd)
    (message "Written to %s" docx-file)))

(global-set-key (kbd "C-c z i") #'my/docx-to-md)
(global-set-key (kbd "C-c z o") #'my/quarto-render-to-docx) 

;;;;;;;;;;;;;;;;;;;;;
;; Quarto Previews ;;
;;;;;;;;;;;;;;;;;;;;;

(defvar my/quarto--file-notify-descriptor nil
  "File notify watch descriptor for the current quarto PDF preview.")

(defun my/quarto-get-formats ()
  "Parse ALL output formats from the current qmd file's YAML front matter."
  (save-excursion
    (goto-char (point-min))
    (when (looking-at "---")
      (let ((yaml-end (re-search-forward "^---$" nil t)))
        (when yaml-end
          (let ((yaml (buffer-substring-no-properties (point-min) yaml-end))
                (formats '())
                (known-formats '("revealjs" "pdf" "beamer" "html" "docx"
                                 "pptx" "epub" "typst" "odt" "gfm"
                                 "commonmark" "hugo" "jekyll")))
            (dolist (fmt known-formats)
              (when (string-match-p (concat "\\(format:.*" fmt
                                            "\\|^\s*" fmt ":\\)") yaml)
                (push fmt formats)))
            (nreverse formats)))))))

(defun my/quarto-select-format ()
  "Prompt user to select a format from those declared in the qmd front matter."
  (let ((formats (my/quarto-get-formats)))
    (if formats
        (completing-read "Quarto format: " formats nil t)
      (completing-read "Quarto format (not detected, enter manually): "
                       '("html" "pdf" "revealjs" "beamer" "docx" "typst")
                       nil nil))))

(defun my/quarto-preview (&optional prompt-format)
  "Preview current qmd file, prompting for format if multiple are declared.
With prefix argument C-u, always prompt for format selection."
  (interactive "P")
  (let* ((file (buffer-file-name))
         (formats (my/quarto-get-formats))
         (format
          (cond
           (prompt-format          (my/quarto-select-format))
           ((> (length formats) 1) (my/quarto-select-format))
           ((= (length formats) 1) (car formats))
           (t                      (my/quarto-select-format)))))
    (unless (and file (string-match-p "\\.qmd\\'" file))
      (user-error "Not visiting a .qmd file"))
    (message "Previewing as %s..." format)
    (pcase format
      ((or "revealjs" "html" "gfm" "commonmark" "hugo" "jekyll")
       (my/quarto--start-preview file format))
      ((or "pdf" "beamer" "typst")
       (my/quarto--render-pdf file format))
      ((or "docx" "pptx" "epub" "odt")
       (my/quarto--render-and-open file format))
      (_ (my/quarto--start-preview file format)))))

(defun my/quarto--start-preview (file format)
  "Launch quarto preview server for FILE with FORMAT in a side window."
  (let ((buf-name "*quarto-preview*"))
    (when-let ((buf (get-buffer buf-name)))
      (when-let ((proc (get-buffer-process buf)))
        (delete-process proc))
      (kill-buffer buf))
    (let ((proc-buf (get-buffer-create buf-name)))
      (start-process "quarto-preview" proc-buf
                     "quarto" "preview" file
                     "--to" format
                     "--no-browser" "--no-watch-inputs")
      (display-buffer proc-buf
                      '(display-buffer-in-side-window
                        (side . right)
                        (window-width . 0.5))))))

(defun my/quarto--render-pdf (file format)
  "Render FILE to FORMAT and open the result in a side window."
  (let* ((buf-name "*quarto-render*")
         (local-file file))
    (when-let ((buf (get-buffer buf-name)))
      (kill-buffer buf))
    (let* ((proc-buf (get-buffer-create buf-name))
           (proc (start-process "quarto-render" proc-buf
                                "quarto" "render" local-file "--to" format)))
      (display-buffer proc-buf
                      '(display-buffer-in-side-window
                        (side . right)
                        (window-width . 0.5)))
      (set-process-sentinel
       proc
       (lambda (p _event)
         (when (eq (process-status p) 'exit)
           (if (= (process-exit-status p) 0)
               (let ((pdf-file
                      (with-current-buffer (process-buffer p)
                        (goto-char (point-min))
                        (when (re-search-forward
                               "Output created: \\(.+\\)$" nil t)
                          (expand-file-name
                           (string-trim (match-string 1))
                           (file-name-directory local-file))))))
                 (if pdf-file
                     (my/quarto--open-in-side-window pdf-file)
                   (message "Render succeeded but couldn't find output path in log")))
             (message "Quarto render FAILED — check *quarto-render* buffer"))))))))

(defun my/quarto--render-and-await (file format on-success)
  "Render FILE to FORMAT; parse output path from quarto log, call ON-SUCCESS."
  (let* ((buf-name "*quarto-render*")
         (local-file file))
    (when-let ((buf (get-buffer buf-name)))
      (kill-buffer buf))
    (let* ((proc-buf (get-buffer-create buf-name))
           (proc (start-process "quarto-render" proc-buf
                                "quarto" "render" local-file "--to" format)))
      (display-buffer proc-buf
                      '(display-buffer-in-side-window
                        (side . right)
                        (window-width . 0.5)))
      (set-process-sentinel
       proc
       (lambda (p _event)
         (when (eq (process-status p) 'exit)
           (if (= (process-exit-status p) 0)
               (let ((out-file
                      (with-current-buffer (process-buffer p)
                        (goto-char (point-min))
                        (when (re-search-forward
                               "Output created: \\(.+\\)$" nil t)
                          (expand-file-name
                           (string-trim (match-string 1))
                           (file-name-directory local-file))))))
                 (if out-file
                     (funcall on-success out-file)
                   (message "Render succeeded but couldn't find output path in log")))
             (message "Quarto render FAILED — check *quarto-render* buffer"))))))))

(defun my/quarto--render-and-open (file format)
  "Render FILE to FORMAT (docx, pptx, epub, odt) then open with system viewer."
  (my/quarto--render-and-await
   file format
   (lambda (out-file)
     (message "Opening %s..." out-file)
     (if (eq system-type 'darwin)
         (start-process "open" nil "open" out-file)
       (start-process "xdg-open" nil "xdg-open" out-file)))))

(defun my/quarto--open-in-side-window (file)
  "Open FILE in a right side window — pdf-view for PDF, find-file otherwise."
  (let ((buf (find-file-noselect file)))
    (with-current-buffer buf
      (cond
       ((and (string-match-p "\\.pdf\\'" file) (featurep 'pdf-tools))
        (pdf-view-mode)
        (setq-local pdf-cache-prefetch-delay nil))
       ((string-match-p "\\.pdf\\'" file)
        (doc-view-mode))))
    (display-buffer buf
                    '(display-buffer-in-side-window
                      (side . right)
                      (window-width . 0.5)))))

(defun my/quarto-preview-stop ()
  "Stop any running quarto preview or render process."
  (interactive)
  (when my/quarto--file-notify-descriptor
    (if (timerp my/quarto--file-notify-descriptor)
        (cancel-timer my/quarto--file-notify-descriptor)
      (file-notify-rm-watch my/quarto--file-notify-descriptor))
    (setq my/quarto--file-notify-descriptor nil)
    (message "Stopped PDF watcher"))
  (dolist (buf-name '("*quarto-preview*" "*quarto-render*"))
    (when-let ((buf (get-buffer buf-name)))
      (when-let ((proc (get-buffer-process buf)))
        (delete-process proc)
        (message "Stopped %s" buf-name)))))

(global-set-key (kbd "C-c q p") #'my/quarto-preview)
(global-set-key (kbd "C-c q s") #'my/quarto-preview-stop)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4bc34187baf114f1f3de085ffe9510b3c43fe505d21bd52d75bd5aade8c7839e"
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
