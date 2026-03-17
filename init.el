;; attempt to setup plain vanilla emacs to my liking
;; borrowing some material from Wouter Spekkink https://github.com/WouterSpekkink/dotfiles/blob/master/emacs/init.el



;;;;;;;;;;;;;;;;;;;; 
;; packages, etc. ;;
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

;;recent files 
(use-package recentf
  :init
  (recentf-mode 1)
  (setq recentf-max-menu-items 50)
  (setq recentf-max-saved-items 50)
  (run-at-time (current-time) 300 'recentf-save-list)
  (setq recentf-exclude (file-expand-wildcards (recentf-expand-file-name("~/Dropbox/org/roam/daily/*.org"))))
  )

;; kill ring and macOS clipboard
(use-package xclip
  :straight t
  :config
  (xclip-mode 1)
  )

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
(set-fringe-mode 10)

(menu-bar-mode -1)
(scroll-bar-mode -1)

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
 	vertico-resize nil)
   )
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

;; terminal emulation
(use-package vterm
  :straight t
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
   )

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

(use-package writeroom-mode
  :straight t
  )

;;;;;;;;;
;; org ;;
;;;;;;;;;

;; org and others
(use-package org
  :straight t (:type built-in)
  :hook (;;(org-mode . +org-enable-auto-reformat-tables-h)
	 (org-mode . org-indent-mode)
	 (org-mode . flyspell-mode))
  )

;; on getting unoconv / soffice to work on Mac https://gist.github.com/pankaj28843/3ad78df6290b5ba931c1
(use-package emacsql
  :straight t
  :defer nil
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
;; (setq org-cite-global-bibliography '("~/Dropbox/common/big_bib.json"))
;; (setq org-cite-csl-styles-dir '("~/Zotero/styles"))
(use-package markdown-mode
  :straight t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  )
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
;; quarto essentials
(use-package quarto-mode
  :straight t
   )


(defun writing-mode ()
  (interactive)
  (setq buffer-face-mode-face '(:family "IBM Plex Mono" :height 150))
  (buffer-face-mode)
  (linum-mode 0)
  (writeroom-mode 1)
  (blink-cursor-mode)
  (visual-line-mode 1)
  (setq truncate-lines nil)
  (setq-default line-spacing 5)
  (setq global-hl-line-mode nil)
  )


(use-package olivetti
  :straight t
  )


;;; iA Writer emulation


;; 1. COLORS
(defvar my/ia-writer-colors-light '(:bg "#f5f5f5" :fg "#424242" :cursor "#007aff" :selection "#d0e8ff"))
(defvar my/ia-writer-colors-dark  '(:bg "#111111" :fg "#e0e0e0" :cursor "#007aff" :selection "#103050"))
(defvar my/ia-current-style 'dark)

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

;; 3. Remove all hooks for now to ensure a "plain" success first
(with-eval-after-load 'pdf-view
  ;; 1. Define the colors from your iA theme
  (setq pdf-view-midnight-colors 
        (cons (plist-get my/ia-writer-colors-dark :fg)
              (plist-get my/ia-writer-colors-dark :bg)))

  ;; 2. Create a function to apply the style safely
  (defun my/pdf-view-setup ()
    (pdf-view-midnight-minor-mode -1)
    (pdf-view-fit-page-to-window))

  ;; 3. Attach it to the hook
  (add-hook 'pdf-view-mode-hook #'my/pdf-view-setup)
  (add-hook 'pdf-view-mode-hook #'auto-revert-mode))

;; 2. THE HARDENED CLEANSE
(defun my/cleanse-interface-for-ia (&rest _args)
  "Ultra-stable cleanse: survives missing functions and high-speed switches."
  (let* ((inhibit-message t)
         (message-log-max nil)
         (b-mode (if (and (boundp 'polymode-major-mode) polymode-major-mode) 
                     polymode-major-mode 
                   major-mode)))
    
    (when (memq b-mode '(markdown-mode gfm-mode org-mode poly-markdown-mode vterm-mode))
      (let* ((palette (if (eq my/ia-current-style 'dark) my/ia-writer-colors-dark my/ia-writer-colors-light))
             (bg (plist-get palette :bg))
             (fg (plist-get palette :fg)))
        
        ;; A. STABILIZE MARGINS (Safe Method)
        (ignore-errors
          (when (bound-and-true-p olivetti-mode)
	    (setq-local olivetti-body-width 68) ;; Lowered from 85 for a narrower column
            (setq-local olivetti-color bg)
            ;; If the specific margin func is missing, just refresh the mode
            (if (fboundp 'olivetti-set-margins)
                (olivetti-set-margins)
              (olivetti-mode 1))))

        ;; B. UI & POLYMODE SILENCE
        (setq-local polymode-display-switch-messages nil)
        (setq-local display-line-numbers nil)
        (set-face-attribute 'fringe nil :background bg :foreground bg)
        (set-face-attribute 'vertical-border nil :foreground bg :background bg)
        
        ;; C. THE 'BLEACH' (Monochrome & Fonts)
        (ignore-errors
          (when (facep 'poly-unfocused-chunk-face) (set-face-attribute 'poly-unfocused-chunk-face nil :background bg))
          (when (facep 'poly-header-face) (set-face-attribute 'poly-header-face nil :background bg :underline nil))
          
          (let ((font (if (derived-mode-p 'org-mode) "iA Writer Mono V" "iA Writer Quattro V")))
            (face-remap-add-relative 'default :family font :height 140))
          
          (dolist (face '(markdown-header-face-1 markdown-header-face-2 markdown-header-face-3 
                          markdown-header-face-4 markdown-link-face font-lock-comment-face 
                          font-lock-keyword-face font-lock-string-face font-lock-constant-face))
            (face-remap-add-relative face :foreground fg :weight 'normal))
          
          (dolist (face '(markdown-blockquote-face org-quote org-block tex-math-face))
            (face-remap-add-relative face :foreground fg :slant 'italic)))

        ;; D. MODELINE PERSISTENCE (Independent block to prevent crashes)
        (ignore-errors
          (set-face-attribute 'mode-line nil :background bg :foreground fg :box nil)
          (set-face-attribute 'mode-line-inactive nil :background bg :foreground fg :box nil)
          (setq-local mode-line-format 
                      '("%e" (:eval (propertize " " 'display `(space :align-to (- right 15))))
                        (:eval (format "Words: %d" (count-words (point-min) (point-max)))))))
        
        (setq-local scroll-margin 99)))))

;; 3. THE APPLY FUNCTION
(defun my/apply-ia-style (palette)
  (let ((bg (plist-get palette :bg))
        (fg (plist-get palette :fg))
        (cursor (plist-get palette :cursor))
	(select (plist-get palette :selection)))
    (set-background-color bg)
    (set-foreground-color fg)
    (set-cursor-color cursor)
    (set-face-attribute 'region nil :background select :foreground 'unspecified)
    (when (fboundp 'olivetti-mode)
      (setq-local olivetti-body-width 68)
      (olivetti-mode 1))
    (my/cleanse-interface-for-ia)))

;; 4. AUTOMATION & HOOKS

(defun my/ia-auto-update-style ()
  "Set theme based on machine name or time of day."
  (let* ((hour (string-to-number (format-time-string "%H")))
         (is-desktop (string= system-name "Erics-Mac-mini.local"))
         ;; Logic: If it's the Mini, always dark. Otherwise, check the time.
         (target-style (cond (is-desktop 'dark)
                             ((and (>= hour 7) (< hour 19)) 'light)
                             (t 'dark))))
    
    (setq my/ia-current-style target-style)
    (my/apply-ia-style (if (eq my/ia-current-style 'dark) 
                           my/ia-writer-colors-dark 
                         my/ia-writer-colors-light))))


(setq polymode-display-switch-messages nil)

(add-hook 'markdown-mode-hook #'my/ia-auto-update-style)
(add-hook 'poly-markdown-mode-hook #'my/ia-auto-update-style)
(add-hook 'org-mode-hook #'my/ia-auto-update-style)

(add-hook 'vterm-mode-hook 
          (lambda () 
            ;; Give vterm a moment to 'settle' into a window before cleansing
            (run-at-time "0.1 sec" nil (lambda () 
                                         (with-current-buffer (current-buffer)
                                           (my/ia-auto-update-style))))))

;; Use the switch hooks with a safety wrapper
(add-hook 'polymode-before-switch-buffer-hook (lambda (&rest _args) (ignore-errors (my/cleanse-interface-for-ia))))
(add-hook 'polymode-after-switch-buffer-hook (lambda (&rest _args) (ignore-errors (my/cleanse-interface-for-ia))))

(global-set-key (kbd "<f9>") (lambda () (interactive) 
                               (setq my/ia-current-style (if (eq my/ia-current-style 'dark) 'light 'dark))
                               (my/ia-auto-update-style)))

(defun my/ia-toggle-theme ()
  "Manually toggle between iA Light and iA Dark modes."
  (interactive)
  (if (eq my/ia-current-style 'dark)
      (setq my/ia-current-style 'light)
    (setq my/ia-current-style 'dark))
  (my/apply-ia-style (if (eq my/ia-current-style 'dark) 
                         my/ia-writer-colors-dark 
                       my/ia-writer-colors-light))
  (message "iA Writer Theme: %s" (symbol-name my/ia-current-style)))

;; Bind it to F9
(global-set-key (kbd "<f9>") #'my/ia-toggle-theme)

;;; end iA Writer emulation

;; --- QUARTO CONFIGURATION ---
(defun my/quarto-smart-preview ()
  "Start Quarto, widen frame, and link PDF."
  (interactive)
  (let* ((f (buffer-file-name))
         (base-name (and f (file-name-sans-extension (file-name-nondirectory f))))
         (current-year (format-time-string "%Y"))
         (out-dir (expand-file-name (format "~/Documents/_outputs/%s/" current-year)))
         (pdf-path (concat out-dir base-name ".pdf")))

    (unless f (user-error "Not visiting a file"))
    (save-buffer)

    ;; 1. Layout: Expand for MacBook M4
    (set-frame-parameter nil 'width 180)
    (set-window-margins nil 10 10)

    ;; 2. Start Process
    (unless (get-process "quarto-preview-process")
      (start-process "quarto-preview-process" nil "quarto" "preview" f "--to" "pdf" "--no-browser"))

    ;; 3. Attempt PDF Link
    (if (file-exists-p pdf-path)
        (let ((pdf-buf (find-file-noselect pdf-path t)))
          (delete-other-windows)
          (split-window-right)
          (with-selected-window (window-in-direction 'right)
            (switch-to-buffer pdf-buf)
            (pdf-view-mode)
            (auto-revert-mode 1)
            (pdf-view-fit-page-to-window))
          (select-window (window-in-direction 'left))
          (message "Preview Linked."))
      (message "First build started. Wait 5s and hit C-c P again."))))


(global-set-key (kbd "C-c p") #'my/quarto-smart-preview)

(setq pdf-view-midnight-colors 
      (cons (plist-get my/ia-writer-colors-dark :fg)
            (plist-get my/ia-writer-colors-dark :bg)))


;;;;;;;;;;;;;;;;;;;;;;;
;; startup by system ;;
;;;;;;;;;;;;;;;;;;;;;;;

(when (string= system-name "Erics-Mac-mini.local") 
  (setq initial-frame-alist '((top . 0) (left . 0) (height . 70) (width . 90)))
    (defun my-setup-initial-window-setup()
    "Do initial window setup"
    (interactive)
;;    (setq initial-frame-alist
;; '((top . 0) (left . 0) (height . 68) (width . 80)))
    (set-face-attribute 'default nil :font "IBM Plex Mono 14")
    ;; (org-agenda nil "z")
    )
  (add-hook 'emacs-startup-hook #'my-setup-initial-window-setup)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil)
  (setq mac-control-modifier 'control)
  (setq ispell-program-name "/opt/homebrew/bin/aspell")
  )

(when (string= system-name "Erics-Macbook-Air.local") 
  (setq initial-frame-alist '((top . 0) (left . 0) (height . 45) (width . 90)))
    (defun my-setup-initial-window-setup()
    "Do initial window setup"
    (interactive)
    (set-face-attribute 'default nil :font "IBM Plex Mono 14")
    )
  (add-hook 'emacs-startup-hook #'my-setup-initial-window-setup)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil)
  (setq mac-control-modifier 'control)
  (setq ispell-program-name "/opt/homebrew/bin/aspell")
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


(defun my/theme-sentinel ()
  "Detects if we are in a writer buffer. If not, restores Gruvbox."
  (if (derived-mode-p 'markdown-mode 'org-mode 'vterm-mode)
      (my/ia-auto-update-style) ;; Keep iA colors
    ;; RESTORE GRUVBOX (Change this to your specific gruvbox theme name)
    (set-face-attribute 'fringe nil :background nil :foreground nil)
    (set-face-attribute 'vertical-border nil :foreground nil :background nil)
    (set-face-attribute 'mode-line nil :box t)))

(add-hook 'buffer-list-update-hook #'my/theme-sentinel)


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


(setq initial-buffer-choice (lambda ()
  (let ((buf (get-buffer-create "Draft.md")))
    (with-current-buffer buf
      ;; 1. Set the mode so our iA hooks fire
      (markdown-mode)
      
      ;; 2. Force the style update
      ;; We use a tiny delay so the window width is known for Olivetti
      (run-at-time "0.1 sec" nil 
                   (lambda (b) 
                     (when (buffer-live-p b)
                       (with-current-buffer b
                         (my/ia-auto-update-style))))
                   buf))
    buf)))

(with-eval-after-load 'polymode
  (define-key quarto-mode-map (kbd "C-c C-p") #'my/quarto-smart-preview)
  (define-key markdown-mode-map (kbd "C-c C-p") #'my/quarto-smart-preview))

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
