;;; quarto-studio.el --- RStudio-like layout for Quarto presentations

;; ─── Assumptions (all provided by init.el, not repeated here) ────────────────
;; quarto-mode, markdown-mode, xwidget-webkit, vertico, consult,
;; orderless, corfu, corfu-terminal, cape, hippie-expand

;; ─── Cape completion for Quarto buffers ──────────────────────────────────────
(defun qps-setup-completion ()
  "Rich multi-source completion for Quarto/markdown buffers."
  (setq-local completion-at-point-functions
              (list
               (cape-capf-super
                #'cape-dabbrev     ; words from open buffers
                #'cape-file        ; filenames: image paths, bibliography, etc.
                #'cape-keyword)    ; markdown/yaml keywords
               #'cape-dict))       ; dictionary words — useful for prose
  ;; orderless works naturally with corfu; ensure basic is available as fallback
  (setq-local completion-styles '(orderless basic)))

(add-hook 'quarto-mode-hook   #'qps-setup-completion)
(add-hook 'markdown-mode-hook #'qps-setup-completion)

;; ─── State ───────────────────────────────────────────────────────────────────
(defvar qps-source-buffer      nil  "The .qmd source buffer.")
(defvar qps-output-buffer-name "*Quarto Output*")
(defvar qps-preview-process    nil  "The running quarto preview process.")
(defvar qps-preview-port       4848 "Port for quarto preview server.")
(defvar qps-preview-url        nil  "URL of the running quarto preview server.")
(defvar qps-output-dir         nil  "Resolved output-dir for current project.")

;; ─── output-dir resolution ───────────────────────────────────────────────────
(defun qps--find-quarto-yml ()
  "Walk up from current file to find _quarto.yml or _quarto.yaml."
  (when-let ((file (buffer-file-name qps-source-buffer)))
    (locate-dominating-file file
                            (lambda (dir)
                              (or (file-exists-p (expand-file-name "_quarto.yml"  dir))
                                  (file-exists-p (expand-file-name "_quarto.yaml" dir)))))))

(defun qps--parse-output-dir (yml-dir)
  "Extract output-dir value from _quarto.yml in YML-DIR, or nil if absent."
  (let* ((yml-file (or (let ((f (expand-file-name "_quarto.yml" yml-dir)))
                         (when (file-exists-p f) f))
                       (let ((f (expand-file-name "_quarto.yaml" yml-dir)))
                         (when (file-exists-p f) f))))
         (output-dir nil))
    (when yml-file
      (with-temp-buffer
        (insert-file-contents yml-file)
        (goto-char (point-min))
        ;; Simple line-based parse — sufficient for a single YAML scalar value
        (when (re-search-forward "^\\s-*output-dir:\\s-*\\(\\S-+\\)" nil t)
          (setq output-dir (match-string 1)))))
    ;; Resolve relative to yml-dir
    (when output-dir
      (expand-file-name output-dir yml-dir))))

(defun qps--resolve-output-dir ()
  "Set qps-output-dir for the current project, or nil if not configured."
  (setq qps-output-dir
        (when-let ((yml-dir (qps--find-quarto-yml)))
          (qps--parse-output-dir yml-dir))))

;; ─── Layout ──────────────────────────────────────────────────────────────────
(defun qps-setup-layout ()
  "Three-pane layout: editor (top-left), output (bottom-left), preview (right)."
  (interactive)
  (delete-other-windows)
  (setq qps-source-buffer (current-buffer))

  (let* ((right-win (split-window-right (floor (* (frame-width) 0.52))))
         (left-bot  (split-window-below (floor (* (frame-height) 0.67)))))

    (select-window left-bot)
    (switch-to-buffer (get-buffer-create qps-output-buffer-name))
    (with-current-buffer qps-output-buffer-name
      (setq-local buffer-read-only t)
      (special-mode))

    (select-window right-win)
    (if qps-preview-url
        (xwidget-webkit-browse-url qps-preview-url)
      (switch-to-buffer (get-buffer-create "*Quarto Preview (pending)*")))

    (select-window (get-buffer-window qps-source-buffer))))

;; ─── Output buffer helpers ───────────────────────────────────────────────────
(defun qps--output-insert (string)
  "Append STRING to the output buffer and scroll its window."
  (with-current-buffer (get-buffer-create qps-output-buffer-name)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert string)
      (when-let ((win (get-buffer-window (current-buffer) t)))
        (set-window-point win (point-max))))))

(defun qps--output-separator (label)
  "Insert a timestamped section separator into the output buffer."
  (qps--output-insert
   (format "\n─── %s  %s ───\n" label (format-time-string "%H:%M:%S"))))

;; ─── Process management ──────────────────────────────────────────────────────

(defvar qps--output-line-buffer ""
  "Partial line buffer for quarto process output.")

(defun qps--strip-ansi (string)
  "Remove ANSI escape sequences from STRING."
  (replace-regexp-in-string "\033\\[[0-9;]*[mK]" "" string))

(defun qps--process-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (qps--output-insert string)           ; raw to output pane (colors etc.)
    (setq qps--output-line-buffer
          (concat qps--output-line-buffer (qps--strip-ansi string)))
    (let ((lines (split-string qps--output-line-buffer "\n")))
      (setq qps--output-line-buffer (car (last lines)))
      (dolist (line (butlast lines))
        (when (and (not qps--detected-url)
                   (string-match "Browse at \\(http://[^ \n\r]+\\)" line))
          (let ((url (match-string 1 line)))
            (setq qps--detected-url (string-trim url))
            (qps--output-insert (format "\n[detected preview URL: %s]\n"
                                        qps--detected-url))
            (qps--open-browser-at qps--detected-url)))))))

(defun qps--process-sentinel (proc event)
  (qps--output-insert (format "\n[quarto: %s]\n" (string-trim event))))

(defun qps-start-preview-server ()
  "Start `quarto preview` as a background server."
  (when (and qps-preview-process
             (process-live-p qps-preview-process))
    (kill-process qps-preview-process))
  (setq qps--detected-url nil) ; resets on fresh start
  (setq qps--output-line-buffer "")
  (qps--resolve-output-dir)
  (let* ((file (buffer-file-name qps-source-buffer))
         ;; Run in project root so quarto sees _quarto.yml
         (dir  (or (qps--find-quarto-yml)
                   (file-name-directory file)))
         (cmd  `("quarto" "preview" ,file
                 "--port" ,(number-to-string qps-preview-port)
                 "--no-browser"
                 ;; If project defines output-dir, make it explicit so the
                 ;; preview server knows where to write and serve from
                 ,@(when qps-output-dir
                     (list "--output-dir" qps-output-dir)))))
    (setq qps-preview-url (format "http://localhost:%d" qps-preview-port))
    (qps--output-separator "quarto preview starting")
    (qps--output-insert (format "dir:  %s\nfile: %s\ncmd:  %s\n"
                                dir file (mapconcat #'identity cmd " ")))
    (setq qps-preview-process
          (let ((default-directory dir))
            (make-process
             :name     "quarto-preview"
             :buffer   qps-output-buffer-name
             :command  cmd
             :filter   #'qps--process-filter
             :sentinel #'qps--process-sentinel)))))

;; ─── Browser control ─────────────────────────────────────────────────────────
(defun qps--xwidget-buffer ()
  "Return the live xwidget-webkit buffer, if any."
  (seq-find (lambda (b)
              (with-current-buffer b
                (eq major-mode 'xwidget-webkit-mode)))
            (buffer-list)))

(defun qps--reload-xwidget ()
  "Reload the xwidget preview buffer."
  (when-let ((xwb (qps--xwidget-buffer)))
    (with-current-buffer xwb
      (xwidget-webkit-reload))))

(defun qps--open-browser-at (url)
  "Open URL in the right-hand xwidget pane."
  (let ((right-win (window-at (- (frame-width) 2)
                              (/ (frame-height) 2))))
    (when right-win
      (with-selected-window right-win
        (xwidget-webkit-browse-url url)))))

(defun qps--wait-and-open-browser (&optional attempts)
  "Poll until the quarto server announces its URL, then open xwidget preview.
The process filter handles the actual open once the URL is known;
this is a fallback for cases where the URL was already detected."
  (let ((attempts (or attempts 0)))
    (cond
     ;; URL already detected by process filter — open immediately
     (qps--detected-url
      (qps--open-browser-at qps--detected-url))
     ;; Give up after ~20 seconds
     ((> attempts 40)
      (qps--output-insert "\n[timed out waiting for quarto preview URL]\n"))
     ;; Keep polling
     (t
      (run-with-timer 0.5 nil #'qps--wait-and-open-browser
                      (1+ attempts))))))

;; ─── Main commands ────────────────────────────────────────────────────────────
(defun quarto-studio ()
  "Open the three-pane Quarto Studio layout and start the preview server."
  (interactive)
  (qps-setup-layout)
  (qps-start-preview-server)
  (qps--wait-and-open-browser))

(defun qps--reload-xwidget ()
  "Reload the xwidget preview buffer, or navigate to detected URL if needed."
  (when-let ((xwb (qps--xwidget-buffer)))
    (with-current-buffer xwb
      (let ((current (xwidget-webkit-uri (xwidget-at (point-min)))))
        (if (and qps--detected-url
                 (not (string= current qps--detected-url)))
            ;; URL has changed (e.g. after restart) — navigate rather than reload
            (xwidget-webkit-browse-url qps--detected-url)
          (xwidget-webkit-reload))))))

(defun qps-kill-preview ()
  "Kill the quarto preview server."
  (interactive)
  (when (and qps-preview-process
             (process-live-p qps-preview-process))
    (kill-process qps-preview-process)
    (setq qps-preview-process nil)
    (qps--output-insert "\n[preview server stopped]\n")
    (message "Quarto preview server stopped.")))

(defun qps-restart-preview ()
  "Kill and restart the preview server (useful after output-dir changes)."
  (interactive)
  (qps-kill-preview)
  (run-with-timer 0.5 nil #'qps-start-preview-server)
  (run-with-timer 1.5 nil #'qps--wait-and-open-browser))

;; ─── Keybindings ─────────────────────────────────────────────────────────────
(defun qps-setup-keys ()
  (local-set-key (kbd "C-c r s") #'quarto-studio)
  (local-set-key (kbd "C-c r p") #'qps-render-and-preview)
  (local-set-key (kbd "C-c r k") #'qps-kill-preview)
  (local-set-key (kbd "C-c r r") #'qps-restart-preview))

(add-hook 'quarto-mode-hook   #'qps-setup-keys)
(add-hook 'markdown-mode-hook #'qps-setup-keys)

;; ─── Cleanup on exit ─────────────────────────────────────────────────────────
(add-hook 'kill-emacs-hook #'qps-kill-preview)

(provide 'quarto-studio)
;;; quarto-studio.el ends here
