;;; quarto-studio.el --- RStudio-like layout for Quarto presentations, PDFs, HTML, and DOCX

;; ─── Assumptions (all provided by init.el, not repeated here) ────────────────
;; quarto-mode, markdown-mode, xwidget-webkit, vertico, consult,
;; orderless, corfu, corfu-terminal, cape, hippie-expand, pdf-tools

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
(defvar qps-preview-window     nil  "The right-hand preview window.")
(defvar qps-output-buffer-name "*Quarto Output*")
(defvar qps-preview-process    nil  "The running quarto preview/render process.")
(defvar qps-preview-port       4848 "Port for quarto preview server (revealjs).")
(defvar qps-preview-url        nil  "URL of the running quarto preview server.")
(defvar qps-output-dir         nil  "Resolved output-dir for current project.")
(defvar qps--detected-url      nil  "Preview URL detected from process output.")
(defvar qps--detected-pdf      nil  "PDF output path detected from process output.")
(defvar qps--detected-docx     nil  "DOCX output path detected from process output.")
(defvar qps--output-format     nil  "Output format: 'revealjs, 'pdf-latex, 'pdf-typst, 'html, or 'docx.")

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

;; ─── Format detection ────────────────────────────────────────────────────────
(defconst qps--format-labels
  '((revealjs  . "revealjs (presentation)")
    (pdf-typst . "typst (PDF)")
    (pdf-latex . "latex (PDF)")
    (html      . "html (web page)")
    (docx      . "docx (Word document)"))
  "Human-readable labels for each output format symbol.")

(defun qps--format-from-label (label)
  "Return the format symbol corresponding to the completing-read LABEL."
  (car (rassoc label qps--format-labels)))

(defun qps--scan-formats (fm)
  "Return a list of format symbols present in front-matter string FM."
  (let (found)
    (when (or (string-match "format:\\s-*revealjs" fm)
              (string-match "revealjs:" fm))
      (push 'revealjs found))
    (when (or (string-match "format:\\s-*typst" fm)
              (string-match "typst:" fm)
              (string-match "pdf-engine:\\s-*typst" fm))
      (push 'pdf-typst found))
    (when (or (string-match "format:\\s-*pdf" fm)
              (string-match "format:\\s-*beamer" fm)
              (string-match "pdf:" fm))
      (push 'pdf-latex found))
    (when (or (string-match "format:\\s-*html\\b" fm)
              (string-match "^\\s-*html:\\s-*$" fm)    ; standalone `html:` line
              (string-match "^\\s-*html:\\s-*{" fm))   ; `html: {` inline mapping
      (push 'html found))
    (when (or (string-match "format:\\s-*docx\\b" fm)
              (string-match "format:\\s-*word\\b" fm)
              (string-match "^\\s-*docx:\\s-*$" fm)
              (string-match "^\\s-*docx:\\s-*{" fm))
      (push 'docx found))
    (nreverse found)))

(defun qps--prompt-format (formats)
  "Ask the user to choose among FORMAT symbols via completing-read.
Returns the chosen symbol."
  (let* ((labels (mapcar (lambda (f) (cdr (assq f qps--format-labels))) formats))
         (choice (completing-read "Render as: " labels nil t)))
    (qps--format-from-label choice)))

(defun qps--detect-format ()
  "Inspect the source buffer's YAML front matter and set `qps--output-format'.
When exactly one format is declared it is selected automatically.
When multiple formats are declared the user is prompted to choose.
Defaults to revealjs when no format can be determined."
  (with-current-buffer qps-source-buffer
    (save-excursion
      (goto-char (point-min))
      (setq qps--output-format
            (if (not (looking-at "^---"))
                'revealjs
              (let* ((fm-end (save-excursion
                               (forward-line 1)
                               (if (re-search-forward "^---" nil t)
                                   (point)
                                 (point-max))))
                     (fm      (buffer-substring-no-properties (point-min) fm-end))
                     (formats (qps--scan-formats fm)))
                (cond
                 ((null formats)          'revealjs)   ; nothing recognised
                 ((= 1 (length formats))  (car formats)) ; unambiguous
                 (t (qps--prompt-format formats)))))))))  ; ask

(defun qps--pdf-format-p ()
  "Return non-nil when the current document renders to PDF (pdf-tools preview)."
  (memq qps--output-format '(pdf-latex pdf-typst)))

(defun qps--render-format-p ()
  "Return non-nil when the format uses render-on-save rather than a live server.
This covers PDF (both engines) and docx.  Plain html uses `quarto preview'
so that the output is served over HTTP and xwidget-webkit can render it."
  (memq qps--output-format '(pdf-latex pdf-typst docx)))

;; ─── Dark-theme detection ────────────────────────────────────────────────────
(defun qps--dark-theme-p ()
  "Return non-nil when the current Emacs theme appears to be dark.
Checks the background luminance of the default face."
  (let* ((bg (face-background 'default nil t))
         (rgb (when (and bg (not (string= bg "unspecified-bg")))
                (color-name-to-rgb bg))))
    (when rgb
      ;; Relative luminance (simplified — sufficient for dark/light discrimination)
      (< (+ (* 0.2126 (nth 0 rgb))
            (* 0.7152 (nth 1 rgb))
            (* 0.0722 (nth 2 rgb)))
         0.4))))

;; ─── Layout ──────────────────────────────────────────────────────────────────
(defun qps-setup-layout ()
  "Three-pane layout: editor (top-left), output (bottom-left), preview (right)."
  (interactive)
  (delete-other-windows)
  (let* ((right-win (split-window-right (floor (* (frame-width) 0.52))))
         (left-bot  (split-window-below (floor (* (frame-height) 0.67)))))
    (setq qps-preview-window right-win)

    (select-window left-bot)
    (switch-to-buffer (get-buffer-create qps-output-buffer-name))
    (with-current-buffer qps-output-buffer-name
      (setq-local buffer-read-only t)
      (special-mode))

    (select-window right-win)
    (cond
     ;; revealjs and html — live preview server → xwidget
     ((memq qps--output-format '(revealjs html))
      (if qps-preview-url
          (xwidget-webkit-browse-url qps-preview-url)
        (switch-to-buffer (get-buffer-create "*Quarto Preview (pending)*"))))
     ;; PDF — pdf-tools buffer
     ((qps--pdf-format-p)
      (if-let ((pdf-buf (qps--pdf-tools-buffer)))
          (switch-to-buffer pdf-buf)
        (switch-to-buffer (get-buffer-create "*Quarto PDF (pending)*"))))
     ;; DOCX — doc-view buffer
     ((eq qps--output-format 'docx)
      (if-let ((docx-buf (qps--doc-view-buffer)))
          (switch-to-buffer docx-buf)
        (switch-to-buffer (get-buffer-create "*Quarto DOCX (pending)*")))))

    (select-window (get-buffer-window qps-source-buffer))))

;; ─── Output buffer helpers ───────────────────────────────────────────────────
(require 'ansi-color)

(defun qps--output-insert (string)
  "Append STRING to the output buffer, render ANSI colour codes, and scroll."
  (with-current-buffer (get-buffer-create qps-output-buffer-name)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (let ((start (point)))
        (insert string)
        (ansi-color-apply-on-region start (point)))
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
  "Remove ANSI escape sequences from STRING.
Used to produce clean text for pattern matching in the line buffer;
display colouring is handled separately by `qps--output-insert'."
  (replace-regexp-in-string "\033\\[[0-9;]*[mK]" "" string))

(defun qps--process-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (qps--output-insert string)           ; raw to output pane (colors etc.)
    (setq qps--output-line-buffer
          (concat qps--output-line-buffer (qps--strip-ansi string)))
    (let ((lines (split-string qps--output-line-buffer "\n")))
      (setq qps--output-line-buffer (car (last lines)))
      (dolist (line (butlast lines))
        (cond
         ;; revealjs/html: live-server URL — record it; qps--wait-and-open-browser opens the pane.
         ;; Guard against quarto serving a non-web file when _quarto.yml overrides the format.
         ((and (memq qps--output-format '(revealjs html))
               (not qps--detected-url)
               (string-match "Browse at \\(http://[^ \n\r]+\\)" line))
          (let ((url (string-trim (match-string 1 line))))
            (if (string-match-p "\\.\\(docx\\|pdf\\|pptx\\|odt\\|rtf\\|epub\\)\\b" url)
                (qps--output-insert
                 (format "\n[ignoring non-web browse URL: %s]\n" url))
              (setq qps--detected-url url)
              (qps--output-insert (format "\n[detected preview URL: %s]\n" url)))))
         ;; PDF: Quarto emits "Output created: path/to/file.pdf"
         ((and (qps--pdf-format-p)
               (string-match "Output created:\\s-*\\(\\S-+\\.pdf\\)" line))
          (let* ((raw      (match-string 1 line))
                 (pdf-path (if (file-name-absolute-p raw)
                               raw
                             (expand-file-name
                              raw
                              (or (qps--find-quarto-yml)
                                  (file-name-directory
                                   (buffer-file-name qps-source-buffer)))))))
            (setq qps--detected-pdf pdf-path)
            (qps--output-insert (format "\n[PDF output: %s]\n" pdf-path))
            (qps--open-pdf-preview pdf-path)))
         ;; DOCX: "Output created: path/to/file.docx"
         ((and (eq qps--output-format 'docx)
               (string-match "Output created:\\s-*\\(\\S-+\\.docx\\)" line))
          (let* ((raw       (match-string 1 line))
                 (docx-path (if (file-name-absolute-p raw)
                                raw
                              (expand-file-name
                               raw
                               (or (qps--find-quarto-yml)
                                   (file-name-directory
                                    (buffer-file-name qps-source-buffer)))))))
            (setq qps--detected-docx docx-path)
            (qps--output-insert (format "\n[DOCX output: %s]\n" docx-path))
            (qps--open-docx-preview docx-path))))))))

(defun qps--process-sentinel (proc event)
  (qps--output-insert (format "\n[quarto: %s]\n" (string-trim event))))

;; ─── revealjs: preview server ────────────────────────────────────────────────
(defun qps-start-preview-server ()
  "Start `quarto preview` as a background server (revealjs/html)."
  (when (and qps-preview-process
             (process-live-p qps-preview-process))
    (kill-process qps-preview-process))
  (setq qps--detected-url  nil
        qps-preview-url    nil   ; clear so layout shows pending buffer, not stale URL
        qps--output-line-buffer "")
  (qps--resolve-output-dir)
  (let* ((file (buffer-file-name qps-source-buffer))
         (dir  (or (qps--find-quarto-yml)
                   (file-name-directory file)))
         (cmd  `("quarto" "preview" ,file
                 "--to"   ,(symbol-name qps--output-format)
                 "--port" ,(number-to-string qps-preview-port)
                 "--no-browser"
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

;; ─── PDF: render and preview ─────────────────────────────────────────────────
(defun qps--render-to-args ()
  "Return --to args for the current output format, or nil for format default."
  (pcase qps--output-format
    ('pdf-typst  '("--to" "typst"))
    ('pdf-latex  '("--to" "pdf"))
    ('docx       '("--to" "docx"))
    (_           nil)))

(defun qps-render-pdf ()
  "Run `quarto render` to produce output from the source buffer.
Works for PDF (latex/typst), HTML, and DOCX formats."
  (when (and qps-preview-process
             (process-live-p qps-preview-process))
    (kill-process qps-preview-process))
  (setq qps--detected-pdf  nil
        qps--detected-docx nil)
  (setq qps--output-line-buffer "")
  (qps--resolve-output-dir)
  (let* ((file (buffer-file-name qps-source-buffer))
         (dir  (or (qps--find-quarto-yml)
                   (file-name-directory file)))
         (cmd  `("quarto" "render" ,file
                 ,@(qps--render-to-args)
                 ,@(when qps-output-dir
                     (list "--output-dir" qps-output-dir)))))
    (qps--output-separator (format "quarto render (%s)" qps--output-format))
    (qps--output-insert (format "dir:  %s\nfile: %s\ncmd:  %s\n"
                                dir file (mapconcat #'identity cmd " ")))
    (setq qps-preview-process
          (let ((default-directory dir))
            (make-process
             :name     "quarto-render"
             :buffer   qps-output-buffer-name
             :command  cmd
             :filter   #'qps--process-filter
             :sentinel #'qps--process-sentinel)))))

;; ─── PDF-tools integration ───────────────────────────────────────────────────
(defun qps--pdf-tools-buffer ()
  "Return a live pdf-view-mode buffer, if any."
  (seq-find (lambda (b)
              (with-current-buffer b
                (eq major-mode 'pdf-view-mode)))
            (buffer-list)))

(defun qps--apply-midnight-mode (buf)
  "Enable pdf-view-midnight-minor-mode in BUF when the Emacs theme is dark."
  (with-current-buffer buf
    (if (qps--dark-theme-p)
        (unless (bound-and-true-p pdf-view-midnight-minor-mode)
          (pdf-view-midnight-minor-mode 1))
      (when (bound-and-true-p pdf-view-midnight-minor-mode)
        (pdf-view-midnight-minor-mode -1)))))

(defun qps--open-pdf-preview (pdf-path)
  "Open PDF-PATH in the preview pane using pdf-tools.
Applies midnight mode when the active theme is dark."
  (let ((win (if (window-live-p qps-preview-window)
                 qps-preview-window
               (selected-window))))
    (with-selected-window win
      (find-file pdf-path)
      (qps--apply-midnight-mode (current-buffer)))))

(defun qps--refresh-pdf-preview ()
  "Revert the open pdf-tools buffer to show the freshly rendered PDF.
Preserves the current page and re-applies midnight mode."
  (when-let ((pdf-buf (qps--pdf-tools-buffer)))
    (with-current-buffer pdf-buf
      (let ((page (ignore-errors (pdf-view-current-page))))
        (revert-buffer t t t)
        (when page
          (pdf-view-goto-page page))
        (qps--apply-midnight-mode (current-buffer))))))

;; ─── doc-view integration (docx) ─────────────────────────────────────────────
(defvar qps--doc-view-unoconv-program "/opt/homebrew/bin/unoconv"
  "Path to the unoconv executable used by doc-view for DOCX conversion.
Set this to the actual location of unoconv on your system.")

(defun qps--doc-view-buffer ()
  "Return a live doc-view-mode buffer, if any."
  (seq-find (lambda (b)
              (with-current-buffer b
                (eq major-mode 'doc-view-mode)))
            (buffer-list)))

(defun qps--open-docx-preview (docx-path)
  "Open DOCX-PATH in the preview pane using doc-view."
  (let ((doc-view-odf->pdf-converter-program qps--doc-view-unoconv-program)
        (win (if (window-live-p qps-preview-window)
                 qps-preview-window
               (selected-window))))
    (with-selected-window win
      (find-file docx-path))))  ; doc-view auto-activates via magic-mode-alist

(defun qps--refresh-docx-preview ()
  "Revert the open doc-view buffer to show the freshly rendered DOCX.
Preserves the current page."
  (when-let ((docx-buf (qps--doc-view-buffer)))
    (with-current-buffer docx-buf
      (let ((page (ignore-errors (doc-view-current-page))))
        (revert-buffer t t t)
        (when page
          ;; doc-view renders asynchronously; wait for pages then jump
          (run-with-timer
           0.5 nil
           (lambda ()
             (when (buffer-live-p docx-buf)
               (with-current-buffer docx-buf
                 (ignore-errors (doc-view-goto-page page)))))))))))

;; ─── Save hook for render-on-save formats ────────────────────────────────────
(defun qps--after-save-render ()
  "Re-render after saving, for all render-on-save formats (PDF, DOCX, HTML)."
  (when (and (buffer-live-p qps-source-buffer)
             (qps--render-format-p)
             (eq (current-buffer) qps-source-buffer))
    (qps-render-pdf)))

(defun qps--install-save-hook ()
  "Install the after-save render hook in the source buffer."
  (with-current-buffer qps-source-buffer
    (add-hook 'after-save-hook #'qps--after-save-render nil t)))

(defun qps--remove-save-hook ()
  "Remove the after-save render hook from the source buffer."
  (when (buffer-live-p qps-source-buffer)
    (with-current-buffer qps-source-buffer
      (remove-hook 'after-save-hook #'qps--after-save-render t))))

;; ─── Browser control (revealjs) ──────────────────────────────────────────────
(defun qps--xwidget-buffer ()
  "Return the live xwidget-webkit buffer, if any."
  (seq-find (lambda (b)
              (with-current-buffer b
                (eq major-mode 'xwidget-webkit-mode)))
            (buffer-list)))

(defun qps--reload-xwidget ()
  "Reload the xwidget preview buffer, or navigate to detected URL if needed."
  (when-let ((xwb (qps--xwidget-buffer)))
    (with-current-buffer xwb
      (let ((current (xwidget-webkit-uri (xwidget-at (point-min)))))
        (if (and qps--detected-url
                 (not (string= current qps--detected-url)))
            (xwidget-webkit-browse-url qps--detected-url)
          (xwidget-webkit-reload))))))

(defun qps--open-browser-at (url)
  "Open URL in the preview pane using xwidget-webkit."
  (let ((win (if (window-live-p qps-preview-window)
                 qps-preview-window
               (selected-window))))
    (with-selected-window win
      (xwidget-webkit-browse-url url))))

(defun qps--wait-and-open-browser (&optional attempts)
  "Poll until the quarto server announces its URL, then open xwidget preview."
  (let ((attempts (or attempts 0)))
    (cond
     (qps--detected-url
      (qps--open-browser-at qps--detected-url))
     ((> attempts 40)
      (qps--output-insert "\n[timed out waiting for quarto preview URL]\n"))
     (t
      (run-with-timer 0.5 nil #'qps--wait-and-open-browser
                      (1+ attempts))))))

;; ─── Main commands ────────────────────────────────────────────────────────────
(defun quarto-studio ()
  "Open the three-pane Quarto Studio layout and start the appropriate preview.
Detects the output format from YAML front matter and routes to the revealjs
live-preview server, or the render-on-save workflow for PDF, DOCX, and HTML."
  (interactive)
  (setq qps-source-buffer (current-buffer))  ; must precede detect-format
  (qps--detect-format)
  (qps--remove-save-hook)          ; clean up any previous session hook
  (setq qps-preview-url nil)       ; ensure layout shows pending buffer, not a stale URL
  (qps-setup-layout)
  (cond
   ((qps--render-format-p)
    (qps--install-save-hook)
    (qps-render-pdf)
    ;; Typst is fast; latex is slow; docx (unoconv) is moderate.
    (let ((delay (pcase qps--output-format
                   ('pdf-typst 3.0)
                   ('pdf-latex 8.0)
                   ('docx      5.0)
                   (_          5.0))))
      (run-with-timer delay nil
                      (lambda ()
                        (cond
                         ((and (qps--pdf-format-p) qps--detected-pdf)
                          (qps--open-pdf-preview qps--detected-pdf))
                         ((and (eq qps--output-format 'docx) qps--detected-docx)
                          (qps--open-docx-preview qps--detected-docx)))))))
   (t
    (qps-start-preview-server)
    (qps--wait-and-open-browser))))

(defun qps-render-and-preview ()
  "Manually trigger a render (PDF/DOCX/HTML) or reload (revealjs)."
  (interactive)
  (if (qps--render-format-p)
      (qps-render-pdf)
    (qps--reload-xwidget)))

(defun qps-kill-preview ()
  "Kill the quarto preview/render process and remove the save hook."
  (interactive)
  (qps--remove-save-hook)
  (when (and qps-preview-process
             (process-live-p qps-preview-process))
    (kill-process qps-preview-process)
    (setq qps-preview-process nil)
    (qps--output-insert "\n[preview server stopped]\n")
    (message "Quarto preview stopped.")))

(defun qps-restart-preview ()
  "Kill and restart the preview/render workflow."
  (interactive)
  (qps-kill-preview)
  (if (qps--render-format-p)
      (progn
        (qps--install-save-hook)
        (run-with-timer 0.5 nil #'qps-render-pdf))
    (run-with-timer 0.5 nil #'qps-start-preview-server)
    (run-with-timer 1.5 nil #'qps--wait-and-open-browser)))

(defun qps-toggle-midnight ()
  "Manually toggle pdf-view midnight mode in the preview pane."
  (interactive)
  (if-let ((pdf-buf (qps--pdf-tools-buffer)))
      (with-current-buffer pdf-buf
        (pdf-view-midnight-minor-mode 'toggle))
    (message "No pdf-tools preview buffer found.")))

(defun qps-switch-format ()
  "Switch the active output format and restart the preview.
Only offers formats actually declared in the document's front matter,
falling back to all known formats if the front matter cannot be parsed."
  (interactive)
  (unless (buffer-live-p qps-source-buffer)
    (user-error "No active quarto-studio session"))
  (let* ((fm (with-current-buffer qps-source-buffer
               (save-excursion
                 (goto-char (point-min))
                 (when (looking-at "^---")
                   (let ((end (save-excursion
                                (forward-line 1)
                                (if (re-search-forward "^---" nil t)
                                    (point)
                                  (point-max)))))
                     (buffer-substring-no-properties (point-min) end))))))
         (available (if fm
                        (let ((found (qps--scan-formats fm)))
                          (if found found '(revealjs pdf-typst pdf-latex html docx)))
                      '(revealjs pdf-typst pdf-latex html docx)))
         (new-format (qps--prompt-format available)))
    (unless (eq new-format qps--output-format)
      (setq qps--output-format new-format)
      (qps--output-separator
       (format "switching to %s"
               (cdr (assq new-format qps--format-labels))))
      (qps-kill-preview)
      (qps-setup-layout)
      (cond
       ((qps--render-format-p)
        (qps--install-save-hook)
        (qps-render-pdf)
        (let ((delay (pcase qps--output-format
                       ('pdf-typst 3.0)
                       ('pdf-latex 8.0)
                       ('docx      5.0)
                       (_          5.0))))
          (run-with-timer delay nil
                          (lambda ()
                            (cond
                             ((and (qps--pdf-format-p) qps--detected-pdf)
                              (qps--open-pdf-preview qps--detected-pdf))
                             ((and (eq qps--output-format 'docx) qps--detected-docx)
                              (qps--open-docx-preview qps--detected-docx)))))))
       (t
        (setq qps--detected-url nil)
        (qps-start-preview-server)
        (qps--wait-and-open-browser))))))

;; ─── Keybindings ─────────────────────────────────────────────────────────────
(define-minor-mode quarto-studio-mode
  "Minor mode providing quarto-studio keybindings.
Defined as a proper minor mode so that its keymap remains active in
polymode/mmm-mode sub-buffers (e.g. the YAML front matter region)
where buffer-local bindings set via `local-set-key' would not be seen."
  :lighter " QS"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c r s") #'quarto-studio)
            (define-key map (kbd "C-c r f") #'qps-switch-format)
            (define-key map (kbd "C-c r p") #'qps-render-and-preview)
            (define-key map (kbd "C-c r k") #'qps-kill-preview)
            (define-key map (kbd "C-c r r") #'qps-restart-preview)
            (define-key map (kbd "C-c r m") #'qps-toggle-midnight)
            map))

(defun qps-setup-keys ()
  (quarto-studio-mode 1))

(add-hook 'quarto-mode-hook   #'qps-setup-keys)
(add-hook 'markdown-mode-hook #'qps-setup-keys)

;; ─── Midnight mode: re-apply when the theme changes ─────────────────────────
;; If the user switches themes mid-session, midnight mode follows suit.
(defun qps--sync-midnight-on-theme-change (&rest _)
  "Re-evaluate midnight mode on all open pdf-tools buffers after a theme load."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (eq major-mode 'pdf-view-mode)
        (qps--apply-midnight-mode buf)))))

(add-hook 'enable-theme-functions  #'qps--sync-midnight-on-theme-change)
(add-hook 'disable-theme-functions #'qps--sync-midnight-on-theme-change)

;; ─── Cleanup on exit ─────────────────────────────────────────────────────────
(add-hook 'kill-emacs-hook #'qps-kill-preview)

(provide 'quarto-studio)
;;; quarto-studio.el ends here
