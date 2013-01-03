(require 'package)
(add-to-list 'package-archives
                          '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
    (package-refresh-contents))

(defvar my-packages
    '(clojure-mode
      evil
      haml-mode
      magit
      markdown-mode
      paredit
      projectile
      sass-mode
      yaml-mode evil
      auto-complete
      ac-nrepl
      textmate
      peepopen
      ace-jump-mode
      rainbow-delimiters
      nrepl)
      "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
    (when (not (package-installed-p p))
          (package-install p)))

(add-to-list 'exec-path "/opt/local/bin")
(setenv "PATH" (concat (getenv "PATH") ":/opt/local/bin"))

(add-to-list 'load-path "~/.emacs.d")

(require 'auto-complete)
(require 'auto-complete-config)
(require 'ac-nrepl)
(require 'rainbow-delimiters)
(require 'surround)

(evil-mode t)
(textmate-mode t)
(global-auto-complete-mode t)

(defun startup-nrepl ()
  (ac-nrepl-setup)
  (rainbow-delimiters-mode t))

(add-hook 'nrepl-mode-hook 'startup-nrepl)
(add-hook 'nrepl-interaction-mode-hook 'startup-nrepl)

;;(add-to-list 'ac-modes 'nrepl-mode)
(setq nrepl-popup-stacktraces nil)

(eval-after-load "auto-complete" '(add-to-list 'ac-modes 'nrepl-mode))

(defun set-auto-complete-as-completion-at-point-function ()
    (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

(add-hook 'nrepl-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'nrepl-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)

(add-hook 'nrepl-mode-hook 'paredit-mode)

;; always open in the same window
(setq ns-pop-up-frames nil)

(setq js-indent-level 2)

;; clojure mode related stuff
(setq lisp-indent-offset nil)

(add-hook 'clojure-mode-hook
    '(lambda ()
       (paredit-mode t)
       (rainbow-delimiters-mode t)
       ;;(clojure-test-mode t)
       ))

(defun connect-nrepl ()
  (interactive)
  (nrepl "127.0.0.1" "50001"))

(defun run-tests ()
  (interactive)
  (save-some-buffers 1)
  (clojure-test-run-tests))

;; always follow symlinks for version controlled files
(setq vc-follow-symlinks t)

(find-file "~/.emacs")
(switch-to-buffer "emacs.el")

;; enable global modes
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
;;(setq ido-ignore-buffers '("nrepl-connection" "*Messages*"))
(ido-mode 1)
(evil-mode 1)
(global-surround-mode 1)
(ac-config-default)
(ac-set-trigger-key "TAB")
(setq ac-auto-start nil)

(set-default 'ac-quick-help-delay 0.2)
(set-default 'ac-sources
             '(ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers))


(setq show-paren-delay 0.2)
(show-paren-mode t)

;; some ruby stuff
(add-hook 'ruby-mode-hook
    '(lambda ()
       (ruby-electric-mode t)
       (local-unset-key "\r")
       (local-set-key "\r" 'newline-and-indent) ))

;; whitespace police
(global-set-key (kbd "<f5>") 'whitespace-cleanup)
;;(define-key global-map (kbd "RET") 'newline-and-indent)
(setq show-trailing-whitespace t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun javadoc-lookup (start end)
  (interactive "r")
  (let ((q (buffer-substring-no-properties start end)))
    (if (< (length q) 100)
  (browse-url (concat "http://www.google.com/search?gfns=1&q=javadoc%20"
          (url-hexify-string q)))
      (message (format "string too long for query: %d chars | max 100" (length q))))))

;; escape sequences for terminal usage
(define-key input-decode-map "[H" [(home)])
(define-key input-decode-map "[F" [(end)])

(define-key input-decode-map "[1;5A" [(control up)])
(define-key input-decode-map "[1;5B" [(control down)])
(define-key input-decode-map "[1;5C" [(control right)])
(define-key input-decode-map "[1;5D" [(control left)])

(define-key input-decode-map "[1;3A" [(meta up)])
(define-key input-decode-map "[1;3B" [(meta down)])
(define-key input-decode-map "[1;3C" [(meta right)])
(define-key input-decode-map "[1;3D" [(meta left)])

(define-key input-decode-map "[1;2A" [(shift up)])
(define-key input-decode-map "[1;2B" [(shift down)])
(define-key input-decode-map "[1;2C" [(shift right)])
(define-key input-decode-map "[1;2D" [(shift left)])

;; special key bindings
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "<f6>") 'connect-nrepl)


(define-key evil-normal-state-map "_" 'nrepl-jump)

(defun clear-error ()
  (interactive)
  (kill-buffer "*nREPL error*"))

(define-key evil-normal-state-map "-" 'clear-error)

;;(define-key evil-insert-state-map (kbd "<S-tab>") 'ac-complete-slime)
;;(define-key evil-insert-state-map (kbd "<C-tab>") 'ac-complete-slime)
;;(define-key evil-insert-state-map (kbd "<backtab>") 'ac-complete-slime)
(define-key evil-normal-state-map ",rt" 'run-tests)
(define-key evil-visual-state-map ",d" 'javadoc-lookup)

(defun reload-code ()
  (interactive)
  (evil-normal-state)
  (save-some-buffers t)
  (nrepl-load-current-buffer))

(global-set-key (kbd "<f9>") 'reload-code)

(define-key evil-normal-state-map ",ef"      'reload-code)
(define-key evil-normal-state-map ",m"      '(lambda ()
                                                (interactive)
                                                (nrepl-switch-to-repl-buffer)))

(define-key evil-normal-state-map ",ci" 'comment-or-uncomment-region)
(define-key evil-normal-state-map ",q" 'evil-quit)
(define-key evil-normal-state-map ",t" 'peepopen-goto-file-gui)
(define-key evil-normal-state-map ",b" 'switch-to-buffer)


;; special window management
(define-key evil-normal-state-map ",h" 'evil-window-left)
(define-key evil-normal-state-map ",l" 'evil-window-right)
(define-key evil-normal-state-map ",k" 'evil-window-up)
(define-key evil-normal-state-map ",j" 'evil-window-down)

(define-key evil-normal-state-map ",." 'nrepl-jump)
(define-key evil-normal-state-map ",:" 'nrepl-jump-back)

;; emulate vim scrolling
(define-key evil-normal-state-map "\C-u" 'evil-scroll-up)
(define-key evil-normal-state-map "\C-f" 'evil-scroll-down)

;; pasting Cmd-v / M-v
(define-key evil-insert-state-map "\M-v" 'yank)
(define-key evil-normal-state-map "\M-v" 'yank)
(define-key evil-insert-state-map "\M-c" 'evil-yank)
(define-key evil-normal-state-map "\M-c" 'evil-yank)

(define-key evil-normal-state-map "\C-c\C-c" 'nrepl-eval-expression-at-point)

;; ace jump move bindings
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)
(define-key evil-normal-state-map (kbd "<S-SPC>") 'ace-jump-char-mode)
(define-key evil-visual-state-map (kbd "SPC") 'ace-jump-mode)
(define-key evil-visual-state-map (kbd "<S-SPC>") 'ace-jump-char-mode)


(defun my-nrepl-return (&optional end-of-input)
  (interactive "P")
  (cond
   ((nrepl-input-complete-p nrepl-input-start-mark (point-max))
    ;; FIXED: do not create a newline because of evil-mode problems
    (nrepl-send-input nil))
   (t
    (nrepl-newline-and-indent))))

;;(evil-define-key 'insert nrepl-mode-map (kbd "RET") 'my-nrepl-return)


(defmacro cofi/define-maybe-exit (entry-char exit-char)
  (let ((name (intern (concat "cofi/maybe-exit-"
                              (char-to-string entry-char)
                              (char-to-string exit-char)))))
    `(progn
       (define-key evil-insert-state-map (char-to-string ,entry-char) #',name)

       (evil-define-command ,name ()
         :repeat change
         (interactive)
         (let ((modified (buffer-modified-p)))
           (insert ,entry-char)
           (let ((evt (read-event (format "Insert %c to exit insert state" ,exit-char)
                                  nil 0.5)))
             (cond
              ((null evt) (message ""))
              ((and (integerp evt) (char-equal evt ,exit-char))
               (delete-char -1)
               (set-buffer-modified-p modified)
               (push 'escape unread-command-events))
              (t (setq unread-command-events (append unread-command-events
                                                     (list evt)))))))))))

(cofi/define-maybe-exit ?j ?j)
(cofi/define-maybe-exit ?k ?k)

;;
;; buffer management
;;
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(setq ido-enable-flex-matching t
      ido-max-prospects 25)
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(defun ido-disable-line-trucation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)

(add-hook 'ido-setup-hook
    (lambda ()
      (define-key ido-completion-map [down] 'ido-next-match)
      (define-key ido-completion-map [up] 'ido-prev-match)))

(setq evil-shift-width 2)
(add-hook 'after-change-major-mode-hook
    (function (lambda ()
          (setq evil-shift-width 2))))

;; some octave support
;;
(setq inferior-octave-program "~/bin/octave")
(setq ;; inferior-octave-program "/opt/local/bin/octave"
      octave-auto-indent t
      octave-auto-newline t)

(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))
(add-hook 'octave-mode-hook
  '(lambda ()
     (define-key evil-normal-state-local-map ",er" 'octave-send-region)
     (define-key evil-normal-state-local-map ",cc" 'octave-send-region)))


(auto-save-mode 0)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;;
;; javascript
;;
(setq js-indent-level 2)


;;
;; Fix Cut'n Paste + Keybindings
;;

;; enable clipboard
(setq x-select-enable-clipboard t)

;; proper handling of META for osx
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;;
;; ack
;;
;; (require 'full-ack)
;; (autoload 'ack-same "full-ack" nil t)
;; (autoload 'ack "full-ack" nil t)
;; (autoload 'ack-find-same-file "full-ack" nil t)
;; (autoload 'ack-find-file "full-ack" nil t)

;; always autosave into same file (use version control for backup)
(setq auto-save-visited-file-name t)


;;
;; cosmetic stuff
;;

;; disable decoration
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; enable color theme
(load-theme 'wombat)

;; disable emacs startup screen
(setq inhibit-splash-screen t)

;; use inconsolata as default font
;;(set-face-attribute 'default nil :font "Inconsolata-16")

;; disable for clojure
(setq font-lock-verbose nil)

;; increase window size (default size is too small)
(add-to-list 'default-frame-alist '(height . 64))
(add-to-list 'default-frame-alist '(width . 160))

(defun my-ido-project-files ()
  "Use ido to select a file from the project."
  (interactive)
  (let (my-project-root project-files tbl)
    (setq my-project-root (textmate-project-root))
    ;; get project files
    (setq project-files
    (split-string
     (shell-command-to-string
      (concat "ack " my-project-root " -f")) "\n"))
    ;; populate hash table (display repr => path)
    (setq tbl (make-hash-table :test 'equal))
    (let (ido-list)
      (mapc (lambda (path)
        ;; format path for display in ido list
        ;; strip project root
        (setq key (replace-regexp-in-string (concat my-project-root) "" path))
        ;; remove trailing | or /
        ;; (setq key (replace-regexp-in-string "\\(|\\|/\\)$" "" key))
        (puthash key path tbl)
        (push key ido-list)
        )
      project-files
      )
      (find-file (gethash (ido-completing-read "project-files: " ido-list) tbl)))))

(define-key evil-normal-state-map ",t" 'my-ido-project-files)



(cd (getenv "PWD"))

(defun yank-to-x-clipboard ()
  (interactive)
  (if (region-active-p)
      (progn
        (shell-command-on-region (region-beginning) (region-end) "xsel -i")
        (message "Yanked region to clipboard!")
        (deactivate-mark))
    (message "No region active; can't yank to clipboard!")))

(global-set-key [f8] 'yank-to-x-clipboard)



;; http://hugoheden.wordpress.com/2009/03/08/copypaste-with-emacs-in-terminal/
;; I prefer using the "clipboard" selection (the one the
;; typically is used by c-c/c-v) before the primary selection
;; (that uses mouse-select/middle-button-click)
(setq x-select-enable-clipboard t)

(unless window-system
  (when (getenv "DISPLAY")
    ;; Callback for when user cuts
    (defun xsel-cut-function (text &optional push)
      ;; Insert text to temp-buffer, and "send" content to xsel stdin
      (with-temp-buffer
        (insert text)
        ;; I prefer using the "clipboard" selection (the one the
        ;; typically is used by c-c/c-v) before the primary selection
        ;; (that uses mouse-select/middle-button-click)
        (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))
    ;; Call back for when user pastes
    (defun xsel-paste-function()
      ;; Find out what is current selection by xsel. If it is different
      ;; from the top of the kill-ring (car kill-ring), then return
      ;; it. Else, nil is returned, so whatever is in the top of the
      ;; kill-ring will be used.
      (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
        (unless (string= (car kill-ring) xsel-output)
          xsel-output )))
    ;; Attach callbacks to hooks
    (setq interprogram-cut-function 'xsel-cut-function)
    (setq interprogram-paste-function 'xsel-paste-function)
    ;; Idea from
    ;; http://shreevatsa.wordpress.com/2006/10/22/emacs-copypaste-and-x/
    ;; http://www.mail-archive.com/help-gnu-emacs@gnu.org/msg03577.html
    ))
