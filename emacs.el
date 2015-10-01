(require 'package)
;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(when (not package-archive-contents)
    (package-refresh-contents))

(defvar my-packages
    '(clojure-mode
      evil
      evil-paredit
      haml-mode
      magit
      markdown-mode
      paredit
      projectile
      ack-and-a-half
      flx
      flx-ido
      sass-mode
      yaml-mode evil
      auto-complete
      ac-nrepl
      ace-jump-mode
      rainbow-delimiters
      cider
      key-chord
      soothe-theme)
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

;; teach dired to always use alternate file on return
(add-hook 'dired-mode-hook 'my-dired-mode-hook)
(defun my-dired-mode-hook ()
  (evil-add-hjkl-bindings dired-mode-map 'normal
    (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "RET")
    'dired-find-alternate-file))

(projectile-global-mode t)

(setq evil-auto-indent t)
(evil-mode t)
(global-auto-complete-mode t)
(require 'evil-org)

(setq cider-repl-shortcut-dispatch-char ?\:)
(defun startup-nrepl ()
  (ac-nrepl-setup)
  (rainbow-delimiters-mode t))

;;(add-hook 'cider-mode-hook 'startup-nrepl)
;;(add-hook 'cider-interaction-mode-hook 'startup-nrepl)

(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(add-to-list 'ac-modes 'cider-repl-mode)

(setq cider-popup-stacktraces nil)

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)


(defun set-auto-complete-as-completion-at-point-function ()
    (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

;(add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)
;(add-hook 'cider-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)

(add-hook 'cider-repl-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-interaction-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-mode-hook 'paredit-mode)

;; always open in the same window
(setq ns-pop-up-frames nil)

(setq js-indent-level 2)

;; clojure mode related stuff
(setq lisp-indent-offset nil)
;(setq clojure-defun-style-default-indent t)

(add-hook 'clojure-mode-hook
    '(lambda ()
       (paredit-mode t)
       (evil-paredit-mode t)
       (rainbow-delimiters-mode t)))

(defun connect-nrepl ()
  (interactive)
  (cider "127.0.0.1" "50001"))

(defun run-tests ()
  (interactive)
  (save-some-buffers 1)
  (clojure-test-run-tests))

;; always follow symlinks for version controlled files
(setq vc-follow-symlinks t)

;; enable global modes
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
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

;; special completion for *.el
(defun my-lisp-mode ()
  (message "Applying custom completions for lisp mode")
  (setq ac-sources '(ac-source-symbols
                     ac-source-abbrev
                     ac-source-functions
                     ac-source-features
                     ac-source-variables
                     ac-source-words-in-same-mode-buffers))
  (paredit-mode t))
(add-hook 'emacs-lisp-mode-hook 'my-lisp-mode)


;; some ruby stuff
(add-hook 'ruby-mode-hook
    '(lambda ()
       (ruby-electric-mode t)
       (local-unset-key "\r")
       (local-set-key "\r" 'newline-and-indent) ))

;; whitespace police
(global-set-key (kbd "<f5>") 'whitespace-cleanup)

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


(define-key evil-normal-state-map "_" 'cider-jump)

(defun clear-error ()
  (interactive)
  (kill-buffer "*nREPL error*"))

(define-key evil-normal-state-map "-" 'clear-error)

(define-key evil-normal-state-map ",rt" 'run-tests)
;(define-key evil-visual-state-map ",d" 'javadoc-lookup)

(defun reload-code ()
  (interactive)
  (evil-normal-state)
  (save-some-buffers t)
  (cider-load-current-buffer))

(global-set-key (kbd "<f9>") 'reload-code)

(evil-ex-define-cmd "cn[ext]" 'next-error)
(evil-ex-define-cmd "cp[rev]" 'previous-error)
(define-key evil-normal-state-map ",n" 'next-error)

(define-key evil-normal-state-map ",ef" 'reload-code)
(define-key evil-normal-state-map ",ci" 'comment-or-uncomment-region)
(define-key evil-normal-state-map ",q" 'evil-quit)
(define-key evil-normal-state-map ",Q" 'kill-this-buffer)
(define-key evil-normal-state-map ",a" 'projectile-ack)
(define-key evil-normal-state-map ",t" 'projectile-find-file)
(define-key evil-normal-state-map ",d" 'projectile-find-dir)
(define-key evil-normal-state-map ",b" 'switch-to-buffer)


;; special window management
(define-key evil-normal-state-map ",h" 'evil-window-left)
(define-key evil-normal-state-map ",l" 'evil-window-right)
(define-key evil-normal-state-map ",k" 'evil-window-up)
(define-key evil-normal-state-map ",j" 'evil-window-down)

(define-key evil-normal-state-map ",r" 'cider-switch-to-relevant-repl-buffer)

(define-key evil-normal-state-map ",=" 'cider-repl-set-ns)
(define-key evil-normal-state-map ",." 'cider-jump)
(define-key evil-normal-state-map ",:" 'cider-jump-back)

;; emulate vim scrolling
(define-key evil-normal-state-map "\C-u" 'evil-scroll-up)
(define-key evil-normal-state-map "\C-f" 'evil-scroll-down)

;; pasting Cmd-v / M-v
(define-key evil-visual-state-map ",y" 'yank-to-x-clipboard)
(define-key evil-visual-state-map "\M-c" 'yank-to-x-clipboard)
(define-key evil-insert-state-map "\M-v" 'yank)
(define-key evil-normal-state-map "\M-v" 'yank)

(define-key evil-normal-state-map "\C-c\C-c" 'cider-eval-expression-at-point)

;; ace jump move bindings
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)
(define-key evil-normal-state-map (kbd "<S-SPC>") 'ace-jump-char-mode)
(define-key evil-visual-state-map (kbd "SPC") 'ace-jump-mode)
(define-key evil-visual-state-map (kbd "<S-SPC>") 'ace-jump-char-mode)

;; exit vim mode on special key combos
(setq key-chord-two-keys-delay 0.3)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define evil-insert-state-map "kj" 'evil-normal-state)
(key-chord-define evil-insert-state-map "kk" 'evil-normal-state)
(key-chord-mode 1)


;;
;; buffer management
;;
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; make ido results vertical
(setq ido-enable-flex-matching t
      ido-max-prospects 25)
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(defun ido-disable-line-trucation ()
  (set (make-local-variable 'truncate-lines) nil))

(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)

(add-hook 'ido-setup-hook
    (lambda ()
      (define-key ido-completion-map (kbd "ESC") 'keyboard-escape-quit)
      (define-key ido-completion-map [down] 'ido-next-match)
      (define-key ido-completion-map [up] 'ido-prev-match)))

(add-hook 'org-mode-hook
  (lambda ()
    (define-key org-mode-map [tab] 'org-mode-cycle)))

(setq evil-shift-width 2)
(add-hook 'after-change-major-mode-hook
    (function (lambda ()
          (setq evil-shift-width 2))))


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
(tool-bar-mode -1)
(menu-bar-mode -1)

;; enable color theme
(load-theme 'soothe t)
;(load-theme 'wombat t)

;; disable emacs startup screen
(setq inhibit-splash-screen t)

;; use inconsolata as default font
;;(set-face-attribute 'default nil :font "Inconsolata-16")

;; disable for clojure
;(setq font-lock-verbose nil)

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

;(define-key evil-normal-state-map ",t" 'my-ido-project-files)




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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Linum-format "%7i ")
 '(ansi-color-names-vector ["#000000" "#B13120" "#719F34" "#CEAE3E" "#7C9FC9" "#7868B5" "#009090" "#F4EAD5"])
 '(custom-safe-themes (quote ("f89e21c3aef10d2825f2f079962c2237cd9a45f4dc1958091be8a6f5b69bb70c" "e80a0a5e1b304eb92c58d0398464cd30ccbc3622425b6ff01eea80e44ea5130e" "787574e2eb71953390ed2fb65c3831849a195fd32dfdd94b8b623c04c7f753f0" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(fci-rule-character-color "#202020")
 '(fci-rule-color "#202020")
 '(fringe-mode 4 nil (fringe))
 '(main-line-color1 "#000000")
 '(main-line-color2 "#000000")
 '(main-line-separator-style (quote chamfer))
 '(powerline-color1 "#000000")
 '(powerline-color2 "#000000"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)

(find-file "~/.emacs")
(switch-to-buffer "emacs.el")

(cd (getenv "PWD"))
