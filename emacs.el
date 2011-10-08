;; For loading libraries from the vendor directory
;; Modified from defunkt's original version to support autoloading.
;; http://github.com/defunkt/emacs/blob/master/defunkt/defuns.el
(defun vendor (library &rest autoload-functions)
  (let* ((file (symbol-name library))
         (normal (concat "~/.emacs.d/bundle/" file))
         (suffix (concat normal ".el"))
         (personal (concat "~/.emacs.d/custom/" file))
         (found nil))
    (cond
     ((file-directory-p normal) (add-to-list 'load-path normal) (set 'found t))
     ((file-directory-p suffix) (add-to-list 'load-path suffix) (set 'found t))
     ((file-exists-p suffix)  (set 'found t)))
    (when found
      (if autoload-functions
          (dolist (autoload-function autoload-functions)
            (autoload autoload-function (symbol-name library) nil t))
        (require library)))
    (when (file-exists-p (concat personal ".el"))
      (load personal))))


(vendor 'color-theme)
(vendor 'color-theme-ir-black)
(vendor 'clojure-mode)
(vendor 'slime)
(vendor 'paredit)
;(vendor 'full-ack)
(vendor 'undo-tree)
(vendor 'evil)
(vendor 'surround)
(vendor 'textmate)
(vendor 'auto-complete)
(vendor 'ace-jump-mode)

(vendor 'haml-mode)
(vendor 'sass-mode)

(load "~/.emacs.d/vendor/peepopen.el")
(load "~/.emacs.d/vendor/cdargs.el")
(load "~/.emacs.d/bundle/clojure-mode/clojure-test-mode.el")
(load "~/.emacs.d/bundle/auto-complete/auto-complete-config.el")

;; always open in the same window
(setq ns-pop-up-frames nil)


;; clojure mode related stuff
(setq slime-protocol-version 'ignore)
(setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol
      slime-net-coding-system 'utf-8-unix)

(slime-setup
      '(slime-repl slime-banner slime-fuzzy))

(add-hook 'clojure-mode-hook
          '(lambda ()
             (paredit-mode t)
             (clojure-test-mode t)))

(add-hook 'slime-repl-mode-hook
          '(lambda ()
             (paredit-mode t)
             (clojure-mode-font-lock-setup)))

(defun swank ()
  (interactive)
  (slime-connect "127.0.0.1" "4005"))

(defun run-tests ()
  (interactive)
  (save-some-buffers 1)
  (clojure-test-run-tests))

;; always follow symlinks for version controlled files
(setq vc-follow-symlinks t)

(find-file "~/.emacs")
(switch-to-buffer "emacs.el")

;; enable global modes
(ido-mode 1)
(evil-mode 1)
(global-surround-mode 1)
(ac-config-default)

(setq show-paren-delay 0.2)
(show-paren-mode t)

;; whitespace police
(global-set-key (kbd "<f5>") 'whitespace-cleanup)
(setq show-trailing-whitespace t)

(defun javadoc-lookup (start end)
  (interactive "r")
  (let ((q (buffer-substring-no-properties start end)))
    (if (< (length q) 100)
        (browse-url (concat "http://www.google.com/search?gfns=1&q=javadoc%20"
                            (url-hexify-string q)))
      (message (format "string too long for query: %d chars | max 100" (length q))))))

;; special key bindings
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "<f6>") 'swank)
(define-key evil-insert-state-map (kbd "<S-tab>") 'ac-complete-slime)
(define-key evil-normal-state-map ",rt" 'run-tests)
(define-key evil-visual-state-map ",d" 'javadoc-lookup)
(define-key evil-normal-state-map ",ef" 'slime-compile-and-load-file)
(define-key evil-normal-state-map ",ci" 'comment-or-uncomment-region)
(define-key evil-normal-state-map ",q" 'evil-quit)
(define-key evil-normal-state-map ",t" 'peepopen-goto-file-gui)
(define-key evil-normal-state-map ",b" 'switch-to-buffer)


;; special window management
(define-key evil-normal-state-map ",h" 'evil-window-left)
(define-key evil-normal-state-map ",l" 'evil-window-right)
(define-key evil-normal-state-map ",k" 'evil-window-up)
(define-key evil-normal-state-map ",j" 'evil-window-down)

;; emulate vim scrolling
(define-key evil-normal-state-map "\C-u" 'evil-scroll-up)
(define-key evil-normal-state-map "\C-f" 'evil-scroll-down)

;; pasting Cmd-v / M-v
(define-key evil-insert-state-map "\M-v" 'yank)
(define-key evil-normal-state-map "\M-v" 'yank)

;; ace jump move bindings
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)
(define-key evil-normal-state-map (kbd "<S-SPC>") 'ace-jump-char-mode)
(define-key evil-visual-state-map (kbd "SPC") 'ace-jump-mode)
(define-key evil-visual-state-map (kbd "<S-SPC>") 'ace-jump-char-mode)

(auto-save-mode 0)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)


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

;; do not make backspace on DEL
(normal-erase-is-backspace-mode 1)

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
(color-theme-ir-black)

;; disable emacs startup screen
(setq inhibit-splash-screen t)

;; use inconsolata as default font
(set-face-attribute 'default nil :font "Inconsolata-12")

;; disable for clojure
(setq font-lock-verbose nil)

;; increase window size (default size is too small)
(add-to-list 'default-frame-alist '(height . 64))
(add-to-list 'default-frame-alist '(width . 160))

(defun cv ()
  (interactive)
  (cdargs))

