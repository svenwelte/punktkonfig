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


(add-to-list 'load-path "~/.emacs.d/bundle/solarized")

(vendor 'color-theme)
(vendor 'color-theme-ir-black)
(vendor 'clojure-mode)
(vendor 'slime)
(vendor 'paredit)
(vendor 'full-ack)
(vendor 'undo-tree)
(vendor 'evil)
(vendor 'surround)
(vendor 'textmate)
;; (vendor 'haml-mode)
;; (vendor 'sass-mode)
(load "~/.emacs.d/vendor/peepopen.el")

(add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)

;; always follow symlinks for version controlled files
(setq vc-follow-symlinks t)

(find-file "~/.emacs")
(switch-to-buffer ".emacs")

;; enable global modes
(ido-mode 1)
(evil-mode 1)
(global-surround-mode 1)

;; whitespace police
(global-set-key (kbd "<f5>") 'whitespace-cleanup)
(setq show-trailing-whitespace t)

;; special key bindings
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
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
