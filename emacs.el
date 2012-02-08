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

(add-to-list 'exec-path "/opt/local/bin")
(setenv "PATH" (concat (getenv "PATH") ":/opt/local/bin"))

;;(vendor 'color-theme)

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
(vendor 'ruby-electric)

(add-to-list 'load-path "~/.emacs.d/bundle/color-theme.local")
(require 'color-theme)
(vendor 'color-theme-ir-black)

(vendor 'haml-mode)
(vendor 'sass-mode)
(vendor 'json-mode)

(load "~/.emacs.d/vendor/peepopen.el")
(load "~/.emacs.d/vendor/cdargs.el")
(load "~/.emacs.d/bundle/clojure-mode/clojure-test-mode.el")
(load "~/.emacs.d/bundle/auto-complete/auto-complete-config.el")

;; always open in the same window
(setq ns-pop-up-frames nil)

(setq js-indent-level 2)

;; clojure mode related stuff
(setq lisp-indent-offset nil)
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

;; some ruby stuff
(add-hook 'ruby-mode-hook
	  '(lambda ()
	     (ruby-electric-mode t)
	     (local-unset-key "\r")
	     (local-set-key "\r" 'newline-and-indent) ))

;; whitespace police
(global-set-key (kbd "<f5>") 'whitespace-cleanup)
(define-key global-map (kbd "RET") 'newline-and-indent)
(setq show-trailing-whitespace t)

(defun javadoc-lookup (start end)
  (interactive "r")
  (let ((q (buffer-substring-no-properties start end)))
    (if (< (length q) 100)
	(browse-url (concat "http://www.google.com/search?gfns=1&q=javadoc%20"
			    (url-hexify-string q)))
      (message (format "string too long for query: %d chars | max 100" (length q))))))

;; escape sequences for terminal usage
;(define-key input-decode-map "[1;5C" [(control right)])
;(define-key input-decode-map "[1;5D" [(control left)])

;; special key bindings
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "<f6>") 'swank)
(define-key evil-insert-state-map (kbd "<S-tab>") 'ac-complete-slime)
(define-key evil-insert-state-map (kbd "<C-tab>") 'ac-complete-slime)
(define-key evil-normal-state-map ",rt" 'run-tests)
(define-key evil-visual-state-map ",d" 'javadoc-lookup)
(define-key evil-normal-state-map ",ef" '(lambda ()
					   (interactive)
					   (save-some-buffers t)
					   (slime-compile-and-load-file)))
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
(define-key evil-normal-state-map "\C-f" 'evil-scroll-down)

;; pasting Cmd-v / M-v
(define-key evil-insert-state-map "\M-v" 'yank)
(define-key evil-normal-state-map "\M-v" 'yank)
(define-key evil-insert-state-map "\M-c" 'evil-yank)
(define-key evil-normal-state-map "\M-c" 'evil-yank)

;; ace jump move bindings
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)
(define-key evil-normal-state-map (kbd "<S-SPC>") 'ace-jump-char-mode)
(define-key evil-visual-state-map (kbd "SPC") 'ace-jump-mode)
(define-key evil-visual-state-map (kbd "<S-SPC>") 'ace-jump-char-mode)


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

(setq ido-enable-flex-matching t)
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
;;(set-face-attribute 'default nil :font "Inconsolata-16")

;; disable for clojure
(setq font-lock-verbose nil)

;; increase window size (default size is too small)
(add-to-list 'default-frame-alist '(height . 64))
(add-to-list 'default-frame-alist '(width . 160))

(defun cv ()
  (interactive)
  (cdargs))

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