;; automatic customization
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; some defaults (https://sam217pa.github.io/2016/09/02/how-to-build-your-own-spacemacs/)
(setq delete-old-versions -1) ; delete excess backup versions silently
(setq version-control t) ; use version control
(setq vc-make-backup-files t)	; make backups file even when in version controlled dir
(setq backup-directory-alist `(("." . "~/.emacs.d/backups"))) ; which directory to put backups file
(setq vc-follow-symlinks t)	; don't ask for confirmation when opening symlinked file
;; (setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t))) ;transform backups file name
;; disable auto-save for now
(setq auto-save-default nil)
(setq inhibit-startup-screen t)	; inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore) ; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8) ; use utf-8 by default
(setq coding-system-for-write 'utf-8)
(setq sentence-end-double-space nil) ; sentence SHOULD end with only a point.
(setq default-fill-column 80)	; toggle wrapping text at the 80th character
(setq initial-scratch-message "Welcome to Emacs") ; print a default message in the empty scratch buffer opened at startup

;; packages
(require 'package)

(custom-set-variables
 '(package-archives
   (quote (("gnu"    . "https://elpa.gnu.org/packages/")
	         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
           ("melpa"  . "https://melpa.org/packages/")))))

(setq custom-theme-directory (concat user-emacs-directory "themes"))
(dolist
    (path (directory-files custom-theme-directory t "\\w+"))
  (when (file-directory-p path)
    (add-to-list 'custom-theme-load-path path)))

(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

(unless package-archive-contents
  (progn
    (package-initialize)))

(defun install-ifna (pkg)
  "Install PKG if not already."
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; general packages
(install-ifna 'corfu)
(add-hook 'prog-mode-hook #'corfu-mode)

(install-ifna 'lsp-mode)
(defun lsp-install-save-hooks ()
  "Hooks for lsp interaction."
  (progn
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'after-save-hook #'lsp-organize-imports t t)
    (lsp)))

;; magit
(install-ifna 'magit)
(global-set-key (kbd "C-c g") #'magit-status)
(global-set-key (kbd "C-x m") #'magit)

;; elcord - for discord
(install-ifna 'elcord)
(elcord-mode)

;; ivy
(install-ifna 'ivy)
(ivy-mode)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

;; helm
(install-ifna 'helm)
(require 'helm)

(install-ifna 'helm-projectile)
(require 'helm-projectile)
(helm-projectile-on)

;; projectile
(install-ifna 'projectile)
(require 'projectile)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(projectile-mode +1)
(setq projectile-completion-system 'ivy)

;; paredit
(install-ifna 'paredit)
(require 'paredit)

;; evil-mode
;; note: my goal if I focus on
;; emacs usage is to disable this...
(setq evil-want-integration t)
(setq evil-want-keybinding nil)

(install-ifna 'evil)
(evil-mode 1)

(install-ifna 'evil-collection)
(evil-collection-init)

;; simple window navigation
(define-key evil-normal-state-map (kbd "C-h") #'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") #'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") #'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") #'evil-window-right)

;; manage buffers
(define-key evil-normal-state-map (kbd "SPC k") #'kill-buffer)
(define-key evil-normal-state-map (kbd "SPC b") #'ivy-switch-buffer)

;; paredit
(define-key evil-normal-state-map (kbd ", w") #'paredit-wrap-round)
(define-key evil-normal-state-map (kbd ", q") #'paredit-wrap-curly)
(define-key evil-normal-state-map (kbd ", k") #'paredit-wrap-square)

;; lsp
(define-key evil-normal-state-map (kbd "g d") #'lsp-find-definition)
(define-key evil-normal-state-map (kbd "K") #'lsp-describe-thing-at-point)
(define-key evil-normal-state-map (kbd "SPC l n") #'lsp-rename)
(define-key evil-normal-state-map (kbd "SPC l f") #'lsp-format-buffer)

;; helm
(define-key evil-normal-state-map (kbd "SPC f f") #'helm-projectile-find-file)
(define-key evil-normal-state-map (kbd "SPC f g") #'helm-do-grep-ag)

;; Ensure evil-mode is loaded before modifying key bindings
(with-eval-after-load 'evil
  ;; Unbind `,` from `evil-repeat-find-char-reverse` in evil motion state
  (define-key evil-motion-state-map (kbd ",") nil)

  ;; Rebind `evil-repeat-find-char-reverse` to another key, e.g., ";"
  (define-key evil-motion-state-map (kbd ";") 'evil-repeat-find-char-reverse))

;; clojure
(install-ifna 'cider)
(install-ifna 'clojure-mode)

(setq cider-repl-buffer-size-limit 10000)
(setq cider-repl-use-clojure-font-lock nil)
(defun cider--maybe-inspire-on-connect ()
  "Display an inspiration connection message."
  (when cider-connection-message-fn
    (message "Connected! %s" (funcall cider-connection-message-fn))))

(add-hook 'clojure-mode-hook #'lsp-install-save-hooks)
(add-hook 'clojure-mode-hook #'cider-auto-test-mode)
(add-hook 'cider-connected-hook #'cider--maybe-inspire-on-connect)

;; change evaluation for evil-mode
(with-eval-after-load 'clojure-mode
  (define-key cider-mode-map (kbd "C-c C-e") #'cider-eval-sexp-up-to-point)
  (define-key cider-mode-map (kbd "C-x C-e") #'cider-eval-sexp-up-to-point)
  (define-key cider-mode-map (kbd ", e e") #'cider-eval-sexp-up-to-point)

  (define-key cider-mode-map (kbd ", e b") #'cider-load-buffer)
  (define-key cider-mode-map (kbd ", r r") #'cider-switch-to-repl-buffer)
  (define-key cider-mode-map (kbd ", n s") #'cider-repl-set-ns))

;; theme configuration
(install-ifna 'modus-themes)
(install-ifna 'doom-themes)

(defun use-default-theme ()
  (interactive)
  (load-theme 'default-black))

(use-default-theme)

;; treemacs
;; note: my goal is also to
;; don't use treemacs anymore
(install-ifna 'treemacs)
(install-ifna 'treemacs-nerd-icons)

(require 'treemacs-nerd-icons)
(treemacs-load-theme "nerd-icons")

(setq treemacs-position 'left)
(treemacs-follow-mode)

(treemacs-project-follow-mode)
(define-key global-map (kbd "M-t") 'treemacs)

;; exec-path-from-shell
(install-ifna 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; utils
(electric-pair-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-default 'truncate-lines t)
(global-display-line-numbers-mode -1)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)

;; major mode
(setq-default major-mode
              (lambda () ; guess major mode from file name
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))
(setq confirm-kill-emacs #'yes-or-no-p)
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)
(defalias 'yes-or-no #'y-or-n-p)
(setq make-backup-files -1)
(setq create-lockfiles -1)

;; simple code style
(setq-default default-tab-width 2)
(setq-default tab-width 2)
(setq-default tab-always-indent t)
(setq-default indent-tabs-mode nil)

;; disable bell
(setq ring-bell-function 'ignore)

;; ctrl-c ctrl-v
(cua-mode)

;; font-face
(set-face-attribute
 'default nil
 :font "Monaco"
 :height 150)
(put 'downcase-region 'disabled nil)

(provide 'init)

;;; init.el ends here
