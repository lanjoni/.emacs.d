;; automatic customization
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; some defaults (https://sam217pa.github.io/2016/09/02/how-to-build-your-own-spacemacs/)
(setq delete-old-versions -1) ; delete excess backup versions silently
(setq version-control t) ; use version control
(setq vc-make-backup-files nil)	; make backups file even when in version controlled dir
(setq make-backup-files nil)
(setq backup-directory-alist `(("." . "~/.emacs.d/backups"))) ; which directory to put backups file
(setq vc-follow-symlinks t)	; don't ask for confirmation when opening symlinked file
;; (setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t))) ;transform backups file name
;; disable auto-save for now
(setq auto-save-default nil)
(auto-save-mode -1)
(setq inhibit-startup-screen t)	; inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore) ; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8) ; use utf-8 by default
(setq coding-system-for-write 'utf-8)
(setq sentence-end-double-space nil) ; sentence SHOULD end with only a point.
(setq default-fill-column 80)	; toggle wrapping text at the 80th character
(setq initial-scratch-message "Welcome to Emacs") ; print a default message in the empty scratch buffer opened at startup

(require 'package)

;; Use Package

(setq package-enable-at-startup nil) ; tells emacs not to load any packages before starting up
;; the following lines tell emacs where on the internet to look up
;; for new packages.
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents) ; updage packages archive
  (package-install 'use-package)) ; and install the most recent version of use-package

(eval-when-compile
  (require 'use-package))

(use-package general
  :ensure t
  :config (general-define-key "C-'" 'avy-goto-word-1))

(use-package avy
  :ensure t
  :commands (avy-goto-word-1))

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package flx
  :ensure t)

(use-package pinentry
  :ensure t
  :defer t
  :config
  (pinentry-start))

(use-package magit
  :ensure t
  :defer t
  :bind (("C-c g" . magit-status)
         ("C-x m" . magit)))

(use-package ivy
  :ensure t
  :config (ivy-mode)
  :init (setq enable-recursive-minibuffers t))

(use-package projectile
  :ensure t
  :init (projectile-mode +1)
  :config
  (setq projectile-completion-system 'ivy)
  (add-to-list 'projectile-globally-ignored-directories ".clj-kondo"))

(use-package helm
  :ensure t)

(use-package helm-projectile
  :ensure t
  :config (helm-projectile-on))

(use-package paredit
  :ensure t)

(use-package cider
  :defer t
  :ensure t
  :config
  (setq cider-repl-buffer-size-limit 10000)
  (setq cider-repl-use-clojure-font-lock nil)
  (defun cider--maybe-inspire-on-connect ()
    "Display an inspiration connection message."
    (when cider-connection-message-fn
      (message "Connected! %s" (funcall cider-connection-message-fn))))

  (define-key cider-mode-map (kbd "C-x C-e") #'cider-eval-sexp-up-to-point)
  (define-key cider-mode-map (kbd ", e e")   #'cider-eval-sexp-up-to-point)
  (define-key cider-mode-map (kbd ", e b")   #'cider-load-buffer)
  (define-key cider-mode-map (kbd ", r r")   #'cider-switch-to-repl-buffer)
  (define-key cider-mode-map (kbd ", n s")   #'cider-repl-set-ns))

(use-package clojure-mode
  :defer t
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'lsp-install-save-hooks)
  (add-hook 'clojure-mode-hook #'cider-auto-test-mode)
  (add-hook 'cider-connected-hook #'cider--maybe-inspire-on-connect))

(use-package evil-collection
  :ensure t
  :defer t
  :config (evil-collection-init))

(use-package evil
  :ensure t
  :config (evil-mode)
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil))

;; lsp
(use-package company
  :defer t
  :ensure t)

(use-package lsp-mode
  :ensure t
  :defer t
  :hook ((clojure-mode . lsp)
         (clojurec-mode . lsp)
         (clojurescript-mode . lsp))
  :init
  (defun lsp-install-save-hooks ()
    "Hooks for lsp interaction."
    (progn
      (add-hook 'before-save-hook #'lsp-format-buffer t t)
      (add-hook 'after-save-hook #'lsp-organize-imports t t)
      (lsp))))

(use-package lsp-ui
  :defer t
  :ensure t
  :commands lsp-ui-mode)

;; tremacs
(use-package treemacs
  :defer t
  :ensure t
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)

  (setq treemacs-position 'left))

(use-package treemacs-nerd-icons
  :defer t
  :ensure t
  :config (treemacs-load-theme "nerd-icons"))

(use-package treemacs-evil
  :defer t
  :ensure t
  :after (treemacs evil))

(use-package treemacs-projectile
  :defer t
  :ensure t
  :after (treemacs projectile))

(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))

;; keybinds
;; better window navigation
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

(define-key evil-motion-state-map (kbd ",") nil)

;; Rebind `evil-repeat-find-char-reverse` to another key, e.g., ";"
(define-key evil-motion-state-map (kbd ";") 'evil-repeat-find-char-reverse)

;; treemacs
(define-key global-map (kbd "M-t") 'treemacs)

;; copilot
(add-to-list 'load-path "~/.emacs.d/copilot.el")
(require 'copilot)

(add-hook 'prog-mode-hook 'copilot-mode)

(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

(add-to-list 'copilot-major-mode-alist '("clojure"))
(add-to-list 'copilot-indentation-alist '(clojure-mode 2))
(add-to-list 'copilot-indentation-alist '(emacs-lisp-mode . 2))

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

;; theme
(setq custom-theme-directory (concat user-emacs-directory "themes"))
(dolist
    (path (directory-files custom-theme-directory t "\\w+"))
  (when (file-directory-p path)
    (add-to-list 'custom-theme-load-path path)))

(defun use-default-theme ()
  (interactive)
  (load-theme 'default-black))
(use-default-theme)

;; font-face
(set-face-attribute
 'default nil
 :font "Monaco"
 :height 150)
(put 'downcase-region 'disabled nil)

(provide 'init)

;;; init.el ends here
