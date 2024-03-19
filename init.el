;; Author: João Lanjoni <joaoaugustolanjoni@gmail.com>
;; URL: https://github.com/lanjoni/.emacs.d
;; Keywords: emacs config

;;; License:

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;; automatic customization
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; packages
(require 'package)

(custom-set-variables
 '(package-archives
   (quote (("gnu" . "https://elpa.gnu.org/packages/")
	         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
           ("melpa" . "https://melpa.org/packages/")))))

(unless package-archive-contents
  (progn
    (package-initialize)))

(defun install-ifna (pkg)
  "Install PKG if not already."
  (unless (package-installed-p pkg)
    (package-install pkg)))

(install-ifna 'all-the-icons)

(install-ifna 'corfu)
(add-hook 'prog-mode-hook #'corfu-mode)

(install-ifna 'lsp-mode)
(defun lsp-install-save-hooks ()
  "Hooks for lsp interaction."
  (progn
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'after-save-hook #'lsp-organize-imports t t)
    (lsp)))

(add-hook 'clojure-mode-hook #'lsp-install-save-hooks)

(install-ifna 'cider)
(add-hook 'clojure-mode-hook #'cider-auto-test-mode)
(setq cider-repl-buffer-size-limit 10000)
(setq cider-repl-use-clojure-font-lock nil)
(defun cider--maybe-inspire-on-connect ()
  "Display an inspiration connection message."
  (when cider-connection-message-fn
    (message "Connected! %s" (funcall cider-connection-message-fn))))

(add-hook 'cider-connected-hook #'cider--maybe-inspire-on-connect)

(install-ifna 'magit)
(global-set-key (kbd "C-c g") #'magit-status)
(global-set-key (kbd "C-x m") #'magit)

(install-ifna 'clojure-mode)

(install-ifna 'elcord)
(elcord-mode)

(install-ifna 'modus-themes)
(install-ifna 'doom-themes)
(load-theme 'doom-ir-black t)

(install-ifna 'evil)
(evil-mode 1)

(install-ifna 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(electric-pair-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-default 'truncate-lines t)
(global-display-line-numbers-mode 0)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)

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

(setq-default default-tab-width 2)
(setq-default tab-width 2)
(setq-default tab-always-indent t)
(setq-default indent-tabs-mode nil)

(cua-mode) ;; ctrl-c ctrl-v

(set-face-attribute
 'default nil
 :font "Monaco"
 :height 150)
(put 'downcase-region 'disabled nil)

(provide 'init)

;;; init.el ends here
