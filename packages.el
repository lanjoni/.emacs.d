;;; package --- Summary
;;; Commentary:
;;
;; lanjoni's Emacs configuration.
;;
;; file: ~/.emacs.d/packages.el
;; author: João Augusto Lissoni Lanjoni <joaoaugustolanjoni@gmail.com>
;; date: Oct 29, 2023

(add-to-list 'load-path (expand-file-name "~/.emacs.d/plugins"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/themes"))

;;; Code:

(require 'package)
(require 'auto-complete)
(require 'corfu)
(require 'flycheck-clojure)
(require 'projectile)
(require 'all-the-icons)
(require 'lsp-mode)
(require 'evil)

;; Visual requires
(require 'awesome-tab)
(require 'default-black-theme)
(require 'prez-theme)

(custom-set-variables
 '(package-archives
   (quote (("gnu" . "https://elpa.gnu.org/packages/")
           ("melpa" . "https://melpa.org/packages/")))))

(package-initialize)

;; Debugging
(defun lsp-install-save-hooks ()
  "Hooks for golang debugging."
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t)
  (lsp))

(add-hook 'go-mode-hook #'lsp-install-save-hooks)
(add-hook 'c-mode-hook #'lsp-install-save-hooks)
(add-hook 'cpp-mode-hook #'lsp-install-save-hooks)
(add-hook 'php-mode-hook #'lsp-install-save-hooks)
(add-hook 'elixir-mode-hook #'lsp-install-save-hooks)
(add-hook 'scala-mode-hook #'lsp-install-save-hooks)
(add-hook 'clojure-mode-hook #'lsp-install-save-hooks)

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
               '(crystal-mode . "crystal"))
  (lsp-register-client
  (make-lsp-client :new-connection (lsp-stdio-connection '("crystalline"))
                   :activation-fn (lsp-activate-on "crystal")
                   :priority '1
                   :server-id 'crystalline)))

(lsp-register-custom-settings
 '(("gopls.completeUnimported" t t)
   ("gopls.staticcheck" t t)))

;; vendors
(load-file "~/.emacs.d/splash-screen.el")

(provide 'lanjoni-packages)
;;; packages.el ends here
