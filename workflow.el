;;; package -- Summary
;;; Commentary:
;;
;; lanjoni's Emacs configuration
;;
;; file: ~/.emacs.d/init.el
;; author: João Augusto Lissoni Lanjoni <joaoaugustolanjoni@gmail.com>
;; Oct 29, 2023

;;; Code:

(setq make-backup-files nil)
(setq create-lockfiles nil)

;; Auto refresh
(global-auto-revert-mode t)
(setq-default auto-revert-use-notify nil)

;; Identation & coding style
(setq-default default-tab-width 2)
(setq-default tab-width 2)
(setq-default tab-always-indent t)
(setq-default indent-tabs-mode nil)
(setq-default c-default-style "bsd"
              c-basic-offset 2
              nasm-basic-offset 2
              python-default-offset 2
              python-indent-offset 2
              python-default-offset 2
              python-indent 2
              css-indent-offset 2
              json-encoding-default-indentation 2
              elm-indent-offset 2
              js-indent-level 2
              rust-indent-offset 2)

(define-key global-map (kbd "C-k") 'previous-line)
(define-key global-map (kbd "C-h") 'backward-char)
(define-key global-map (kbd "C-j") 'next-line)
(define-key global-map (kbd "C-l") 'forward-char)

(define-key global-map (kbd "M-t") 'treemacs)
(define-key global-map (kbd "M-f") 'treemacs-project-follow-mode)

;; Zoom
(define-key global-map (kbd "C-=") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; CTRL + Z and CTRL + Shift + Z
(define-key global-map (kbd "C-z") 'undo-only)
(define-key global-map (kbd "C-S-z") 'undo-redo)

;; Shell
(define-key global-map (kbd "C-t") 'eshell)

;; Awesome bindings
(global-set-key (kbd "s-1") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "s-2") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "s-3") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "s-4") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "s-5") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "s-6") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "s-7") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "s-8") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "s-9") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "s-0") 'awesome-tab-select-visible-tab)

;; Ctrl-c Ctrl-v
(cua-mode)

;; Evil mode (VI layer)
(evil-mode 1)

;; Removing C-k binding for correction
(eval-after-load "evil-maps"
  (dolist (map '(evil-motion-state-map
                 evil-insert-state-map
                 evil-emacs-state-map))
    (define-key (eval map) "\C-k" nil)))

;; Awesome tab
(awesome-tab-mode t)

;; Code completion
(global-flycheck-mode)
(global-company-mode t)
(auto-complete-mode t)

(provide 'lanjoni-workflow)

;;; workflow.el ends here
