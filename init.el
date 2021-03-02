(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)

(exec-path-from-shell-initialize)

;; eshell
(defun m-eshell-hook ()
  (define-key eshell-mode-map [(control p)] 'eshell-previous-matching-input-from-input)
  (define-key eshell-mode-map [(control n)] 'eshell-next-matching-input-from-input)
 
  (define-key eshell-mode-map [up] 'previous-line)
  (define-key eshell-mode-map [down] 'next-line))
(add-hook 'eshell-mode-hook 'm-eshell-hook)

;; shell
(defun m-shell-hook ()
  (define-key shell-mode-map [(control p)] 'comint-previous-command)
  (define-key shell-mode-map [(control n)] 'comint-next-command))

;; theme
(load-theme 'monokai t)

;; linum
(global-linum-mode 1)
(add-hook 'eshell-mode-hook (lambda () (linum-mode -1)))
(add-hook 'shell-mode-hook (lambda () (linum-mode -1)))

;; disable
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq make-backup-files nil)
(setq auto-save-default nil)

(global-set-key (kbd "<f5>") 'eval-buffer)
(global-set-key (kbd "C-h") 'delete-backward-char)

(use-package rbenv
  :config
  (global-rbenv-mode)
  (setq rbenv-installation-dir "~/.anyenv/envs/rbenv"))

;; ido
(ido-mode 1)
(ido-everywhere 1)

;; all-the-icons
(use-package all-the-icons)

;; neotree
(use-package neotree
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  :bind
  ("C-t" . neotree-toggle)
  ("C-S-h" . neotree-hidden-file-toggle))

;; centaur-tabs
(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  :bind
  ("<C-iso-lefttab>" . centaur-tabs-backward)
  ("C-<tab>" . centaur-tabs-forward))

;; powerline
(use-package powerline
  :config
  (powerline-default-theme))

;; dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (monokai-theme spacemacs-theme rbenv dashboard use-package neotree markdown-preview-mode markdown-preview-eww markdown-mode+ magit exec-path-from-shell centaur-tabs all-the-icons))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
