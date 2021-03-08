(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; initialize use-packge
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

;; linum
(global-linum-mode 1)
(add-hook 'eshell-mode-hook (lambda () (linum-mode -1)))
(add-hook 'shell-mode-hook (lambda () (linum-mode -1)))

;; disable
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; custom variable
(setq custom-file (concat user-emacs-directory "/custom.el"))

;; global keybinds
(global-set-key (kbd "<f5>") 'eval-buffer)
(global-set-key (kbd "C-h") 'delete-backward-char)

;; ivy
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1))

;; ivy-ricy
(use-package ivy-rich
  :ensure t
  :config
  (ivy-rich-mode 1))

;; counsel
(use-package counsel
  :ensure t
  :config
  (counsel-mode 1)
  :bind
  ("M-x" . counsel-M-x))

;; all-the-icons
(use-package all-the-icons
  :ensure t)

;; org
(use-package org
  :ensure t)

;; avy
(use-package avy
  :ensure t
  :bind
  ("M-g l" . avy-goto-line)
  ("M-g w" . avy-goto-word-1))

;; magit
(use-package magit
  :ensure t)

;; monokai-theme
(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai t))

;; neotree
(use-package neotree
  :ensure t
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  :bind
  ("C-t" . neotree-toggle)
  ("C-S-h" . neotree-hidden-file-toggle))

;; centaur-tabs
(use-package centaur-tabs
  :ensure t
  :demand
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match)
  (setq centaur-tabs-style "bar")
  (setq centaur-tabs-set-bar 'left)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-gray-out-icons 'buffer)
  (setq centaur-tabs-set-modified-marker t)
  (setq centaur-tabs-modified-marker "◉")
  :bind
  ("<C-iso-lefttab>" . centaur-tabs-backward)
  ("C-<tab>" . centaur-tabs-forward))

;; dashboard
(use-package dashboard
  :ensure t
  :diminish
  (dashboard-mode page-break-lines-mode)
  :config
  (dashboard-setup-startup-hook))

;; doom-themes
(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-italic t)
  (doom-themes-enable-bold t)
  :config
  (load-theme 'doom-molokai t)
  (doom-themes-org-config))

;; doom-modeline
(use-package doom-modeline
  :ensure t
  :custom
  (doom-modeline-icon t)
  :config
  (doom-modeline-mode 1))

;; highlight-indent-guides
(use-package highlight-indent-guides
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :config
  (rainbow-delimiters-mode 1))

;; paren
(use-package paren
  :ensure t
  :config
  (show-paren-mode 1))

;; markdown-preview-mode
(use-package markdown-preview-mode
  :ensure t
  :config
  (setq markdown-preview-stylesheets (list "github.css")))

(use-package docker
  :ensure t)

(use-package exec-path-from-shell
  :ensure t)

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js[x]?$" . web-mode))
  (setq web-mode-content-types-alist
	'(("jsx" . "\\.js[x]?\\'")))
  (add-hook 'web-mode-hook
          '(lambda ()
             (setq web-mode-attr-indent-offset nil)
             (setq web-mode-markup-indent-offset 2)
             (setq web-mode-css-indent-offset 2)
             (setq web-mode-code-indent-offset 2)
             (setq web-mode-sql-indent-offset 2)
             (setq indent-tabs-mode nil)
             (setq tab-width 2)
          )))
