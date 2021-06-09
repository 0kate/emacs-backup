(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable-melpa.org/packages/") t)
(package-initialize)

;; initialize use-packge
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-instanll 'use-package)))

;; anyenv shims
(defun append-anyenv-path (env)
  (let ((path (concat (getenv "HOME") "/.anyenv/envs/" env)))
    (setq bin-path (concat path "/bin"))
    (setq shims-path (concat path "/shims"))
    (setenv "PATH" (concat bin-path path-separator (getenv "PATH")))
    (setenv "PATH" (concat shims-path path-separator (getenv "PATH")))))
(append-anyenv-path "denv")
(append-anyenv-path "jenv")
(append-anyenv-path "nodenv")
(append-anyenv-path "pyenv")
(append-anyenv-path "rbenv")

;; display-line-numbers-mode
(add-hook 'eshell-mode-hook (lambda ()
			     (display-line-numbers-mode -1)
			     (define-key eshell-mode-map (kbd "C-p") 'eshell-previous-input)
			     (define-key eshell-mode-map (kbd "C-n") 'eshell-next-input)))
(add-hook 'shell-mode-hook (lambda ()
			     (display-line-numbers-mode -1)
			     (define-key shell-mode-map (kbd "C-p") 'comint-previous-input)
			     (define-key shell-mode-map (kbd "C-n") 'comint-next-input)
			     (define-key shell-mode-map (kbd "C-l") 'comint-clear-buffer)))
(add-hook 'dashboard-mode-hook (lambda ()
				 (display-line-numbers-mode -1)))
(add-hook 'prog-mode-hook (lambda ()
			    (display-line-numbers-mode t)))

;; disable
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq indent-tab-mode nil)

;; line spacing
(set-default 'line-spacing 3)

;; scroll
(setq scroll-conservatively 1)

;; custom variable
(setq custom-file (concat user-emacs-directory "/custom.el"))

;; tab width
(add-hook 'json-mode-hook (lambda ()
			    (setq js-indent-level 2)))

;; global keybinds
(global-set-key (kbd "<f5>") 'eval-buffer)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-{") 'shrink-window-horizontally)
(global-set-key (kbd "C-}") 'enlarge-window-horizontally)

;; exec-path-from-path
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

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
  ("C-0" . avy-goto-word-0))

;; magit
(use-package magit
  :ensure t
  :bind
  ("C-9" . magit-status))

;; git-gutter
(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode t))

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
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

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

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js[x]?$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ts[x]?$" . web-mode))
  :hook
  (web-mode . (lambda ()
		(setq web-mode-attr-indent-offset nil)
		(setq web-mode-markup-indent-offset 2)
		(setq web-mode-css-indent-offset 2)
		(setq web-mode-code-indent-offset 2)
		(setq web-mode-sql-indent-offset 2)
		(setq indent-tabs-mode nil)
		(setq tab-width 2))))

(use-package tide
  :ensure t
  :hook
  (web-mode . (lambda ()
		(interactive)
		(tide-setup)
		(flycheck-mode +1)
		(setq flycheck-check-syntax-automatically '(save mode-enabled))
		(eldoc-mode +1)
		(tide-hl-identifier-mode +1)
		;; company is an optional dependency. You have to
		;; install it separately via package-install
		;; `M-x package-install [ret] company`
		(company-mode +1))))

(use-package company
  :ensure t
  :config
  (global-company-mode t)
  (setq company-idle-delay 0)
  (push 'company-lsp company-backends)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

;; go-mode
(use-package go-mode
  :ensure t)

;; lsp-mode
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-completion-provider :capf)
  :hook
  ((c-mode . lsp)
   (python-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; lsp-ui
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :hook
  (lsp-ui-mode . (lsp-ui-mode t)))

;; lsp-ivy
(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

;; lsp-pyright
(use-package lsp-pyright
  :ensure t
    :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

;; mozc
(add-to-list 'load-path "/usr/share/emacs24/site-lisp/emacs-mozc")
(use-package mozc
  :ensure t
  :config
  (setq default-input-method "japanese-mozc"))

;; mozc-popup
(use-package mozc-popup
  :ensure t)

;; mew
(use-package mew
  :ensure t
  :config
  (setq mew-mail-domain "me.com")
  (setq mew-proto "%")
  (setq mew-imap-server "imap.mail.me.com")
  (setq mew-imap-ssl-port "993")
  (setq mew-imap-user "o.keito317@icloud.com")
  (setq mew-imap-auth t)
  (setq mew-imap-ssl t))

(use-package whitespace
  :ensure t)

(use-package clojure-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

;; beacon
(use-package beacon
  :ensure t
  :custom
  (beacon-color "yellow")
  :config
  (beacon-mode 1))
