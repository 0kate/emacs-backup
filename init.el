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

;; display-line-numbers-mode
(display-line-numbers-mode t)
(add-hook 'eshell-mode-hook ())
(add-hook 'shell-mode-hook (lambda ()
			     (display-line-numbers-mode nil)
			     (define-key shell-mode-map (kbd "C-p") 'comint-previous-input)
			     (define-key shell-mode-map (kbd "C-n") 'comint-next-input)))
(add-hook 'dashboard-mode-hook ())

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

;; global keybinds
(global-set-key (kbd "<f5>") 'eval-buffer)
(global-set-key (kbd "C-h") 'delete-backward-char)

;; exec-path-from-path
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; rbenv
(setenv "RBENV_ROOT" "~/.anyenv/envs/rbenv")
(use-package rbenv
  :ensure t
  :config
  (global-rbenv-mode))

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
  :config
  (rainbow-delimiters-mode t))

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

(use-package company
  :ensure t
  :config
  (global-company-mode t)
  (setq company-idle-delay 0)
  (push 'company-lsp company-backends)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

;; lsp-mode
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-completion-provider :capf)
  (setq lsp-pyls-server-command '("${HOME}/.anyenv/envs/pyenv/shims/pyls"))
  :hook
  ((c-mode . lsp)
   (python-mode . lsp)
   (ruby-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; lsp-ui
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; lsp-ivy
(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

;; lsp-pyright
(use-package lsp-pyright
  :ensure t
  :hook
  (python-mode . (lambda ()
		   (require 'lsp-pyright)
		   (lsp))))

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

(defun test-while ()
  (interactive)
  (setq str (read-string "input >>"))
  (setq i 0)
  (setq array (list "a" "b"))
  (push "c" array)
  (message (mapconcat 'identity array ","))
  (while (< i (length str))
    (setq i (1+ i))))

(defun chunking-str (str chunk-len)
  (let (i 0) (chunks '())
       (while (< i (length str))
	 (push chunks (substring str i (+ chunk-len i)))
	 (let i (+ i chunk-len)))
  (chunks)))

(defun test-chunking-str ()
  (interactive)
  (setq str (read-string "str >> "))
  (setq chunks (chunking-str str 2))
  (message (mapconcat 'identity chunks ",")))

;; fsa-payload-generator
(defun fsa-payload-generator ()
  (interactive)
  (let target-addr (read-string "Target address >> "))
  (let venom-addr (read-string "Venom address >> "))
  (let bytes '())
  (let i 0)
  (while (< i (length target-addr))
    (when (eq (% i 2) 0)
	(progn
	  (let byte (substring target-addr i (+ 2 i)))
	  (push byte bytes)))
    (setq i (1+ i)))
  (message (mapconcat 'identity bytes "")))
