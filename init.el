(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable-melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'load-path "~/.local/share/icons-in-terminal/")
(package-initialize)

;; initialize use-packge
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-instanll 'use-package)))

(defun append-to-path (new-path)
  (setenv "PATH" (concat new-path path-separator (getenv "PATH"))))

;; anyenv shims
(defun append-anyenv-path (env)
  (let ((path (concat (getenv "HOME") "/.anyenv/envs/" env)))
    (setq bin-path (concat path "/bin"))
    (setq shims-path (concat path "/shims"))
    (append-to-path bin-path)
    (append-to-path shims-path)))
(append-anyenv-path "denv")
(append-anyenv-path "jenv")
(append-anyenv-path "nodenv")
(append-anyenv-path "pyenv")
(append-anyenv-path "rbenv")

(append-to-path (concat (getenv "HOME") "/.local/waf-2.0.22"))

(defun eshell-run (cmd)
  "RUns the command 'cmd' in eshell."
  (with-current-buffer "*eshell*"
    (end-of-buffer)
    (eshell-kill-input)
    (message (concat "Running in Eshell: " cmd))
    (insert cmd)
    (eshell-send-input)
    (eshell-bol)
    (yank)))

;; display-line-numbers-mode
(add-hook 'eshell-mode-hook
          (lambda ()
            (display-line-numbers-mode -1)
            (define-key eshell-mode-map (kbd "M-p") 'eshell-previous-input)
            (define-key eshell-mode-map (kbd "M-n") 'eshell-next-input)
            (define-key eshell-mode-map (kbd "C-l") (lambda ()
                                                      (interactive)
                                                      (eshell-run "clear 1")))))
(add-hook 'shell-mode-hook
          (lambda ()
            (display-line-numbers-mode -1)
            (define-key shell-mode-map (kbd "C-p") 'comint-previous-input)
            (define-key shell-mode-map (kbd "C-n") 'comint-next-input)
            (define-key shell-mode-map (kbd "C-l") 'comint-clear-buffer)))
(add-hook 'dashboard-mode-hook
          (lambda ()
            (display-line-numbers-mode -1)))
(add-hook 'json-mode-hook
          (lambda ()
            (setq js-indent-level 2)))
(add-hook 'prog-mode-hook
          (lambda ()
            (display-line-numbers-mode t)))
(add-hook 'text-mode-hook
          (lambda ()
            (display-line-numbers-mode t)))
(add-hook 'c-mode-hook
          (lambda ()
            (setq c-basic-offset 4)
            (setq tab-width 2)))

;; disable
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq mode-line-percent-position nil)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq initial-scratch-message "")

;; tabs
(custom-set-variables '(tab-width 4))
(setq-default indent-tabs-mode nil)

;; font size
(set-face-attribute 'default nil :height 115)

;; line spacing
(set-default 'line-spacing 3)

;; scroll
(setq scroll-conservatively 1)

;; custom variable
(setq custom-file (concat user-emacs-directory "/custom.el"))

;; global keybinds
(global-set-key (kbd "<f5>") 'eval-buffer)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-M-[") 'shrink-window-horizontally)
(global-set-key (kbd "C-]") 'enlarge-window-horizontally)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C-x p") (lambda ()
                                (interactive)
                                (other-window -1)))
(global-set-key (kbd "C-x SPC") 'rectangle-mark-mode)

;; electric-pair
(electric-pair-mode 1)

(setenv "PATH" (concat "$HOME/go/1.17.3/bin" ":"
                       "$HOME/.anyenv/envs/goenv/versions/1.17.3/bin" ":"
                       (getenv "PATH")))

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
  (global-git-gutter-mode t)
  :custom
  (git-gutter:modified-sign "~")
  :custom-face
  (git-gutter:added ((t (:inherit fringe :background "green4" :foreground "green1"))))
  (git-gutter:deleted ((t (:inherit fringe :background "brown4" :foreground "brown1"))))
  (git-gutter:modified ((t (:inherit fringe :background "turquoise4" :foreground "turquoise1")))))

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
  (setq neo-window-fixed-size nil)
  :bind
  ("C-t" . neotree-toggle)
  ("C-S-h" . neotree-hidden-file-toggle))

;; centaur-tabs
(use-package centaur-tabs
  :ensure t
  :demand
  :config
  (centaur-tabs-headline-match)
  (centaur-tabs-mode t)
  :custom
  (centaur-tabs-style "wave")
  (centaur-tabs-height 30)
  (centaur-tabs-set-icons t)
  (centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker "◉")
  :bind
  ("<C-iso-lefttab>" . centaur-tabs-backward)
  ("C-<tab>" . centaur-tabs-forward))

;; dashboard
(use-package dashboard
  :ensure t
  :diminish
  (dashboard-mode page-break-lines-mode)
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents . 5)
                          (bookmarks . 5)
                          (projects . 5))))

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
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (add-hook 'nxml-mode-hook 'highlight-indent-guides-mode))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; paren
(use-package paren
  :ensure t
  :config
  (show-paren-mode 1))

;; markdown-preview-mode
(use-package markdown-preview-mode
  :ensure t
  :config
  (setq markdown-preview-stylesheets '("github.css")))

(use-package docker
  :ensure t)

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js[x]?$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ts[x]?$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue?$" . web-mode))
  (setq indent-tabs-mode nil)
  (setq tab-width 2)
  :custom
  (web-mode-auto-close-style 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-script-padding 0))

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
  :ensure t
  :init
  (add-hook 'go-mode-hook (lambda ()
                            (add-hook 'before-save-hook #'lsp-format-buffer t t)
                            (add-hook 'before-save-hook #'lsp-organize-imports t t)
                            (setq indent-tabs-mode nil)
                            (setq c-basic-offset 4)
                            (setq tab-width 4))))

;; lsp-mode
(use-package lsp-mode
  :ensure t
  :hook
  ((sh-mode . lsp)
   (c-mode . lsp)
   (c++-mode . lsp)
   (dockerfile-mode . lsp)
   (go-mode . lsp-deferred)
   (java-mode . lsp)
   (json-mode . lsp)
   (nxml-mode . lsp)
   (python-mode . lsp)
   (scala-mode . lsp)
   (rust-mode . lsp)
   (web-mode . lsp)
   ;; key integration
   (lsp-mode . lsp-enable-which-key-integration))
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-prefer-capf t)
  (lsp-rust-server 'rust-analyzer)
  :commands (lsp lsp-deferred))

;; lsp-ui
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :after lsp-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-max-width  60)
  (lsp-ui-doc-max-height 20)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-use-webkit nil)

  ;; lsp-ui-flycheck
  (lsp-ui-flycheck-enable t)

  ;; lsp-ui-sideline
  ;; (lsp-ui-sideline-enable t)
  ;; (lsp-ui-sideline-ignore-duplicate t)
  ;; (lsp-ui-sideline-show-symbol t)
  ;; (lsp-ui-sideline-show-hover t)
  ;; (lsp-ui-sideline-show-diagnostics t)
  ;; (lsp-ui-sideline-show-code-actions nil)

  ;; lsp-ui-imenu
  (lsp-ui-imenu-enable t)
  (lsp-ui-imenu-kind-position 'top)

  ;; lsp-ui-peek
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-always-show t)
  (lsp-ui-peek-peek-height 30)
  (lsp-ui-peek-list-width 30)
  (lsp-ui-peek-fontify 'always)
  :hook
  (lsp-mode . lsp-ui-mode))

;; lsp-ivy
(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

;; lsp-pyright
(use-package lsp-pyright
  :ensure t)

;; lsp-java
(use-package lsp-java
  :ensure t)

;; ccls
(use-package ccls
  :ensure t
  :config
  (setq ccls-executable "/usr/bin/ccls"))

;; which-key
(use-package which-key
  :ensure t)

;; yasnippet
(use-package yasnippet
  :ensure t)

;; mozc
(add-to-list 'load-path "/usr/share/emacs24/site-lisp/emacs-mozc")
(use-package mozc
  :ensure t
  :config
  (setq default-input-method "japanese-mozc"))

;; mozc-popup
(use-package mozc-popup
  :ensure t)

(use-package whitespace
  :ensure t
  :config
  (global-whitespace-mode t)
  (setq whitespace-style '(face
                           trailing
                           tabs
                           empty
                           spaces
                           tab-mark))
  (setq whitespace-space-regexp "\\(\u3000+\\)"))

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

;; rust-mode
(use-package rust-mode
  :ensure t)

;; terraform-mode
(use-package terraform-mode
  :ensure t)

;; projectile
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1))

;; minimap
(use-package minimap
  :ensure t
  :config
  (minimap-mode nil)
  :custom
  (minimap-major-modes '(prog-mode
                         markdown-mode
                         nxml-mode))
  (minimap-window-location 'right)
  (minimap-update-delay 0.2)
  (minimap-minimum-width 1)
  :bind
  ("C-x m" . 'minimap-mode))

;; csv-mode
(use-package csv-mode
  :ensure t)

;; scala-mode
(use-package scala-mode
  :ensure t)

;; sbt-mode
(use-package sbt-mode
  :ensure t)

;; lsp-metals
(use-package lsp-metals
  :ensure t)

;; evil
(use-package evil
  :ensure t)
  ;; :config
  ;; (evil-mode 1))

;; icons-in-terminal
;; (require 'icons-in-terminal)
;; (insert (icons-in-terminal 'oct_flame))
