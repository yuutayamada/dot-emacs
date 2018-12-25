;; Init.el

;; todo: check loadup.el, startup process for termux

;; TODO: C-h
(global-set-key (kbd "C-h") 'delete-backward-char)
;; for Japanese keyboard, ¥ -> \
(define-key key-translation-map (kbd "¥") (kbd "\\"))

;;; Electric indent mode
;; make C-j great again, but use other electric indent bindings
(setq-default electric-indent-chars nil)
(global-set-key (kbd "C-j") 'newline-and-indent)

;;; background mode
(setq-default frame-background-mode 'dark)
(defun Y-set-frame-background (frame)
  "Set FRAME background mode."
  (set-terminal-parameter frame 'background-mode frame-background-mode)
  (frame-set-background-mode frame))
(add-hook 'after-make-frame-functions 'Y-set-frame-background)

;;; backup files
(setq make-backup-files nil)

;; auto save file configuration
(defconst emacs-tmp-dir
  (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory))
(setq backup-directory-alist `((".*" . ,emacs-tmp-dir))
      auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t))
      auto-save-list-file-prefix emacs-tmp-dir)

;;; straight.el/package-manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; TODO: remove once `use-package' is merged in devel
(straight-use-package 'use-package)
(require 'use-package)

;;;; Builtin Packages ;;;;
(add-hook 'prog-mode-hook 'show-paren-mode)

;; Dired
;; looks like this is correct way to load according to dired-x's comment
(add-hook 'dired-load-hook (lambda () (require 'dired-x)))
(use-package dired
  :init (setq dired-use-ls-dired nil) ; my ls doesn't support --dired
  :bind (:map dired-mode-map
	      ("SPC" . dired-omit-mode)
	      ("-"   . dired-up-directory)
	      (";"   . magit-status)
	      ("r"   . wdired-change-to-wdired-mode))
  :config
  ;; dired-x
  (setq dired-omit-files; see also `dired-omit-regexp'
	(rx (and ; ignore #autosave# & dot files
	     line-start (and (or "." "#")
			     (0+ (or letter (syntax whitespace))))))))

(use-package ido
  :init (ido-mode t)
  :config
  (defconst ido-save-directory-list-file
    (concat (getenv "XDG_CACHE_HOME") "/emacs/ido.last"))
  (setq ido-use-virtual-buffers t
	recentf-max-saved-items 2000))

(use-package recentf
  :init
  (setq recentf-save-file
	(concat (getenv "XDG_CACHE_HOME") "/emacs/recentf")))

(use-package org
  :no-require t
  :init (add-hook 'org-mode-hook 'org-indent-mode)
  :config (setq org-use-speed-commands t))

(use-package epa
  ;; use gpg on emacs
  ;; don't forget to put "allow-emacs-pinentry" on gpp-agent.conf
  :init (setq epa-pinentry-mode 'loopback))

;;;; Package config and installation ;;;;

;; allow installation via straight with use-package
(defconst straight-use-package-by-default t)

(use-package paredit
  :hook (lisp-mode pardit-mode))

;; TODO: screen touch doesn't work except emacs-state from ipad
;; TODO: cursor shape change (but mosh doesn't support yet)
(use-package evil
  :no-require t
  :commands (evil-normal-state); autoload commands
  :hook (find-file . evil-normal-state)
  :config; use emacs key binds on insert state
  ;; empty insert state map to emulate emacs' keybinds
  (setq evil-insert-state-map (make-sparse-keymap))
  ;; C-[ as escape key
  (let ((n-state 'evil-normal-state))
    (define-key esc-map (kbd "C-[") n-state)
    (define-key evil-insert-state-map (kbd "C-[") n-state)
    (define-key evil-replace-state-map (kbd "C-[") n-state))
  ;; toggle evil state and emacs state by M-. for iPad, so I can use C-z 
  ;; to other stuff (iPad uses Cmd-. as escape)
  (define-key evil-normal-state-map (kbd "C-[") 'evil-emacs-state)
  (define-key evil-emacs-state-map (kbd "M-.") 'evil-normal-state)
  
  (add-hook 'evil-emacs-state-entry-hook 'hl-line-mode)
  (add-hook 'evil-emacs-state-exit-hook '(lambda () (hl-line-mode -1)))

  (setq evil-cross-lines t
	evil-move-beyond-eol t
	evil-move-cursor-back nil))

(use-package magit
  :bind (("M-g M-s" . magit-status)
	 ("M-g s"   . magit-status)))

(use-package with-editor
  :config
  (define-key (current-global-map)
    [remap async-shell-command] 'with-editor-async-shell-command)
  (define-key (current-global-map)
    [remap shell-command] 'with-editor-shell-command))

(use-package counsel
  :bind (("M-o" . counsel-apropos)
	 ("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)))

(use-package git-gutter
  :hook (find-file git-gutter-mode))

(use-package fish-mode
  :hook
  (before-save . fish_indent-before-save)
  :config
  (setq fish-enable-auto-indent t))
