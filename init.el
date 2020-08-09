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

;; I don't need protection for multiuser editing
(setq create-lockfiles nil) ; for .#FILEs

;; files.el
(use-package files
  :custom
  ;; for #FILE#s
  (auto-save-file-name-transforms
   `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
      ,(concat temporary-file-directory "backups/\\2")
      t)))
  ;; for ~FILEs
  (backup-directory-alist
   `(("." . ,(concat temporary-file-directory "backups")))))

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
  :bind (;; `org-capture' has unique behavier with C-u prefix:
	 ;;   C-u: Visit the target location of a capture template.
	 ;;        You get to select the template in the usual way.
	 ;;   C-u C-u: Visit the last stored capture item in its buffer.
	 ;;   C-u 0: to insert capture in an org-mode buffer
	 ("C-c c" . org-capture)
	 ;; speed key uses v for `org-agenda', so use same key
	 ("C-c v" . org-agenda))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)))
  :custom
  ;; access org keybind without C-c prefix at begging of header (e.g, *<-)
  (org-use-speed-commands t)
  ;; file/directory related
  (org-default-notes-file (concat (getenv "HOME") "/life/notes.org"))
  (org-directory (concat (getenv "HOME") "/life/org"))
  ;; agenda files should be less volume than other ever growing org files
  ;; because the files are searched for schedules/deadlines by `org-agenda'.
  (org-agenda-files (list org-default-notes-file
			  (concat org-directory "/GTD")))
  ;; For archive command (C-c C-x C-a)
  ;; datetree/ string is special, signifying to archive items to datetree.
  ;; %s is replaced original archived file name
  (org-archive-location (concat org-directory "/archive/%s::datetree/"))

  ;;; Capture; adding org header for new idea you captured
  (org-capture-templates
   ;; The list of template element can be:
   ;;   keys, desc, entry/item/checkitem/table-line/plain,
   ;;   target, template, and properties. See also org-capture.el
   ;;   or type C in org-capture buffer.
   '(;; default entry from `org-capture-select-template'
     ("t" "Task" entry (file+headline "" "Tasks")
      "* TODO %?\n  %u\n  %a")
     ("i" "Idea" entry (file+headline "" "Ideas")
      "* %?\n  %u\n  %a")
     ;; test operation for clocked item
     ("c" "Jump to clocked item" plain (clock)
      "" :jump-to-captured t :immediate-finish t)))

  ;; GTD stuff
  ;; I don't use (/!), (@), (!) because my work is not picky
  ;; to recording the work.
  (org-todo-keywords '((sequence "TODO" "|" "DONE" "CANCELED")))

  ) ; end of org package

(use-package epa
  ;; use gpg on emacs
  ;; don't forget to put "allow-emacs-pinentry" on gpp-agent.conf
  :init (setq epa-pinentry-mode 'loopback))

(use-package hl-line
  :config
  (add-hook 'evil-emacs-state-entry-hook 'hl-line-mode)
  (add-hook 'evil-emacs-state-exit-hook '(lambda () (hl-line-mode -1))))

;;;; Package config and installation ;;;;

;; allow installation via straight with use-package
(defconst straight-use-package-by-default t)

(use-package paredit
  :hook ((lisp-mode . enable-paredit-mode)
	 (emacs-lisp-mode . enable-paredit-mode)))

;; TODO: screen touch doesn't work except emacs-state from ipad
;; TODO: cursor shape change (but mosh doesn't support yet)
(use-package evil
  :no-require t; :config is loaded when evil is loaded
  :init (setq evil-insert-state-bindings
	      '(("\C-[" . evil-normal-state)
		("\C-r" . evil-paste-from-register)
		("\C-h" . evil-delete-backward-char-and-join)
		("\C-o" . evil-execute-in-normal-state)))
  :commands (evil-normal-state evil-emacs-state); autoload commands
  :config
  ;; Turn on evil mode only for actual files except mode files
  ;; of evil-emacs-state-modes.
  (setq evil-default-state 'emacs)
  (add-to-list 'evil-emacs-state-modes 'org-mode)
  (add-hook 'find-file-hook
	    '(lambda ()
	       (let ((eesm (bound-and-true-p evil-emacs-state-modes)))
		 (if (member major-mode eesm)
		     (evil-emacs-state)
		   (evil-normal-state)))))
  ;; Avoid evil on editing commit message on magit 
  (add-to-list 'evil-buffer-regexps '("COMMIT_EDITMSG"))
  
  ;; Display powerline-ish mode status. (note: nerd fonts required)
  (advice-add 'evil-generate-mode-line-tag :around #'Y-evil-mode-line)
  (defun Y-evil-mode-line (_orig &rest _args)
    (let* (;; <C-x 8 RET> to insert unicode character directly,
	   ;; but I use hex code just because it doesn't crush window
	   ;; width in here.
	   ;; TODO: use #xE0C2, #xe0C0 instead
	   (opener (format "%c█" #xe0b2)) 
	   (closer (format "█%c" #xe0b0))
	   (data (cl-case (bound-and-true-p evil-state)
		   (normal   '("N" . (:foreground "brightgreen")))
		   (visual   '("V" . (:foreground "blue")))
		   (replace  '("R" . (:foreground "brightred")))
		   (operator '("O" . (:foreground "yellow")))
		   (insert   '("I" . (:foreground "cyan")))
		   (t        '("Ɛ" . (:foreground "brightmagenta")))))
	   (str (car data)))
      ;; Inverse video is just to specify by :foreground for all
      (add-face-text-property
       0 (length str)
       (append '(:inherit mode-line :inverse-video t) (cdr data))
       nil str)
      ;; open close
      (add-face-text-property 0 (length opener) (cdr data) nil opener)
      (add-face-text-property 0 (length closer)	(cdr data) nil closer)
      (format "%s%s%s" opener str closer)))
  
  ;; use same keybinding as insert mode in replace mode
  (set-keymap-parent evil-replace-state-map evil-insert-state-map))

;; Shift to evil mode by C-[ C-[ (ESC ESC)
;; TODO: assign different key to evil-toggle-key
(define-key esc-map (kbd "C-[") 'evil-normal-state)

(use-package magit
  :bind (("C-h h" . magit-status)))

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
