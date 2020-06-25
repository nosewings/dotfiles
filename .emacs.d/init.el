;;; init.el -- GNU Emacs initialization file.

;;; Commentary:

;; This is my init file.

;;; Code:

;;;; Functions and macros

(defmacro eval-if-fboundp (func-call)
  "Evaluate FUNC-CALL if the function is defined.
Used to silence flycheck warnings."
  (let ((sym (car func-call)))
    `(if (fboundp ',sym) ,func-call
       (error "Function `%s' not defined" (symbol-name ',sym)))))

(defun kill-other-buffers ()
  "Kill all buffers except the current one."
  (interactive)
  (let ((current (current-buffer)))
    (dolist (buffer (buffer-list))
	    (if (not (eq buffer current))
		(kill-buffer buffer)))))

;;;; straight.el

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
(eval-if-fboundp
 (straight-use-package 'use-package))
(defvar straight-use-package-by-default)
(setq straight-use-package-by-default t)

;;;; custom

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;;;; Packages

(use-package cargo)

(use-package counsel
  :init
  (counsel-mode))

(use-package cuda-mode)

(use-package dante
  :after haskell-mode
  :commands 'dante-mode
  :hook (haskell-mode . dante-mode)
  :config
  (eval-if-fboundp
   (flycheck-add-next-checker 'haskell-dante '(info . haskell-hlint))))

(use-package elpy
  :init
  (advice-add 'python-mode :before 'elpy-enable))

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

(use-package flycheck
  :init
  (global-flycheck-mode))

(use-package haskell-mode
  :init
  (add-hook 'haskell-mode-hook (lambda ()
				 (haskell-indent-mode)
				 (setq buffer-face-mode-face '(:family "Hasklig"))
				 (buffer-face-mode t))))



(use-package hasklig-mode
  :hook haskell-mode)

(use-package ivy
  :init
  (ivy-mode t))

(use-package magit)

(use-package monokai-theme)

(use-package opencl-mode)

(use-package org
  :commands org-mode
  :init
  (add-hook
   'org-mode-hook
   (lambda ()
     (visual-line-mode t))))

(use-package pkgbuild-mode)

(use-package racer
  :init
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode))

(use-package rust-mode)

(use-package swiper
  :init
  (global-set-key "\C-s" 'swiper))

(use-package tex
  :straight auctex
  :init
  (add-hook
   'TeX-mode-hook
   (lambda ()
     (visual-line-mode t)))
  :config
  (setq TeX-parse-self t
	TeX-auto-save t))

(use-package yaml-mode)

;;;; Backups

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;;;; Buffers

(setq-default message-log-max nil)
(kill-buffer "*Messages*")
(setq initial-scratch-message nil)

;;;; UI

(load-theme 'monokai)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq inhibit-splash-screen t)

(menu-bar-mode 0)
(tool-bar-mode 0)

(setq column-number-mode t)
(global-display-line-numbers-mode)

(defvar show-paren-delay)
(setq show-paren-delay 0)
(show-paren-mode 1)

(global-hl-line-mode t)

(xterm-mouse-mode t)

;; Too often have I accidentally pressed this key combination.
;; The function is save-buffers-kill-emacs.
(global-unset-key (kbd "C-x C-c"))

;;;; Misc

(ignore-errors
  (load-file (let ((coding-system-for-read 'utf-8))
	       (shell-command-to-string "agda-mode locate"))))

(setq c-default-style
      '((java-mode . "java")
	(awk-mode . "awk")
	(other . "k&r"))
      c-basic-offset 4)

;;;; Server

(server-start)

;;; init.el ends here
