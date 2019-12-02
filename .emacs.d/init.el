;;; init.el -- GNU Emacs initialization file.

;;; Commentary:

;; This is my init file.

;;; Code:

;;;; custom

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;;;; package

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;;;; use-package

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-defer t)
(setq use-package-always-ensure t)

;;;; Packages

(use-package cargo)

(use-package counsel
  :init
  (counsel-mode))

(use-package dante
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'dante-mode)
  (add-hook 'dante-mode-hook
   '(lambda () (flycheck-add-next-checker 'haskell-dante
                '(warning . haskell-hlint)))))

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
  (add-hook 'haskell-mode 'haskell-indent-mode))

(use-package ivy
  :init
  (ivy-mode t))

(use-package magit)

(use-package monokai-theme)

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
  :ensure auctex
  :init
  (add-hook
   'TeX-mode-hook
   (lambda ()
     (visual-line-mode t)))
  :config
  (setq TeX-parse-self t
	TeX-auto-save t))

;;;; Backups

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;;;; Buffers

(setq-default message-log-max nil)
(kill-buffer "*Messages*")
(setq initial-scratch-message nil)

;;;; UI

(setq inhibit-splash-screen t)

(menu-bar-mode 0)
(tool-bar-mode 0)

(setq column-number-mode t)
(global-display-line-numbers-mode)

(setq show-paren-delay 0)
(show-paren-mode 1)

(global-hl-line-mode t)

(load-theme 'monokai)

;;;; Functions

(defun kill-other-buffers ()
  "Kill all buffers except the current one."
  (interactive)
  (let ((current (current-buffer)))
    (mapcar (lambda (buffer)
	      (if (not (eq buffer current))
		  (kill-buffer buffer)))
	    (buffer-list))))

;;;; Server

(server-start)

;;; init.el ends here
