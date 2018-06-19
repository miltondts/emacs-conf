;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;         Save files to saves                      ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 100
  kept-old-versions 100
  version-control t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;         General Emacs Apperance                  ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(column-number-mode 1)
(setq inhibit-startup-message 't)
(blink-cursor-mode 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;         Firefox Style Font Resizing              ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar rlm-default-font-size 100)
(defvar rlm-font-size
  rlm-default-font-size)

(defun change-font-size (num)
  (setq rlm-font-size (+ rlm-font-size num))
  (message (number-to-string rlm-font-size))
  (set-face-attribute 'default nil
		      :height rlm-font-size))

(defun font-increase ()
  (interactive)
  (change-font-size 2))

(defun font-decrease ()
  (interactive)
  (change-font-size -2))

(defun font-restore ()
  (interactive)
  (setq rlm-font-size rlm-default-font-size)
  (change-font-size 0))

;; Same bindings as Firefox
(global-set-key (kbd "C-+") 'font-increase)
(global-set-key (kbd "C--") 'font-decrease)
(global-set-key (kbd "C-=") 'font-restore)

(change-font-size 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;         Firefox Style Fullscreen                 ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter
     nil 'fullscreen
     (if (equal 'fullboth current-value)
	 (if (boundp 'old-fullscreen) old-fullscreen nil)
       (progn (setq old-fullscreen current-value)
	      'fullboth)))))
;; again, same bindings as firefox
(global-set-key [f11] 'toggle-fullscreen)

;; start in fullscreen mode
(toggle-fullscreen)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;         Miscellaneous Settings                   ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq auto-mode-alist (cons '("README" . text-mode) auto-mode-alist))
;; activate auto-fill-mode for various other modes
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'scheme-mode-hook 'turn-on-auto-fill)
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

(require 'package)

(package-initialize)

(ido-mode 1)
(smex-initialize)

(setq ergoemacs-theme nil)
(setq ergoemacs-keyboard-layout "gb")
(ergoemacs-mode 1)

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(show-paren-mode 1)

(load-theme 'spacemacs-dark t)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(global-auto-revert-mode)

(defun my-dired-mode-hook ()
  ;; To hide dot-files by default
  (dired-hide-dotfiles-mode)

  ;; To toggle hiding
  (define-key dired-mode-map "." #'dired-hide-dotfiles-mode)
  (define-key dired-mode-map "i" 'dired-subtree-insert)
  (define-key dired-mode-map ";" 'dired-subtree-remove)
  (define-key dired-mode-map [return] 'dired-single-buffer)
  (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
  (define-key dired-mode-map "^"
    (function
     (lambda nil (interactive) (dired-single-buffer "..")))))

(add-hook 'dired-mode-hook #'my-dired-mode-hook)

(setq org-agenda-files '("~/gtd/inbox.org"
                         "~/gtd/gtd.org"
                         "~/gtd/tickler.org"))

(setq org-default-notes-file "~/gtd/inbox.org")
(define-key global-map "\C-cc" 'org-capture)

(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/gtd/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline "~/gtd/tickler.org" "Tickler")
                               "* %i%? \n %U")))

(setq org-refile-targets '(("~/gtd/gtd.org" :maxlevel . 3)
                           ("~/gtd/someday.org" :level . 1)
                           ("~/gtd/tickler.org" :maxlevel . 2)))

(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

(setq org-journal-dir "~/journal")

(require 'org-journal)

(setq tramp-default-method "ssh")

(load (expand-file-name "~/quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "sbcl")
