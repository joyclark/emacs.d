(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(starter-kit
                      starter-kit-bindings
                      starter-kit-js
                      starter-kit-lisp
                      ;; Clojure & friends
                      clojure-mode
                      cider
                      cider-tracing
                      rainbow-delimiters
                      midje-mode
                      clojure-cheatsheet
                      ;; Project navigation
                      projectile
                      ack-and-a-half
                      ;; Misc.
                      markdown-mode
                      twilight-theme
                      nyan-mode
                      hlinum
                      color-theme
                      color-theme-solarized
                      windmove
                      yasnippet)
  "A list of packages to ensure are installed at launch.")

;; Automaticaly install any missing packages
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Load the provided Clojure start kit configurations
(load (concat user-emacs-directory "clojure-starter-kit.el"))

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-warning-face ((t (:inherit nil :foreground "red" :background nil))))
 '(linum-highlight-face ((t (:inherit default :background "color-238" :foreground "white"))) t)
 '(show-paren-match ((((class color) (background dark)) (:inherit nil :foreground "red")))))

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))


;; refactor
(require 'clj-refactor)
(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               (cljr-add-keybindings-with-prefix "s-r")
                               ))                             


(load-library "iso-insert")

;; US-Keyboard & Umlaute
(define-key global-map (kbd "M-a") 'insert-a-umlaut) 
(define-key global-map (kbd "M-o") 'insert-o-umlaut)
(define-key global-map (kbd "M-u") 'insert-u-umlaut)
(define-key global-map (kbd "M-A") 'insert-A-umlaut)
(define-key global-map (kbd "M-O") 'insert-O-umlaut)
(define-key global-map (kbd "M-U") 'insert-U-umlaut)

(define-key global-map (kbd "M-z") 'smex) ;; Because I trigger it accidently all the time
(define-key global-map (kbd "s-3") 'smex) ;; eclipse style cmd-3 (MAC OS)
(define-key global-map (kbd "s-1") 'dirtree) ;;
(define-key global-map (kbd "s-F") 'iwb)

(define-key global-map (kbd "C-x f") 'ido-find-file) ;; Because I trigger it accidently all the time

(define-key projectile-mode-map [?\s-d] 'projectile-find-dir)
(define-key projectile-mode-map [?\s-p] 'projectile-switch-project)
(define-key projectile-mode-map [?\s-f] 'projectile-find-file)
(define-key projectile-mode-map [?\s-g] 'projectile-grep)


(set-face-attribute 'default nil :height 165)


(require 'midje-mode)
(add-hook 'clojure-mode-hook 'midje-mode)
(add-hook 'clojure-mode-hook 'typed-clojure-mode)

(nyan-mode)
 (require 'color-theme)
 (color-theme-initialize)
(load-theme 'solarized-dark t)
 
; (color-theme-robin-hood)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

  (defun esk-pretty-fn ()
    (font-lock-add-keywords nil `(("(\\(\\<fn\\>\\)"
                                   (0 (progn (compose-region (match-beginning 1)
                                                             (match-end 1)
                                                             "\u03BB"
                                                             'decompose-region)))))))

(add-to-list 'load-path "~/.emacs.d/manual-pkgs/")

(require 'windata)
(require 'tree-mode)
(require 'dirtree)


(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)


(load-theme 'wombat)

;; prevent from infinite printing
(setq cider-repl-print-length 100) 


;; Smooth scrolling
(setq scroll-step            1
      scroll-conservatively  10000)

;; Swap keybindings for send to repl and eval in buffer
;; (define-key cider-mode-map (kbd "C-c M-p") 'cider-pprint-eval-last-sexp)
;; (define-key cider-mode-map (kbd "C-c C-p") 'cider-insert-last-sexp-in-repl)
 (define-key cider-mode-map (kbd "TAB") 'complete-symbol)

;; Shut up emacs!
(setq visible-bell 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "1f3304214265481c56341bcee387ef1abb684e4efbccebca0e120be7b1a13589" default))))
