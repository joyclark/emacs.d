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
                     ; cider-tracing
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
                      company
                     ; company-cider
                      yasnippet
                      ;;icomplete

                      clj-refactor
                      key-chord
                      )
  "A list of packages to ensure are installed at launch.")

;; Automaticaly install any missing packages
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Load the provided Clojure start kit configurations
(load (concat user-emacs-directory "clojure-starter-kit.el"))

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

(add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-warning-face ((t (:inherit nil :foreground "red" :background nil))))
 '(linum-highlight-face ((t (:inherit default :background "color-238" :foreground "white"))) t))

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

(add-to-list 'auto-mode-alist '("\\.cljx\\'" . clojure-mode))

;(require 'icomplete)
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)


(load-library "iso-insert")

(require 'key-chord)
(key-chord-define-global "bb" 'switch-to-previous-buffer)
(key-chord-mode +1)

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

(setq-default mode-line-format
  (list
    ;; the buffer name; the file name as a tool tip
    '(:eval (propertize "%b " 'face 'font-lock-keyword-face
        'help-echo (buffer-file-name)))

    ;; line and column
    "(" ;; '%02' to set to 2 chars at least; prevents flickering
      (propertize "%02l" 'face 'font-lock-type-face) ","
      (propertize "%02c" 'face 'font-lock-type-face) 
    ") "

    ;; relative position, size of file
    '(:eval (list (nyan-create))) 

    ;; the current major mode for the buffer.
    "["

    '(:eval (propertize "%m" 'face 'font-lock-string-face
              'help-echo buffer-file-coding-system))
    "] "


    "[" ;; insert vs overwrite mode, input-method in a tooltip
    '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
              'face 'font-lock-preprocessor-face
              'help-echo (concat "Buffer is in "
                           (if overwrite-mode "overwrite" "insert") " mode")))

    ;; was this buffer modified since the last save?
    '(:eval (when (buffer-modified-p)
              (concat ","  (propertize "Mod"
                             'face 'font-lock-warning-face
                             'help-echo "Buffer has been modified"))))

    ;; is this buffer read-only?
    '(:eval (when buffer-read-only
              (concat ","  (propertize "RO"
                             'face 'font-lock-type-face
                             'help-echo "Buffer is read-only"))))  
    "] "

    ;; add the time, with the date and the emacs uptime in the tooltip
    '(:eval (propertize (format-time-string "%H:%M")
              'help-echo
              (concat (format-time-string "%c; ")
                      (emacs-uptime "Uptime:%hh"))))
    " --"
    ;; i don't want to see minor-modes; but if you want, uncomment this:
    ;; minor-mode-alist  ;; list of minor modes
    "%-" ;; fill with '-'
    ))






(require 'color-theme)
(color-theme-initialize)

 
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
;(load-theme 'jens)


;; prevent from infinite printing
(setq cider-repl-print-length 100) 


;; Enable Winner Mode
(when (fboundp 'winner-mode)
      (winner-mode 1))

;; Smooth scrolling
(setq scroll-step            1
      scroll-conservatively  10000)

;; Swap keybindings for send to repl and eval in buffer
;; (define-key cider-mode-map (kbd "C-c M-p") 'cider-pprint-eval-last-sexp)
;; (define-key cider-mode-map (kbd "C-c C-p") 'cider-insert-last-sexp-in-repl)
 ;(define-key cider-mode-map (kbd "TAB") 'complete-symbol)

(define-key global-map (kbd "TAB") 'company-complete)

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(global-set-key (kbd "C-c b") 'switch-to-previous-buffer)


;; Shut up emacs!
;;(setq visible-bell 1)
(setq ring-bell-function 'ignore)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default))))
