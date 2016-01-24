;; load-path で ~/.emacs.d とか書かなくてよくなる
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/misc/jdee-2.4.0.1/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/misc/cedet-1.1/common"))
(load-file (expand-file-name "~/.emacs.d/misc/cedet-1.1/common/cedet.el"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/misc/elib-1.0"))

;;;;;; el-get settings
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
;;;;;; end of el-get settings

;;jde
(require 'jde)
;; ecb does not support emacs24 yet???
;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/misc/ecb-2.40"))
;;(load-file "~/.emacs.d/misc/ecb-2.40/ecb.el")
;;(require 'ecb)
;; android development in JDEE
(custom-set-variables
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
; '(anything-command-map-prefix-key "")
 '(custom-enabled-themes (quote (deeper-blue)))
 '(haskell-mode-hook (quote (turn-on-haskell-indentation)) t)
 '(jde-global-classpath (quote ("/System/Library/Frameworks/JavaVM.framework/Classes/classes.jar" "~/andriod-sdks/platforms/android-8/android.jar" "~/android-sdks/platforms/android-16/android.jar"))))

;; initial loading
(setq load-path (cons "~/.emacs.d" load-path))
(add-to-list 'load-path "~/.emacs.d/misc")

(load
 (expand-file-name "~/.emacs.d/elpa/package.el"))
(when (require 'package nil t)
;  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
  (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
  (package-initialize))

;; Disable toolbar
(tool-bar-mode 0)

;; SVN mode
(el-get-bundle psvn)
(autoload 'svn-status "psvn" nil t)
(setq svn-status-hide-unmodified t)
(setq process-coding-system-alist
      (cons '("svn" . utf-8) process-coding-system-alist))

;; Git mode
;; Magit
(add-to-list 'load-path "~/.emacs.d/from_git/magit")
(require 'magit)
(require 'magit-svn)

;; YAML mode
(el-get-bundle yaml-mode)
;;; yaml-mode の設定
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
	  '(lambda ()
	     (setq comment-start "#")
	     (setq comment-start-skip "\\(^\\s-*\\|\\=\\s-*\\)#+ *")
	     (setq comment-end-skip "$")
	     (set (make-local-variable 'comment-style) 'indent) ))

;; rinari
;;; required inf-ruby
(el-get-bundle rinari)

;; rspec-mode
(add-to-list 'load-path "~/.emacs.d/from_git/mode-compile") ;; dependency
(add-to-list 'load-path "~/.emacs.d/from_git/rspec-mode")
(require 'rspec-mode)

;; configuration of rhtml-mode
(add-to-list 'load-path "~/.emacs.d/from_git/rhtml")
(require 'rhtml-mode)
(add-hook 'rhtml-mode-hook
          (lambda () (rinari-launch)))

;; configuration of yasnippet/rails-snippets
(add-to-list 'load-path
	     "~/.emacs.d/from_git/yasnippet")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/.emacs.d/from_git/yasnippet/snippets")
(yas/load-directory "~/.emacs.d/from_git/yasnippets-rails/rails-snippets")

(autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
(autoload 'inf-ruby-setup-keybindings "inf-ruby" "" t)
(eval-after-load 'ruby-mode
  '(add-hook 'ruby-mode-hook 'inf-ruby-setup-keybindings))

;; display line number
(global-linum-mode t)

;; dabbrev
;;; http://www.cua.dk/ido.html
(global-set-key "\C-o" 'dabbrev-expand)

(setq inhibit-startup-message t)
(setq-default transient-mark-mode t)

;; Skip the line which is remaining of the previous line for C-n(next-line)
(setq column-number-mode t)
(setq line-move-visual nil)

;; font-lock
(require 'font-lock)
(setq-default font-lock-maximum-decoration t)

;; do not backup file
(setq make-backup-files nil)

;; do not autosave
(setq auto-save-default nil)

(add-hook 'ruby-mode-hook
          '(lambda ()
             (setq tab-width 2)
             (setq indent-tabs-mode nil)
             (setq ruby-indent-level tab-width)))

;; make scratch buffer empty
(setq initial-scratch-message nil)

;; specify font setting
(if (display-graphic-p)
    (progn
     (create-fontset-from-ascii-font "Menlo-10:weight=normal:slant=normal"
				     nil
				     "menlokakugo")
     (set-fontset-font "fontset-menlokakugo"
		       'unicode
		       (font-spec :family "Hiragino Kaku Gothic ProN" :size 12)
		       nil
		       'append)
     (add-to-list 'default-frame-alist '(font . "fontset-menlokakugo"))))

;; yasnippet
(setq yas/trigger-key 'TAB)

;; ruby-mode
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rjs$" . ruby-mode))
(defun ruby-mode-hook-ruby-elecrtric ()
  (ruby-electric-mode t))
(add-hook 'ruby-mode-hook 'ruby-mode-hook-ruby-elecrtric)

;;rspec-mode
;; (install-elisp "http://perso.tls.cena.fr/boubaker/distrib/mode-compile.el")
(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)

;; rvm-mode
(defadvice ido-completing-read (around invaild-ido-completing-read activate)
  "ido-completing-read -> completing-read"
  (complete-read))
(rvm-use-default)

;; rubydb -- ruby debugger
(autoload 'rubydb "rubydb3x" "Run rubydb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger.")

;; ruby-block
;; highlight the line corresponding to "end"
;; downloaded from http://www.emacswiki.org/emacs/download/ruby-block.el
(el-get-bundle ruby-block)
(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)

;; auto-complete
(el-get-bundle auto-complete)

;; rsense
;;(setq rsense-home (expand-file-name "~/.emacs.d/from_git/rsense/"))
;;;;(setq rsense-rurema-home "~/opt/rurema")
;;(add-to-list 'load-path (concat rsense-home "/etc"))
;;(require 'rsense)
;;(define-key ruby-mode-map "\C-c\C-i" 'ac-complete-rsense)
;;(define-key ruby-mode-map "\C-ct" 'rsense-type-help)
;;(define-key ruby-mode-map "\C-cj" 'rsense-jump-to-definition)

;; C-c .で補完
(add-hook 'ruby-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c .") 'ac-complete-rsense)))
(add-hook 'ruby-mode-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-rsense-method)
            (add-to-list 'ac-sources 'ac-source-rsense-constant)))

;; Coffee-mode
(add-to-list 'load-path "~/.emacs.d/from_git/coffee-mode")
(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(defun coffee-custom ()
  "coffee-mode-hook"
  (set (make-local-variable 'tab-width) 2))
(add-hook 'coffee-mode-hook '(lambda() (coffee-custom)))

;; SASS
;; (add-to-list 'load-path "~/.emacs.d/from_git/scss-mode")
(require 'sass-mode)

;; Cucumber-mode
(add-to-list 'load-path "~/.emacs.d/from_git/cucumber.el")
(require 'feature-mode)


;; helm
;;(require 'helm-config)
;;(helm-descbinds-mode)
;;(require 'helm-migemo)
;;(setq helm-use-migemo t)


;; show trailing whitespace
(when (boundp 'show-trailing-whitespace)
  (setq-default show-trailing-whitespace t))

;; make _ not to be a separator of a word
(modify-syntax-entry ?_ "w")

;; haskell mode
(load "~/.emacs.d/from_git/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'font-lock-mode)
(setq haskell-program-name "ghci")


;; js mode(js.el)
(add-hook 'js-mode-hook
          '(lambda()
             (setq js-indent-level 2)
             (setq indent-tabs-mode nil)))

(add-hook 'emacs-lisp-mode-hook
          '(lambda()
             (setq indent-tabs-mode nil)))

;; slime
(add-to-list 'load-path (expand-file-name "~/.emacs.d/misc/slime-2013-03-13"))
(when (require 'slime nil t)
  (setq inferior-lisp-program "clisp -K full")
  (setq slime-protocol-version 'ignore)

  (defun mit-scheme-start-swank (file encoding)
    (format "%S\n\n" `(start-swank ,file)))

  (defun mit-scheme-find-buffer-package ()
    (save-excursion
      (let ((case-fold-search t))
        (goto-char (point-min))
        (and (re-search-forward "^;+ package: \\(([^)]+)\\)" nil t)
             (match-string-no-properties 1)))))

  (defun mit-scheme-slime-mode-init ()
    (slime-mode t)
    (make-local-variable 'slime-find-buffer-package-function)
    (setq slime-find-buffer-package-function 'mit-scheme-find-buffer-package))

  (slime-setup)
  (if (not (memq 'mit-scheme slime-lisp-implementations))
      (setq slime-lisp-implementations
            '((clisp ("clisp") :init slime-init-command)
              (mit-scheme ("mit-scheme") :init mit-scheme-start-swank))))
  (setq slime-default-lisp 'mit-scheme)
  ;;(setq slime-default-lisp 'clisp)
  (add-hook 'scheme-mode-hook 'mit-scheme-slime-mode-init))
  (add-hook 'scheme-mode-hook 
    (lambda()
      (setq indent-tabs-mode nil)))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(add-hook 'haml-mode-hook
  (lambda()
    (setq indent-tabs-mode nil)))


(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x$"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))
(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-html-offset   2)
  (setq web-mode-css-offset    2)
  (setq web-mode-script-offset 2)
  (setq web-mode-php-offset    2)
  (setq web-mode-java-offset   2)
  (setq web-mode-asp-offset    2)
  (setq indent-tabs-mode nil))
(add-hook 'web-mode-hook 'web-mode-hook)

;; helm
;;; helm
(require 'helm-config)
(helm-descbinds-mode)
;(helm-mode 1)

(let ((key-and-func
       `((,(kbd "C-r")   helm-for-files)
         (,(kbd "C-^")   helm-c-apropos)
         (,(kbd "C-;")   helm-resume)
         (,(kbd "M-s")   helm-occur)
         (,(kbd "M-x")   helm-M-x)
         (,(kbd "M-y")   helm-show-kill-ring)
         (,(kbd "M-z")   helm-do-grep)
         (,(kbd "C-S-h") helm-descbinds)
        )))
  (loop for (key func) in key-and-func
        do (global-set-key key func)))


;; slim-mode
(require 'slim-mode)
