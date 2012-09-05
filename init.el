;;jde
(add-to-list 'load-path (expand-file-name "~/.emacs.d/misc/jdee-2.4.0.1/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/misc/cedet-1.1/common"))
(load-file (expand-file-name "~/.emacs.d/misc/cedet-1.1/common/cedet.el"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/misc/elib-1.0"))
(require 'jde)
;; ecb does not support emacs24 yet???
;(add-to-list 'load-path (expand-file-name "~/.emacs.d/misc/ecb-2.40"))
;(load-file "~/.emacs.d/misc/ecb-2.40/ecb.el")
;(require 'ecb)
;; android development in JDEE
(custom-set-variables
 '(jde-global-classpath (quote (
                                "/System/Library/Frameworks/JavaVM.framework/Classes/classes.jar"
                                "~/andriod-sdks/platforms/android-8/android.jar"
                                "~/android-sdks/platforms/android-16/android.jar"
                                ))))

;; initial loading
(setq load-path (cons "~/.emacs.d" load-path))
(add-to-list 'load-path "~/.emacs.d/auto-install")
(add-to-list 'load-path "~/.emacs.d/misc")
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/auto-install/")
;;(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)
;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

;; Disable toolbar
(tool-bar-mode 0)

;; SVN mode
(add-to-list 'load-path "~/.emacs.d/from_git/psvn")
(require 'psvn)
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
(add-to-list 'load-path "~/elisp/yaml-mode")
;;; yaml-mode の設定
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (setq comment-start "#")
             (setq comment-start-skip "\\(^\\s-*\\|\\=\\s-*\\)#+ *")
             (setq comment-end-skip "$")
             (set (make-local-variable 'comment-style) 'indent) ))

;; rinari
;;; required inf-ruby
(require 'rinari)

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

(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook '(lambda () (inf-ruby-keys)))

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
             (setq ruby-indent-level tab-width)
             ))

;; make scratch buffer empty
(setq initial-scratch-message nil)

;; specify font setting
(create-fontset-from-ascii-font "Menlo-10:weight=normal:slant=normal" nil "menlokakugo")
(set-fontset-font "fontset-menlokakugo"
                  'unicode
                  (font-spec :family "Hiragino Kaku Gothic ProN" :size 12)
                  nil
                  'append)
(add-to-list 'default-frame-alist '(font . "fontset-menlokakugo"))

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
(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)

;; auto-complete
(add-to-list 'load-path "~/.emacs.d/from_git/auto-complete")
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
(require 'auto-complete-config)
(ac-config-default)

;;rsense
(setq rsense-home (expand-file-name "~/.emacs.d/from_git/rsense/"))
;;(setq rsense-rurema-home "~/opt/rurema")
(add-to-list 'load-path (concat rsense-home "/etc"))
(require 'rsense)
(define-key ruby-mode-map "\C-c\C-i" 'ac-complete-rsense)
(define-key ruby-mode-map "\C-ct" 'rsense-type-help)
(define-key ruby-mode-map "\C-cj" 'rsense-jump-to-definition)

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

;;SASS
;;(add-to-list 'load-path "~/.emacs.d/from_git/scss-mode")
(require 'sass-mode)

;; Cucumber-mode
(add-to-list 'load-path "~/.emacs.d/from_git/cucumber.el")
(require 'feature-mode)

;; anything
(add-to-list 'load-path "~/.emacs.d/from_git/anything-config")
(add-to-list 'load-path "~/.emacs.d/from_git/anything-config/contrib")
(require 'anything-startup)
;(global-set-key "\^x\^f" 'anything-filelist+)

;; anything.el
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(anything-command-map-prefix-key "\C-c\C-f"))

; show trailing whitespace
(when (boundp 'show-trailing-whitespace)
  (setq-default show-trailing-whitespace t))

; make _ not to be a separator of a word
(modify-syntax-entry ?_ "w")

; haskell mode
(load "~/.emacs.d/from_git/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'font-lock-mode)
(custom-set-variables
 '(haskell-mode-hook '(turn-on-haskell-indentation)))


; js mode(js.el)
(add-hook 'js-mode-hook
	  '(lambda()
             (setq js-indent-level 2)
             (setq indent-tabs-mode nil)
	     ))

