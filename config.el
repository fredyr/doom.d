;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Fredrik Dyrkell"
      user-mail-address "fredrik.dyrkell@1928diagnostics.com")

(cond (IS-MAC
       (setq mac-command-modifier      'control
             mac-option-modifier       'meta
             mac-right-option-modifier 'alt)))

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "DejaVu Sans Mono" :size 20 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "DejaVu Sans Mono" :size 21))

;; THERE are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-fd)
(setq doom-fd-brighter-comments t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/projects/1928/docs")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; (add-load-path! "~/.doom.d/lisp/emacs-evil-colemak-basics")
;; (use-package! evil-colemak-basics
;;   :after evil
;;   :config
;;   ;(setq evil-colemak-basics-rotate-t-f-j t)
;;   )
;; (after! evil (global-evil-colemak-basics-mode))
;; (setq evil-colemak-basics-layout-mod 'mod-dh)

(defun append-semicolon ()
  (interactive)
  (evil-end-of-line)
  (evil-append nil)
  (insert ";")
  (evil-normal-state))

(define-key evil-motion-state-map "H" 'evil-first-non-blank)
(define-key evil-motion-state-map "L" 'evil-end-of-line)

(global-set-key (kbd "C-<left>") 'sp-forward-barf-sexp)
(global-set-key (kbd "C-<right>") 'sp-forward-slurp-sexp)
(global-set-key (kbd "C-;") 'append-semicolon)

(setq evil-move-cursor-back nil)
(setq evil-move-beyond-eol t)

(add-hook! smartparens-mode
  (smartparens-strict-mode)
  (electric-pair-mode))

;; https://github.com/hlissner/doom-emacs/issues/478
(after! smartparens
  (dolist (brace '("(" "{" "["))
    (sp-pair brace nil :unless '(:rem sp-point-before-word-p sp-point-before-same-p))
    ;; (sp-with-modes 'org-mode
    ;;   (sp-local-pair "<" nil :actions :rem))
    ))


;; python black settings
;; taken from https://gist.github.com/jordangarrison/8720cf98126a1a64890b2f18c1bc69f5
;; (use-package! python-black
;;   :demand t
;;   :after python)
;; (add-hook! 'python-mode-hook #'python-black-on-save-mode)
;; (setq blacken-line-length 100)
;; (setq blacken-skip-string-normalization t)

(after! python
  (setq python-black-extra-args '("--skip-string-normalization" "--line-length" "100"))
  (setq python-shell-interpreter "python3")
  (setq lsp-pylsp-plugins-pydocstyle-enabled nil)
  (setq lsp-pylsp-plugins-pyflakes-enabled nil)
  (setq lsp-pylsp-plugins-flake8-enabled t)
  (add-hook! 'python-mode-hook #'python-black-on-save-mode)
  )

;; (add-hook 'c-common-mode-hook
;;   (lambda ()
;;     (add-hook (make-local-variable 'before-save-hook)
;;               'clang-format-buffer)))
;; (map! :leader :desc "Comment line" "c l " #'comment-line)
;; (add-hook! 'before-save-hook #'+format/buffer)
;; (remove-hook! 'before-save-hook #'+format/buffer)
;;
(setq abbrev-file-name "~/.emacs.d/.abbrev_defs")

(setq company-idle-delay 1.0)
;; (setq doom-localleader-key "\\")
;; (setq doom-localleader-alt-key "M-,")
;;
;;
;; https://stackoverflow.com/questions/16770868/org-babel-doesnt-load-graphviz-editing-mode-for-dot-sources
;; https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-dot.html
;;
(require 'ob-dot)
(add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))
(org-babel-do-load-languages 'org-babel-load-languages '((dot . t)))

;; Zig ZLS setup
(use-package! zig-mode
  :hook ((zig-mode . lsp-deferred))
  :custom (zig-format-on-save nil)
  :config
  (after! lsp-mode
    (add-to-list 'lsp-language-id-configuration '(zig-mode . "zig"))
    (lsp-register-client
      (make-lsp-client
        :new-connection (lsp-stdio-connection "zls")
        :major-modes '(zig-mode)
        :server-id 'zls))))
