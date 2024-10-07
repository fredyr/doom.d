;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates snippets and.
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
      doom-variable-pitch-font (font-spec :family "DejaVu Sans Mono" :size 20))

;; THERE are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;
;;
;;(setq doom-theme 'doom-monokai-pro)
;;(setq doom-monokai-pro-padded-modeline t)
;;
(setq doom-theme 'doom-fd)
(setq doom-fd-brighter-comments t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "/home/fredyr/Documents/1928/docs")

(setq deft-directory "/home/fredyr/Documents/1928/docs"
      deft-extensions '("org" "txt")
      deft-recursive t)

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
;;   (setq evil-colemak-basics-rotate-t-f-j t)
;;   (setq evil-colemak-basics-layout-mod 'mod-dh)
;;   )
;; (after! evil (global-evil-colemak-basics-mode))

;; (setq avy-keys '(?a ?r ?s ?t ?g ?m ?n ?e ?i ?o))
;; (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)

(defun append-semicolon ()
  (interactive)
  (evil-end-of-line)
  (evil-append nil)
  (insert ";")
  (evil-normal-state))

(defun cargo-test-nocapture ()
  (interactive)
  (shell-command "cargo test -- --nocapture"))

(use-package avy :ensure t :demand t
  :init
  (avy-setup-default)
  :config
  (defun avy-goto-parens ()
    (interactive)
    (let ((avy-command this-command))   ; for look up in avy-orders-alist
      (avy-jump "(+"))))

(map! :after rust
      :map rust-mode-map
      :localleader
      ;; <localleader> x will invoke the dosomething command
      (:prefix "t"
       :desc "all --nocapture" "r" #'cargo-run-nocapture)
      )

(define-key evil-motion-state-map "H" 'doom/backward-to-bol-or-indent)
(define-key evil-motion-state-map "L" 'doom/forward-to-last-non-comment-or-eol)

;; https://github.com/danielmt/doom-emacs-config/blob/master/modules/private/personal/%2Bbindings.el
(map! "C-:"  #'append-semicolon)
(map! :after evil
      :nv "g h" #'doom/backward-to-bol-or-indent
      :nv "g l" #'doom/forward-to-last-non-comment-or-eol
      :nv "g s p" #'avy-goto-parens)

(sp-use-smartparens-bindings)

(setq evil-move-cursor-back nil)
(setq evil-move-beyond-eol t)

(require 'magit)
(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

(setq abbrev-file-name "~/.emacs.d/.abbrev_defs")
(setq abbrev-suggest t)
(setq abbrev-suggest-hint-threshold 2)

(setq company-idle-delay 0.0)

;; https://stackoverflow.com/questions/16770868/org-babel-doesnt-load-graphviz-editing-mode-for-dot-sources
;; https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-dot.html
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

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode)
  ;; If you have use-package-hook-name-suffix set to nil, uncomment and use the
  ;; line below instead:
  ;; :hook (eshell-mode-hook . esh-autosuggest-mode)
  :ensure t)
(setq evil-collection-company-use-tng nil)

(setq lsp-enable-file-watchers nil)

;; add to $DOOMDIR/config.el
(after! dap-mode
  (setq dap-python-debugger 'debugpy))

(use-package! gptel
  :config
  (setq
   gptel-default-mode #'org-mode
   gptel-model "gemini-1.5-flash"
   gptel-backend (gptel-make-gemini "Gemini"
                   :key "AIzaSyAOcFOP8vTKZUcIDYao7HDlWmn9BB5nEWY"
                   :stream t)))

(use-package macrursors
  :config
  (dolist (mode '(corfu-mode goggles-mode beacon-mode))
    (add-hook 'macrursors-pre-finish-hook mode)
    (add-hook 'macrursors-post-finish-hook mode))
  (define-prefix-command 'macrursors-mark-map)
  (global-set-key (kbd "C-c SPC") #'macrursors-select)
  (global-set-key (kbd "C->") #'macrursors-mark-next-instance-of)
  (global-set-key (kbd "C-<") #'macrursors-mark-previous-instance-of)
  (global-set-key (kbd "C-;") 'macrursors-mark-map)
  (define-key macrursors-mark-map (kbd "C-;") #'macrursors-mark-all-lines-or-instances)
  (define-key macrursors-mark-map (kbd ";") #'macrursors-mark-all-lines-or-instances)
  (define-key macrursors-mark-map (kbd "l") #'macrursors-mark-all-lists)
  (define-key macrursors-mark-map (kbd "s") #'macrursors-mark-all-symbols)
  (define-key macrursors-mark-map (kbd "e") #'macrursors-mark-all-sexps)
  (define-key macrursors-mark-map (kbd "f") #'macrursors-mark-all-defuns)
  (define-key macrursors-mark-map (kbd "n") #'macrursors-mark-all-numbers)
  (define-key macrursors-mark-map (kbd ".") #'macrursors-mark-all-sentences)
  (define-key macrursors-mark-map (kbd "r") #'macrursors-mark-all-lines)
  )

(after! org-superstar
  (setq-default org-superstar-headline-bullets-list '("►" "•" "▸" "✸"))
  (setq prettify-symbols-alist
        (map-merge 'list prettify-symbols-alist
                   `(("#+begin_src" . "▹")
                     ("#+end_src" . "◃")
                     ("#+results:" . "□"))
                   )))
;; ("◉" "⁑" "⁂" "❖" "✮" "✱" "✸")
