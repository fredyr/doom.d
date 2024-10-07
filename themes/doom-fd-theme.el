;; doom-fd-theme.el --- a more fd version of doom-one -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
(require 'doom-themes)
;;; Code:
;;
(defgroup doom-fd-theme nil
  "Options for the `doom-fd' theme."
  :group 'doom-themes)

(defcustom doom-fd-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-fd-theme
  :type 'boolean)

(defcustom doom-fd-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-fd-theme
  :type 'boolean)

(defcustom doom-fd-comment-bg doom-fd-brighter-comments
  "If non-nil, comments will have a subtle, darker background.
Enhancing their legibility."
  :group 'doom-fd-theme
  :type 'boolean)

(defcustom doom-fd-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-fd-theme
  :type '(choice integer boolean))


;;
(def-doom-theme doom-fd
                "A dark theme based off of xero's fd VIM colorscheme"

                ((bg         '("#242424"))
                 (bg-alt     '("#222222"))
                 (base0      '("#1d2127"))
                 (base1      '("#1d2127"))
                 (base2      '("#272727"))
                 (base3      '("#32353f"))
                 (base4      '("#494952"))
                 (base5      '("#62686E"))
                 (base6      '("#757B80"))
                 (base7      '("#9ca0a4"))
                 (base8      '("#faf4c6"))
                 (fg         '("#f6f3e8"))
                 (fg-alt     '("#5D656B"))

                 (grey       '("#686858"))
                 (red        '("#aa4450"))
                 ;; TODO color
                 (orange     '("#ffcf4a"))

                 (green      '("#87875f"))
                 (green-br   '("#719611"))
                 (teal       '("#578F8F" "#44b9b1" ))
                 ;; (yellow     '("#cc8800")) ;; this is the brownish
                 (yellow     '("#ffbd42")) ;; this is the brownish

                 (blue       '("#8ac6f2"))
                 (dark-blue  '("#6688aa"))
                 (magenta    '("#8787AF"))
                 (violet     '("#8181a6"))

                 (cyan       '("#73a5c9")) ;; search highlight, project/file menus, dired date/time, completion
                 (dark-cyan  '("#5e88a6")) ;; darker version of above

                 (fd-red           "#e5786d") ;; numbers
                 (fd-const         "#e67457") ;; was red, but want to change numbers from consts
                 (fd-blue          "#8ac6f2")
                 (fd-light-yellow  "#ffa")
                 (fd-yellow        "#ff5")
                 (fd-green         "#95e454")
                 (fd-comments      "#99968b")
                 (fd-highlight     "#454545")
                 ;; (fd-type          "#cdcde0")
                 (fd-type (doom-lighten fd-blue 0.5))

                 ;; face categories
                 (highlight      cyan)
                 (vertical-bar   base0)
                 (selection      base8)
                 (builtin        fd-light-yellow)
                 (comments       (if doom-fd-brighter-comments fd-comments grey))
                 (doc-comments   (if doom-fd-brighter-comments (doom-lighten fd-comments 0.0) (doom-darken grey 0.1)))
                 (constants      fd-const)
                 (functions      fd-yellow)
                 (keywords       fd-blue)
                 (methods        fd-yellow)
                 (operators      green-br)
                 (type           fd-type)
                 (strings        fd-green)
                 (variables      fd-light-yellow)
                 (numbers        fd-red)
                 (region         base4)
                 (error          red)
                 (warning        orange)
                 (success        green)
                 (vc-modified    yellow)
                 (vc-added       green)
                 (vc-deleted     red)

                 ;; custom categories
                 (hidden     `(,(car bg) "black" "black"))
                 (hidden-alt `(,(car bg-alt) "black" "black"))
                 (-modeline-pad
                  (when doom-fd-padded-modeline
                    (if (integerp doom-fd-padded-modeline) doom-fd-padded-modeline 4)))

                 (modeline-fg     "#bbc2cf")
                 (modeline-fg-alt (doom-blend blue grey (if doom-fd-brighter-modeline 0.4 0.08)))

                 (modeline-bg
                  (if doom-fd-brighter-modeline
                      modeline-bg
                    `(,(doom-darken (car bg) 0.15) ,@(cdr base1))))
                 (modeline-bg-l
                  (if doom-fd-brighter-modeline
                      `("#383f58" ,@(cdr base1))
                    `(,(car base3) ,@(cdr base0))))
                 (modeline-bg-inactive   `(,(doom-darken (car bg-alt) 0.2) ,@(cdr base0)))
                 (modeline-bg-inactive-l (doom-darken bg 0.20)))

  ;;;; Base theme face overrides
                ((cursor :background "yellow")
                 ((font-lock-comment-face &override)
                  :background (if doom-fd-comment-bg (doom-darken bg-alt 0.095)))
                 ;; (font-lock-keyword-face :foreground fd-blue :bold bold)
                 (fringe :background base2)
                 ((line-number &override) :foreground base4)
                 ((line-number-current-line &override) :foreground blue :bold bold)
                 (mode-line
                  :background base3 :foreground modeline-fg
                  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,base3)))
                 (mode-line-inactive
                  :background bg-alt :foreground modeline-fg-alt
                  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
                 (mode-line-emphasis :foreground (if doom-fd-brighter-modeline base8 highlight))
                 (mode-line-buffer-id :foreground green-br :bold bold)

   ;;;; company
                 (company-tooltip-selection     :background base3)
   ;;;; css-mode <built-in> / scss-mode
                 (css-proprietary-property :foreground orange)
                 (css-property             :foreground green)
                 (css-selector             :foreground blue)
   ;;;; doom-modeline
                 (doom-modeline-bar :background (if doom-fd-brighter-modeline modeline-bg highlight))
                 (doom-modeline-buffer-path :foreground (if doom-fd-brighter-modeline base8 blue) :bold bold)
   ;;;; elscreen
                 (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; markdown-mode
                 (markdown-header-face :inherit 'bold :foreground red)
   ;;;; org <built-in>
                 ((org-block &override) :background bg-alt)
                 ((org-block-begin-line &override) :background bg-alt)
                 ((org-block-end-line &override) :background bg-alt)
                 (org-hide :foreground hidden)
                 ;; TODO fix colors
                 (org-level-1 :foreground fd-blue :weight 'bold)
                 (org-level-2 :foreground (doom-lighten fd-blue 0.25) :weight 'bold)
                 (org-level-3 :foreground (doom-lighten fd-blue 0.50) :weight 'bold)
                 (org-level-4 :foreground (doom-lighten fd-blue 0.75) :weight 'bold)

   ;;;; rainbow-delimiters
                 (rainbow-delimiters-depth-1-face :foreground dark-cyan)
                 (rainbow-delimiters-depth-2-face :foreground teal)
                 (rainbow-delimiters-depth-3-face :foreground dark-blue)
                 (rainbow-delimiters-depth-4-face :foreground green)
                 (rainbow-delimiters-depth-5-face :foreground violet)
                 (rainbow-delimiters-depth-6-face :foreground green)
                 (rainbow-delimiters-depth-7-face :foreground orange)
   ;;;; rjsx-mode
                 (rjsx-attr :foreground magenta :slant 'italic :weight 'medium)
                 (rjsx-tag :foreground blue)
                 (rjsx-tag-bracket-face :foreground base8)
   ;;;; solaire-mode
                 (solaire-mode-line-face
                  :inherit 'mode-line
                  :background modeline-bg-l
                  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
                 (solaire-mode-line-inactive-face
                  :inherit 'mode-line-inactive
                  :background modeline-bg-inactive-l
                  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l))))

  ;;;; Base theme variable overrides
                ;; ()
                )

;;; doom-fd-theme.el ends here
