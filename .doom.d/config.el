;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Daniel Gonzalez"
      user-mail-address "d.gonzalez0902@gmail.com")

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
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)
(setq doom-font (font-spec :family "monospace" :size 12))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
(setq display-line-numbers-type 'relative)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.


;; Key Mappings

;; TODO: figure out how to bind deft to f8 - for now SPC n d is fine
;;(map! )

;; org keybinding
;; FIXME: this causes a bug and break everything
;; (after! org (map! :localleader
;;       :map org-mode-map
;;       (:prefix "o"
;;         :desc "Tags" "t" 'org-set-tags
;;         (:prefix ("p" . "Properties")
;;           :desc "Set" "s" 'org-set-property
;;           :desc "Delete" "d" 'org-delete-property
;;           :desc "Actions" "a" 'org-property-action
;;           )
;;         )
;;       (:prefix ("i" . "Insert")
;;         :desc "Link/Image" "l" 'org-insert-link
;;         :desc "Item" "o" 'org-toggle-item
;;         :desc "Footnote" "f" 'org-footnote-action
;;         :desc "Table" "t" 'org-table-create-or-convert-from-region
;;         :desc "Screenshot" "s" 'org-download-screenshot
;;         (:prefix ("h" . "Headings")
;;           :desc "Normal" "h" 'org-insert-heading
;;           :desc "Todo" "t" 'org-insert-todo-heading
;;           (:prefix ("s" . "Subheadings")
;;             :desc "Normal" "s" 'org-insert-subheading
;;             :desc "Todo" "t" 'org-insert-todo-subheading
;;             )
;;           )
;;         (:prefix ("e" . "Exports")
;;           :desc "Dispatch" "d" 'org-export-dispatch
;;           )
;;         )
;;       )
;;   )

;; Variable setting
(setq org-roam-directory "~/org/org-roam")
(setq
  org-journal-dir "~/org/org-roam/journal"
  org-journal-file-format "%Y%m%d.org"
      )

(after! org
  (setq
    ;; | divides between "active" and "done" statuses
    org-todo-keywords '((sequence "DOING(d!)" "NEXT(n!)" "TODO(t!)" "BACKLOG(l!)" "BLOCKED(b!)" "WAITING(w!)" "FOLLOWUP(f!)" "INREVIEW(r!)" "|" "DONE(F!)" "CANCELLED(C!)" ))
    org-todo-keyword-faces
    '(("TODO" :foreground "#98BE65" :weight bold)
      ("NEXT" :foreground "MediumSeaGreen" :weight bold)
      ("WAITING" :foreground "LightSkyBlue" :weight bold)
      ("DOING" :foreground "DodgerBlue1" :weight bold)
      ("BACKLOG" :foreground "SeaGreen" :weight bold)
      ("INREVIEW" :foreground "gold1" :weight bold)
      ("FOLLOWUP" :foreground "LightSalmon" :weight bold)
      ("BLOCKED" :foreground "firebrick2" :weight bold)
      ("DONE" :foreground "LightSlateGray" :weight bold :strike-through t)
      ("CANCELLED" :foreground "LightSlateGray" :weight bold :strike-through t))
  )
)

(setq
  deft-directory "~/org"
  deft-extensions '("md" "org" "txt")
  deft-recursive t
)

;; startup hooks

;; start org-roam-mode on startup
(add-hook 'after-init-hook 'org-roam-mode)

;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)
