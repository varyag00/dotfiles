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
(setq doom-theme 'doom-one-light)
;; (setq doom-font (font-spec :family "Cascadia Mono PL" :size 12))
(setq doom-font (font-spec :family "JetBrains Mono" :size 12))

(defun synchronize-theme ()
  "Change doom colour theme at specified times of day."
  (let* ((light-theme 'doom-one-light)
         (dark-theme 'doom-one)
         (start-time-light-theme 6)
         (end-time-light-theme 19)
         (hour (string-to-number (substring (current-time-string) 11 13)))
         (next-theme (if (member hour (number-sequence start-time-light-theme end-time-light-theme))
                         light-theme dark-theme)))
    (when (not (equal doom-theme next-theme))
      (setq doom-theme next-theme)
      (load-theme next-theme))))

(run-with-timer 0 900 'synchronize-theme)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;;(setq display-line-numbers-type t)
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

;; org packages

;;(require 'org-download)
;;(require 'org-alert)


;; Key Mappings

;; TODO delete old journal
;; (map! :leader
;;       (:prefix ("j" . "journal") ;; org-journal bindings
;;         :desc "Create new journal entry" "j" #'org-journal-new-entry
;;         :desc "Open previous entry" "p" #'org-journal-open-previous-entry
;;         :desc "Open next entry" "n" #'org-journal-open-next-entry
;;         :desc "Search journal" "s" #'org-journal-search-forever))

(map! :leader
      (:prefix ("j" . "org-roam-dailies") ;; org-journal bindings
        :desc "Create new daily entry" "j" #'org-roam-dailies-goto-today
        :desc "Quickly capture for today" "c" #'org-roam-dailies-capture-today
        :desc "Open previous entry" "p" #'org-roam-dailies-goto-previous-note
        :desc "Open next entry" "n" #'org-roam-dailies-goto-next-note
        :desc "Search for entry" "s" #'org-roam-dailies-capture-date))
;; configure org-journal capture templates
;; NOTE this seems to break things, see https://github.com/bastibe/org-journal
 ;; (defun org-journal-find-location ()
 ;;  ;; Open today's journal, but specify a non-nil prefix argument in order to
 ;;  ;; inhibit inserting the heading; org-capture will insert the heading.
 ;;  (org-journal-new-entry t)
 ;;  (unless (eq org-journal-file-type 'daily)
 ;;    (org-narrow-to-subtree))
 ;;  (goto-char (point-max)))

;; don't overwrite org-capture-templates; simply append
;; (setf (alist-get "j" org-capture-templates nil nil #'string-equal)
;;                  `("Journal entry"
;;                    plain
;;                    (function ,#'org-journal-find-location)
;;                    "** %(format-time-string org-journal-time-format)%^{Title}\n%i%?"
;;                    :jump-to-captured t :immediate-finish t))

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
(setq org-roam-dailies-directory "journal/")

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :target (file+head "%<%Y%m%d>.org"
                            "#+title: %<%A, %Y-%m-%d>\n#+created: %U\n#+last_modified: %U\n\n"
                            ))))

;; (setq
;;   org-journal-dir "~/org/org-roam/journal"
;;   org-journal-file-format "%Y%m%d.org"
;;   org-journal-carryover-items nil)

;; open org files showing all headlines, hiding everything all
(setq org-startup-folded "overview")

(after! org
  (setq
    ;; | divides between "active" and "done" statuses
    org-todo-keywords '((sequence "DOING(d!)" "NEXT(n!)" "TODO(t!)" "PLANNED(p!)" "BACKLOG(l!)" "BLOCKED(b!)" "DELEGATED(g!)" "WAITING(w!)" "FOLLOWUP(f!)" "INREVIEW(r!)" "|" "DONE(F!)" "CANCELLED(C!)" ))
    org-todo-keyword-faces
    ;; TODO try adding a :background!
    '(("TODO" :foreground "#98BE65" :weight bold)
      ("NEXT" :foreground "MediumSeaGreen" :weight bold)
      ("PLANNED" :foreground "DarkGoldenrod" :weight bold)
      ("WAITING" :foreground "LightSkyBlue" :weight bold)
      ("DOING" :foreground "DodgerBlue1" :weight bold)
      ("BACKLOG" :foreground "SeaGreen" :weight bold)
      ("INREVIEW" :foreground "gold1" :weight bold)
      ("FOLLOWUP" :foreground "LightSalmon" :weight bold)
      ("BLOCKED" :foreground "firebrick2" :weight bold)
      ("DELEGATED" :foreground "LightPink" :weight bold)
      ("DONE" :foreground "LightSlateGray" :weight bold :strike-through t)
      ("CANCELLED" :foreground "LightSlateGray" :weight bold :strike-through t))
  )
)
(setq org-tag-alist (quote (("@errand" . ?e)
                            ("@office" . ?o)
                            ("@home" . ?h)
                            )))

(setq org-roam-capture-templates
 `(("d" "default" plain "%?"
  :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                     "#+title: ${title}\n#+created: %U\n#+last_modified: %U\n#+filetags: \n\n")
  :unnarrowed t))
 )

(defun dan/org-mode-hook ()
  "Run this when org mode is loaded."

  ;;Global minor mode to keep your Org-roam session automatically synchronized on save
  ;;(org-roam-db-autosync-mode)


  ;;Increase size of header font
  (set-face-attribute `org-document-title nil :weight 'bold :height 1.5)
  (set-face-attribute `org-level-1 nil :weight 'semi-bold :height 1.3)
  (set-face-attribute `org-level-2 nil :weight 'semi-bold :height 1.25)
  (set-face-attribute `org-level-3 nil :weight 'semi-bold :height 1.2)

  ;; I think this means to set the value of face to each element in the list
  (dolist (face '(org-level-4
                  org-level-5
                  org-level-6
                  org-level-7
                  org-level-8))
    (set-face-attribute face nil :weight 'semi-bold :height 1.15))


  ;; not sure how I feel about this one, it makes emphasis hard to remove (unless I'm missing a keybinding...)
  ;; (setq org-hide-emphasis-markers t)
  ;; these already run with (org +pretty) doom config
 ;;
 ;; (setq org-superstar-prettify-item-bullets t)
 ;; (setq org-superstar-prettify-leading-stars t)
 ;;
  ;; (setq org-hide-leading-stars t
  ;;       org-pretty-entities t
  ;;     ;; org-hide-emphasis-markers nil
  ;; )

  ;; updates last modified time
  ;; NOTE: requires that org capture templates include LAST_MODIFIED: in the first 8 lines
  ;; so this is added in the capture templates above
  ;; use seq1-local because these vars should not be changed globally
  (setq-local
   time-stamp-start "#\\+last_modified:[ \t]*"
   time-stamp-active t
   time-stamp-end "$"
   time-stamp-format "\[%Y-%02m-%02d %3a %02H:%02M\]")
  (add-hook 'before-save-hook 'time-stamp nil 'local)

)
;; run before org-mode starts
(add-hook 'org-mode-hook 'dan/org-mode-hook)
;; allows communication with external apps, such as chrome for org-roam-server
;; must register the protocol before use, see https://www.orgroam.com/manual.html#Org_002droam-Protocol
(require 'org-roam-protocol)

(setq
  deft-directory "~/org"
  deft-extensions '("md" "org" "txt")
  deft-recursive t
)

;; startup hooks

;; add LAST_MODIFIED timestamp to org-files
;; source: https://github.com/skx/dotfiles/blob/master/.emacs.d/init.md#org-mode-timestamping
;; TODO doesn't seem to work; fix it
;; (defun skx/update-org-modified-property ()
;;   "If a file contains a '#+LAST_MODIFIED' property update it to contain
;;   the current date/time"
;;   (interactive)
;;   (save-excursion
;;     (widen)
;;     (goto-char (point-min))
;;     (when (re-search-forward "^#\\+LAST_MODIFIED:" (point-max) t)
;;       (progn
;;         (kill-line)
;;         (insert (format-time-string " %d/%m/%Y %H:%M:%S") )))))

;; (defun skx-org-mode-before-save-hook ()
;;   (when (eq major-mode 'org-mode)
;;     (skx/update-org-modified-property)))

;; (add-hook 'before-save-hook #'skx-org-mode-before-save-hook)

;; Drag-and-drop to `dired`
;;(add-hook 'dired-mode-hook 'org-download-enable)

;; enable emojis
(add-hook 'after-init-hook #'global-emojify-mode)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 ;;set above
 ;; '(org-document-title ((t (:foreground "#c678dd" :weight bold :height 1.3))))
)

(use-package! winum
  :config
  (winum-mode)
;; CTRL + num window shortcuts
  (map! :ne "C-1" #'winum-select-window-1)
  (map! :ne "C-2" #'winum-select-window-2)
  (map! :ne "C-3" #'winum-select-window-3)
  (map! :ne "C-4" #'winum-select-window-4)
  (map! :ne "C-5" #'winum-select-window-5)
  (map! :ne "C-6" #'winum-select-window-6)

;; SPC + num window shortcuts
  (map! :ne "SPC 1" #'winum-select-window-1)
  (map! :ne "SPC 2" #'winum-select-window-2)
  (map! :ne "SPC 3" #'winum-select-window-3)
  (map! :ne "SPC 4" #'winum-select-window-4)
  (map! :ne "SPC 5" #'winum-select-window-5)
  (map! :ne "SPC 6" #'winum-select-window-6)
  (map! :ne "SPC 7" #'winum-select-window-7)
  (map! :ne "SPC 8" #'winum-select-window-8)
  (map! :ne "SPC 9" #'winum-select-window-9)
  (map! :ne "SPC 0" #'winum-select-window-0-or-10)
)

;; mouse buttons
(map! :ne [mouse-8] #'better-jumper-jump-backward)
(map! :ne [mouse-9] #'better-jumper-jump-forward)

;; TODO these seem to not really work
;; map C-i back to jump forward in evil normal mode
;; (map! :n "<C-i>" #'better-jumper-jump-backward)
;; (map! :n "<C-o>" #'better-jumper-jump-forward)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/org/org-roam/journal/20200826.org"))))
