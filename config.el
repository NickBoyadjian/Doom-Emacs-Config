(setq user-full-name "Nick Boyadjian"
      user-mail-address "nick.boyadjian@podium.com"
      doom-font (font-spec :family "FiraCode Nerd Font" :size 15)
      doom-theme 'catppuccin-frappe
      display-line-numbers-type 'relative
      projectile-project-search-path '("~/projects/podium")
      fancy-splash-image "~/Pictures/luffy.png")

(setq doom-theme 'catppuccin-frappe)
;; (use-package kaolin-themes
;;   :ensure t
;;   :config
;;   (load-theme 'kaolin-bubblegum t)
;;   (kaolin-treemacs-theme))

;; (custom-set-faces
;;   '(mode-line ((t (:family "FiraCode Nerd Font"))))
;;   '(mode-line-active ((t (:family "FiraCode Nerd Font")))) ; For 29+
;;   '(mode-line-inactive ((t (:family "FiraCode Nerd Font"))))
;;   '(org-level-4 ((t (:height 0.8 :foreground "#ff665c"))))
;;   '(org-level-3 ((t (:height 0.95 :foreground "#a991f1"))))
;;   '(org-level-2 ((t (:height 1.1 :foreground "#C57BDB"))))
;;   '(org-level-1 ((t (:height 1.35 :foreground "#51afef"))))
;;   '(org-document-title ((t (:height 1.6 :underline nil))))
;; )

(defun open-notes ()
  (interactive)
    (display-buffer (find-file-noselect "~/.org/notes.org")
                    '(display-buffer-in-side-window . (( side . right ))))
    (other-window 1))

(map! :leader
      :desc "Open notes file"
      "o n" #'open-notes)

(defun open-agenda-file()
  (interactive)
    (display-buffer (find-file-noselect "~/org/todo.org")
                    '(display-buffer-in-side-window . (( side . right ))))
    (other-window 1))

(map! :leader
      :desc "Open notes file"
      "o N" #'open-agenda-file)

(after! org
  (setq
   org-ellipsis " ↓ "
   org-directory "~/org/"
   org-startup-folded t))

(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "FiraCode Nerd Font" :size 15))))
 '(fixed-pitch ((t ( :family "FiraCode Nerd Font" :height 160)))))

 (add-hook 'org-mode-hook 'variable-pitch-mode)

(after! org
  (set-ligatures! 'org-mode
    :merge t
    :checkbox      "[ ]"
    :pending       "[-]"
    :checkedbox    "[X]"
    :list_property "::"
    :em_dash       "---"
    :ellipsis      "..."
    :arrow_right   "->"
    :arrow_left    "<-"
    :title         "#+title:"
    :subtitle      "#+subtitle:"
    :author        "#+author:"
    :date          "#+date:"
    :property      "#+property:"
    :options       "#+options:"
    :startup       "#+startup:"
    :macro         "#+macro:"
    :html_head     "#+html_head:"
    :html          "#+html:"
    :latex_class   "#+latex_class:"
    :latex_header  "#+latex_header:"
    :beamer_header "#+beamer_header:"
    :latex         "#+latex:"
    :attr_latex    "#+attr_latex:"
    :attr_html     "#+attr_html:"
    :attr_org      "#+attr_org:"
    :begin_quote   "#+begin_quote"
    :end_quote     "#+end_quote"
    :caption       "#+caption:"
    :header        "#+header:"
    :begin_export  "#+begin_export"
    :end_export    "#+end_export"
    :results       "#+RESULTS:"
    :property      ":PROPERTIES:"
    :end           ":END:"
    :priority_a    "[#A]"
    :priority_b    "[#B]"
    :priority_c    "[#C]"
    :priority_d    "[#D]"
    :priority_e    "[#E]"
    :roam_tags     "#+roam_tags:"
    :filetags      "#+filetags:"
  ))

(setq ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-fold-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 ;; Org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities t
 org-ellipsis "…"

 ;; Agenda styling
 org-agenda-tags-column 0
 org-agenda-block-separator ?─
 org-agenda-time-grid
 '((daily today require-timed)
   (800 1000 1200 1400 1600 1800 2000)
   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
 org-agenda-current-time-string
 "⭠ now ─────────────────────────────────────────────────")
(global-org-modern-mode)

(add-hook 'js2-mode-hook
          'prettier-js-mode)
(setq js2-basic-offset 4)

(defun nick/enter-pipe ()
  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)
    (insert "|> ")))

(evil-define-key 'insert 'elixir-mode-map (kbd "<C-return>") 'nick/enter-pipe)

(after! lsp-mode
  (setq lsp-idle-delay 1.0
        lsp-log-io nil
        lsp-use-plists t
        read-process-output-max (* 1024 1024)) ;; 1mb
  gc-cons-threshold (* 1024 1024 100)) ;; 100MiB

(defadvice! +lsp-diagnostics--flycheck-buffer ()
  :override #'lsp-diagnostics--flycheck-buffer
  "Trigger flycheck on buffer."
  (remove-hook 'lsp-on-idle-hook #'lsp-diagnostics--flycheck-buffer t)
  (when (bound-and-true-p flycheck-mode)
    (flycheck-buffer)))


(after! lsp-mode
  (setq lsp-lens-enable t
        lsp-semantic-tokens-enable t ;; hide unreachable ifdefs
        lsp-enable-symbol-highlighting t
        lsp-headerline-breadcrumb-enable nil
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-symbol nil
        lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-show-code-actions nil
        lsp-elixir-suggest-specs nil))

(after! centaur-tabs
  (centaur-tabs-group-by-projectile-project)
  (centaur-tabs-mode 0)
  (setq centaur-tabs-height 36
        centaur-tabs-style "wave"
        centaur-tabs-set-icons t
        centaur-tabs-modified-marker "o"
        centaur-tabs-close-button "×"
        centaur-tabs-set-bar 'above
        centaur-tabs-gray-out-icons 'buffer))

(use-package! doom-nano-modeline
  :init
  (setq doom-nano-modeline-position 'bottom)

  :config
  (doom-nano-modeline-mode 1))

;; (use-package moody
;;   :ensure t
;;   :config
;;   (setq x-underline-at-descent-line t)

;;   (setq-default mode-line-format
;;                 '(" "
;;                   mode-line-front-space
;;                   mode-line-client
;;                   mode-line-frame-identification
;;                   mode-line-buffer-identification " " mode-line-position
;;                   (vc-mode vc-mode)
;;                   (multiple-cursors-mode mc/mode-line)
;;                   " " mode-line-modes
;;                   mode-line-end-spaces))

;;   (use-package minions
;;     :ensure t
;;     :config
;;     (minions-mode +1))

;;   (setq global-mode-string (remove 'display-time-string global-mode-string))

;;   (moody-replace-mode-line-buffer-identification)
;;   (moody-replace-vc-mode))

(map! :leader
      :desc "Open ranger"
      "o ." #'ranger)

(use-package blamer
  :bind (("s-i" . blamer-show-commit-info))
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                    :background nil
                    :height 140
                    :italic t)))
  :config
  (global-blamer-mode -1))

;; (use-package all-the-icons)
;; (use-package neotree
;;   :config
;;   (setq neo-smart-open t
;;         neo-window-width 30
;;         neo-window-fixed-size nil
;;         neo-window-position 'left
;;         neo-theme (if (display-graphic-p) 'icons 'arrow)
;;         projectile-switch-project-action 'neotree-projectile-action)
;;   ;; truncate long file names in neotree
;;   (add-hook 'neo-after-create-hook
;;             #'(lambda (_)
;;                 (with-current-buffer (get-buffer neo-buffer-name)
;;                   (setq truncate-lines t)
;;                   (setq word-wrap nil)
;;                   (setq neo-smart-open t)
;;                   (setq neo-window-position 'left)
;;                   (make-local-variable 'auto-hscroll-mode)
;;                   (setq auto-hscroll-mode nil)))))
;; (doom-themes-neotree-config)
;; (setq doom-themes-neotree-file-icons t)

(global-emojify-mode)

(treemacs-load-theme "all-the-icons")
