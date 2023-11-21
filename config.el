(setq user-full-name "Nick Boyadjian"
      user-mail-address "nick.boyadjian@podium.com"
      doom-font (font-spec :family "FiraCode Nerd Font" :size 15)
      display-line-numbers-type 'relative
      projectile-project-search-path '("~/projects/podium")
      fancy-splash-image "~/Pictures/luffy.png")

(setq doom-theme 'doom-challenger-deep)

(custom-set-faces
  '(org-document-title ((t (:height 1.6 :underline nil)))))

(defun nick/open-notes ()
  (interactive)
    (display-buffer (find-file-noselect "~/org/notes.org")
                    '(display-buffer-in-side-window . (( side . right ))))
    (other-window 1))

(map! :leader
      :desc "Open notes file"
      "o n" #'nick/open-notes)

(defun nick/open-agenda-file()
  (interactive)
    (display-buffer (find-file-noselect "~/org/todo.org")
                    '(display-buffer-in-side-window . (( side . right ))))
    (other-window 1))

(map! :leader
      :desc "Open notes file"
      "o N" #'nick/open-agenda-file)

(map! :leader
      :desc "Apheleia mode"
      "t a" 'apheleia-mode)

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

(add-hook 'elixir-mode-hook #'lsp)
(use-package alchemist
  :diminish (alchemist-mode alchemist-phoenix-mode)
  :hook ((elixir-mode . alchemist-mode)
         (elixir-mode . alchemist-phoenix-mode)))

(defun nick/enter-pipe ()
  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)
    (insert "|> ")))

(add-hook 'elixir-mode-hook
          (lambda ()
            (define-key evil-insert-state-local-map
                        (kbd "<C-return>") 'nick/enter-pipe)))

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

(use-package! doom-nano-modeline
  :init
  (setq doom-nano-modeline-position 'bottom)

  :config
  (doom-nano-modeline-mode 1))

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

(use-package! treemacs-all-the-icons
  :config
  (treemacs-load-theme 'nerd-icons)
  (setq doom-themes-treemacs-theme 'nerd-icons)
  (map! :desc "Select Treemacs window" "<f8>" #'treemacs-select-window)

  (treemacs-follow-mode)
  (treemacs-toggle-fixed-width))

(after! persp-mode
  ;; alternative, non-fancy version which only centers the output of +workspace--tabline
  (defun workspaces-formatted ()
    (+doom-dashboard--center (frame-width) (+workspace--tabline)))

  (defun hy/invisible-current-workspace ()
    "The tab bar doesn't update when only faces change (i.e. the
current workspace), so we invisibly print the current workspace
name as well to trigger updates"
    (propertize (safe-persp-name (get-current-persp)) 'invisible t))

  (customize-set-variable 'tab-bar-format '(workspaces-formatted tab-bar-format-align-right hy/invisible-current-workspace))

  ;; don't show current workspaces when we switch, since we always see them
  (advice-add #'+workspace/display :override #'ignore)
  ;; same for renaming and deleting (and saving, but oh well)
  (advice-add #'+workspace-message :override #'ignore))

;; need to run this later for it to not break frame size for some reason
;; (run-at-time nil nil (cmd! (tab-bar-mode +1)))
