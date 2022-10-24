;; NOTE: init.el is now generated from Emacs.org.  Please edit that file
;;       in Emacs and init.el will be generated automatically!

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

;; Sets the directory used to install packages
(setq package-user-dir (expand-file-name "packages/" user-emacs-directory))

(add-to-list 'load-path "/Users/nikirby/.cache/emacs/packages/")

(package-initialize)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

;; Packages will download when evaluated. 
(require 'use-package)
(setq use-package-always-ensure t)

;; Disables the initial startup screen
(setq inhibit-startup-message t)

;; Configure the general UI

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar

;; Disable bell sound
(setq ring-bell-function 'ignore)

;; Configure no-littering defaults before loading
(setq no-littering-etc-directory (expand-file-name "etc/" user-emacs-directory)
      no-littering-var-directory (expand-file-name "var/" user-emacs-directory))

;; Use no-littering to automatically set common paths to the new user-emacs-directory
(require 'no-littering)

;; Moves customizations to custom.el
(setq custom-file (no-littering-expand-etc-file-name "custom.el"))

;; Moves the auto save files
(setq auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; Set font size
(set-face-attribute 'default nil :font "Fira Code Retina" :height 200)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height 200)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Helvetica" :height 200)

(use-package all-the-icons)

;; configure line numbers
(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)


;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(add-hook 'text-mode-hook 'visual-line-mode)

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-palenight t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline

  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-modal-icon nil))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.2))

(use-package swiper)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  :config
  (setq ivy-format-function #'ivy-format-function-line)
  (setq ivy-rich-display-transformers-list
        (plist-put ivy-rich-display-transformers-list
                   'ivy-switch-buffer
                   '(:columns
                     ((ivy-rich-candidate (:width 40))
                      (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right)); return the buffer indicators
                      (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))          ; return the major mode info
                      (ivy-rich-switch-buffer-project (:width 15 :face success))             ; return project name using `projectile'
                      (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))  ; return file path relative to project root or `default-directory' if project is nil
                     :predicate
                     (lambda (cand)
                       (if-let ((buffer (get-buffer cand)))
                           ;; Don't mess with EXWM buffers
                           (with-current-buffer buffer
                             (not (derived-mode-p 'exwm-mode)))))))))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         ("C-M-j" . counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^ automatically

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer kirby/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

(kirby/leader-keys
  "t"  '(:ignore t :which-key "Toggles")
  "tr" '(auto-revert-mode :which-key "Auto-reload file")
  "to" '(org-mode :which-key "Toggle Org Mode")
  "tw" '(toggle-word-wrap :which-key "Toggle Word Wrap")
  )

(kirby/leader-keys
  "b"  '(:ignore t :which-key "Buffers")
  "bb" '(ibuffer :which-key "iBuffer")
  "bk" '(kill-this-buffer :which-key "Kill Current Buffer")
  )

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package hydra)

(defhydra hydra-text-scale (:timeout 5)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(kirby/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

;; Modes that should start with evil disabled. C-z to activate evil.
(defun kirby/evil-hook ()
  (dolist (mode '(eshell-mode
                  git-rebase-mode
                  term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  ;;:hook (evil-mode . kirby/evil-hook)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  ;; Makes horizontal movement cross lines
  (setq-default evil-cross-lines t)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  ;; Color the evil tag
  (setq evil-normal-state-tag   (propertize "N" 'face '((:background "yellow1"     :foreground "black")))
        evil-emacs-state-tag    (propertize "E" 'face '((:background "dark red"    :foreground "black")))
        evil-insert-state-tag   (propertize "I" 'face '((:background "light green" :foreground "black")))
        evil-replace-state-tag  (propertize "R" 'face '((:background "chocolate"   :foreground "black")))
        evil-motion-state-tag   (propertize "M" 'face '((:background "plum3"       :foreground "black")))
        evil-visual-state-tag   (propertize "V" 'face '((:background "gray"        :foreground "black")))
        evil-operator-state-tag (propertize "O" 'face '((:background "sandy brown" :foreground "black")))))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package vterm
  :commands vterm
  :config
  ;; (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (setq vterm-shell "zsh")
  (setq vterm-max-scrollback 10000))

(defun kirby/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell
  :hook (eshell-first-time-mode . kirby/configure-eshell)
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))

  (eshell-git-prompt-use-theme 'powerline))

(use-package eshell-git-prompt
  :after eshell)

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-liahF --group-directories-first --time-style=iso"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer)
  :init
  (setq insert-directory-program "gls" dired-use-ls-dired t)
  )

(use-package dired-single)

(defun kirby/my-dired-init ()
  (define-key dired-mode-map [remap dired-find-file]
    'dired-single-buffer)
  (define-key dired-mode-map [remap dired-mouse-find-file-other-window]
    'dired-single-buffer-mouse)
  (define-key dired-mode-map [remap dired-up-directory]
    'dired-single-up-directory))

;; if dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
    ;; we're good to go; just add our bindings
    (kirby/my-dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'kirby/my-dired-init))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :init (setq all-the-icons-dired-monochrome nil)
  )

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(use-package dired-open
  :config
  ;; (add-to-list 'dired-open-function #'dired-open-xdg t)
  ;; To configure an extension add ("<extention>" . "file")
  (setq dired-open-extensions '(("pdf" . "open"))
        ))

(defun kirby/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil))

(use-package org
  :hook (org-mode . kirby/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"))

;; Replace list hyphen with dot
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                          (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; Make sure org-indent face is available
(require 'org-indent)

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("➀" "➁" "➂" "➃" "➄" "➅" "➆" "➇" "➈" "➉")))

(require 'org-faces)

  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
      (set-face-attribute (car face) nil :font "Fira Code Retina" :weight 'regular :height (cdr face)))

;; Run describe-face and search for org to find other faces that might need fixed pitch
;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil           :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil            :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-table nil           :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil          :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil        :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil       :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil        :inherit 'fixed-pitch)

;; enables org babel
(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)   ; Enables emacs-lisp language
    (python . t)))     ; Enables python language

;; Enabled config file configuration in org babel
(push '("conf-unix" . conf-unix) org-src-lang-modes)


;; Allows evaluation of code blocks without prompts
(setq org-confirm-babel-evaluate nil)

;; Enables tab completion of "<sh" to a shell code block. Same for other characters
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

;; Automatically tangle our Emacs.org config file when we save it
(defun kirby/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.dotfiles/emacs.org")) 
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle)
      (load-file user-init-file)))
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.dotfiles/system.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'kirby/org-babel-tangle-config)))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projecile-completion-system `ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/source_code")
    (setq projectile-project-search-path '("~/source_code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
 :after projectile
 :config
 (counsel-projectile-mode))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (git-commit-style-convention-checks 'nil)
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge
  :init
  (setq auth-sources '("~/.authinfo")))

;; enable rainbow-delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(defun kirby/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook
  (lsp-mode . kirby/lsp-mode-setup)
  (sh-mode . lsp)			; Enable Bash LSP
  :init
  (setq lsp-keymap-prefix "SPC l") 
  :config
  (lsp-enable-which-key-integration t))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1) ; Minimum characters before completions show up
  (company-idle-delay 0.0))         ; Delay before completion shows up

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-treemacs		
  :after lsp-mode)

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))
