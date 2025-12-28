((axis :source "elpaca-menu-lock-file" :recipe
       (:source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless :host
        github :repo "junyi-hou/emacs-axis" :package "axis" :ref
        "0d747c293e65f60a96adc8b8ad290afeae919ac0"))
 (clojure-ts-mode :source "elpaca-menu-lock-file" :recipe
                  (:package "clojure-ts-mode" :repo "clojure-emacs/clojure-ts-mode"
                   :fetcher github :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                    "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                     "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :source "MELPA" :protocol https :inherit t :depth treeless :host
                   github :ref "31f12ea1e386fe0e8eb3641e8b55bd0103eeb111"))
 (cond-let
   :source "elpaca-menu-lock-file" :recipe
   (:package "cond-let" :fetcher github :repo "tarsius/cond-let" :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
     "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
      "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless :ref
    "288b7d36563223ebaf64cb220a3b270bdffb63f1"))
 (consult :source "elpaca-menu-lock-file" :recipe
          (:package "consult" :repo "minad/consult" :fetcher github :files
           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
            "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
            "docs/*.texi" "docs/*.texinfo"
            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
             "LICENSE" "README*" "*-pkg.el"))
           :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
           :host github :ref "da62326326b9b121edb46b0f31adb2569e4e896a"))
 (corfu :source "elpaca-menu-lock-file" :recipe
        (:package "corfu" :repo "emacs-straight/corfu" :files
         ("*" "extensions/*.el" (:exclude ".git")) :fetcher github :source
         "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless :host github
         :ref "b922d589c2e5e1a1577f872c066fe5e534f9dfc8"))
 (dash :source "elpaca-menu-lock-file" :recipe
       (:package "dash" :fetcher github :repo "magnars/dash.el" :files
        ("dash.el" "dash.texi") :source "elpaca-menu-lock-file" :protocol https :inherit
        t :depth treeless :ref "fb443e7a6e660ba849cafcd01021d9aac3ac6764"))
 (doom-themes :source "elpaca-menu-lock-file" :recipe
              (:package "doom-themes" :fetcher github :repo "doomemacs/themes" :files
               (:defaults "themes/*.el" "themes/*/*.el" "extensions/*.el") :source
               "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless :host
               github :ref "376cf4bdd7d296a3da94aa9a6c68761e7c38a252"))
 (eldoc-mouse :source "elpaca-menu-lock-file" :recipe
              (:package "eldoc-mouse" :fetcher github :repo "huangfeiyu/eldoc-mouse"
               :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                 "LICENSE" "README*" "*-pkg.el"))
               :source "elpaca-menu-lock-file" :protocol https :inherit t :depth
               treeless :host github :ref "614a57d1b19a66e5e71510a9aaf7d6a17fce6bae"))
 (elisp-refs :source "elpaca-menu-lock-file" :recipe
             (:package "elisp-refs" :repo "Wilfred/elisp-refs" :fetcher github :files
              (:defaults (:exclude "elisp-refs-bench.el")) :source
              "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless :ref
              "541a064c3ce27867872cf708354a65d83baf2a6d"))
 (elpaca :source
   "elpaca-menu-lock-file" :recipe
   (:source nil :protocol https :inherit ignore :depth 1 :repo
    "https://github.com/progfolio/elpaca.git" :ref
    "b5ef5f19ac1224853234c9acdac0ec9ea1c440a1" :files
    (:defaults "elpaca-test.el" (:exclude "extensions")) :build
    (:not elpaca--activate-package) :package "elpaca"))
 (elpaca-use-package :source "elpaca-menu-lock-file" :recipe
                     (:package "elpaca-use-package" :wait t :repo
                      "https://github.com/progfolio/elpaca.git" :files
                      ("extensions/elpaca-use-package.el") :main
                      "extensions/elpaca-use-package.el" :build
                      (:not elpaca--compile-info) :source "elpaca-menu-lock-file"
                      :protocol https :inherit t :depth treeless :ref
                      "b5ef5f19ac1224853234c9acdac0ec9ea1c440a1"))
 (envrc :source "elpaca-menu-lock-file" :recipe
        (:package "envrc" :fetcher github :repo "purcell/envrc" :files
         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
          "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
          "docs/*.texi" "docs/*.texinfo"
          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
           "LICENSE" "README*" "*-pkg.el"))
         :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
         :host github :ref "de1ae6e538764f74659f358b04af0d84fa0fef42"))
 (evil :source "elpaca-menu-lock-file" :recipe
       (:package "evil" :repo "emacs-evil/evil" :fetcher github :files
        (:defaults "doc/build/texinfo/evil.texi" (:exclude "evil-test-helpers.el"))
        :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless :host
        github :ref "b06f644bdb5b06c6ac46c11b0259f15ac9ffd5da"))
 (evil-nerd-commenter :source "elpaca-menu-lock-file" :recipe
                      (:package "evil-nerd-commenter" :fetcher github :repo
                       "redguardtoo/evil-nerd-commenter" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                        "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                        "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                         "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :protocol https :inherit t :depth
                       treeless :host github :ref
                       "ae52c5070a48793e2c24474c9c8dbf20175d18a0"))
 (evil-surround :source "elpaca-menu-lock-file" :recipe
                (:package "evil-surround" :repo "emacs-evil/evil-surround" :fetcher
                 github :old-names (surround) :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                  "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                   "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :source "elpaca-menu-lock-file" :protocol https :inherit t :depth
                 treeless :host github :ref "da05c60b0621cf33161bb4335153f75ff5c29d91"))
 (expand-region :source "elpaca-menu-lock-file" :recipe
                (:package "expand-region" :repo "magnars/expand-region.el" :fetcher
                 github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                  "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                   "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :source "elpaca-menu-lock-file" :protocol https :inherit t :depth
                 treeless :host github :ref "351279272330cae6cecea941b0033a8dd8bcc4e8"))
 (f :source "elpaca-menu-lock-file" :recipe
    (:package "f" :fetcher github :repo "rejeep/f.el" :files
     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
      "docs/*.texinfo"
      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
       "README*" "*-pkg.el"))
     :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless :ref
     "931b6d0667fe03e7bf1c6c282d6d8d7006143c52"))
 (flymake-childframe :source "elpaca-menu-lock-file" :recipe
                     (:source "elpaca-menu-lock-file" :protocol https :inherit t :depth
                      treeless :host github :repo "junyi-hou/flymake-childframe"
                      :package "flymake-childframe" :ref
                      "8f46751a25a130be7138ce7d6f4c53d7d5cf9989"))
 (gcmh :source "elpaca-menu-lock-file" :recipe
       (:package "gcmh" :repo "emacsmirror/gcmh" :fetcher gitlab :files
        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
         "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
         "docs/*.texinfo"
         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
          "LICENSE" "README*" "*-pkg.el"))
        :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless :host
        github :wait t :ref "0089f9c3a6d4e9a310d0791cf6fa8f35642ecfd9"))
 (goto-chg :source "elpaca-menu-lock-file" :recipe
           (:package "goto-chg" :repo "emacs-evil/goto-chg" :fetcher github :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
             "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
             "docs/*.texi" "docs/*.texinfo"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
              "LICENSE" "README*" "*-pkg.el"))
            :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
            :ref "72f556524b88e9d30dc7fc5b0dc32078c166fda7"))
 (gptel :source "elpaca-menu-lock-file" :recipe
        (:package "gptel" :repo "karthink/gptel" :fetcher github :files
         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
          "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
          "docs/*.texi" "docs/*.texinfo"
          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
           "LICENSE" "README*" "*-pkg.el"))
         :source "MELPA" :protocol https :inherit t :depth treeless :host github :ref
         "0f65be08ead0c9bc882fad5a4dcb604448e366a6"))
 (helpful :source "elpaca-menu-lock-file" :recipe
          (:package "helpful" :repo "Wilfred/helpful" :fetcher github :files
           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
            "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
            "docs/*.texi" "docs/*.texinfo"
            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
             "LICENSE" "README*" "*-pkg.el"))
           :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
           :host github :ref "03756fa6ad4dcca5e0920622b1ee3f70abfc4e39"))
 (hl-todo :source "elpaca-menu-lock-file" :recipe
          (:package "hl-todo" :repo "tarsius/hl-todo" :fetcher github :files
           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
            "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
            "docs/*.texi" "docs/*.texinfo"
            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
             "LICENSE" "README*" "*-pkg.el"))
           :source "MELPA" :protocol https :inherit t :depth treeless :host github :ref
           "94893087e0aca6642a3ebf11f46b3d5f47c1eb22"))
 (ht :source "elpaca-menu-lock-file" :recipe
     (:package "ht" :fetcher github :repo "Wilfred/ht.el" :files
      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
       "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
       "docs/*.texinfo"
       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
        "LICENSE" "README*" "*-pkg.el"))
      :source "MELPA" :protocol https :inherit t :depth treeless :ref
      "1c49aad1c820c86f7ee35bf9fff8429502f60fef"))
 (indent-bars :source "elpaca-menu-lock-file" :recipe
              (:package "indent-bars" :repo "jdtsmith/indent-bars" :files
               ("*" (:exclude ".git" "LICENSE")) :source "elpaca-menu-lock-file"
               :protocol https :inherit t :depth treeless :host github :ref
               "62d7b959455f89eeea55308d33d52dd7a73c6f48"))
 (inheritenv :source "elpaca-menu-lock-file" :recipe
             (:package "inheritenv" :fetcher github :repo "purcell/inheritenv" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
               "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
               "docs/*.info" "docs/*.texi" "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                "LICENSE" "README*" "*-pkg.el"))
              :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
              :ref "b9e67cc20c069539698a9ac54d0e6cc11e616c6f"))
 (jarchive :source "elpaca-menu-lock-file" :recipe
           (:package "jarchive" :repo "https://git.sr.ht/~dannyfreeman/jarchive" :files
            ("*" (:exclude ".git" "LICENSE")) :source "GNU ELPA" :protocol https
            :inherit t :depth treeless :type git :ref
            "9b5edcb9ac4937ad41bc830714534fec5ebe48ee"))
 (jupyter :source "elpaca-menu-lock-file" :recipe
          (:package "jupyter" :fetcher github :repo "nnicandro/emacs-jupyter" :files
           (:defaults "Makefile" "widget.html" "js") :source "elpaca-menu-lock-file"
           :protocol https :inherit t :depth treeless :host github :ref
           "3615c2de16988c4dd9d1978bfa10ee3092e85b33"))
 (llama :source "elpaca-menu-lock-file" :recipe
        (:package "llama" :fetcher github :repo "tarsius/llama" :files
         ("llama.el" ".dir-locals.el") :source "elpaca-menu-lock-file" :protocol https
         :inherit t :depth treeless :ref "e4803de8ab85991b6a944430bb4f543ea338636d"))
 (lsp-mode :source "elpaca-menu-lock-file" :recipe
           (:package "lsp-mode" :repo "emacs-lsp/lsp-mode" :fetcher github :files
            (:defaults "clients/*.*") :source "MELPA" :protocol https :inherit t :depth
            treeless :host git :ref "4cedcbf453e7f199cdcf457657fd50657db81c42"))
 (lv :source "elpaca-menu-lock-file" :recipe
     (:package "lv" :repo "abo-abo/hydra" :fetcher github :files ("lv.el") :source
      "MELPA" :protocol https :inherit t :depth treeless :ref
      "59a2a45a35027948476d1d7751b0f0215b1e61aa"))
 (macher :source "elpaca-menu-lock-file" :recipe
         (:source nil :protocol https :inherit t :depth treeless :host github :repo
          "kmontag/macher" :package "macher" :ref
          "e6b13bac2a1893e533ce4857e947f7de22df927e"))
 (magit :source "elpaca-menu-lock-file" :recipe
        (:package "magit" :fetcher github :repo "magit/magit" :files
         ("lisp/magit*.el" "lisp/git-*.el" "docs/magit.texi" "docs/AUTHORS.md" "LICENSE"
          ".dir-locals.el" (:exclude "lisp/magit-section.el"))
         :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
         :host github :ref "2d8f43e68125d9f7cf97ba182a5d266fe1a52c67"))
 (magit-section :source "elpaca-menu-lock-file" :recipe
                (:package "magit-section" :fetcher github :repo "magit/magit" :files
                 ("lisp/magit-section.el" "docs/magit-section.texi"
                  "magit-section-pkg.el")
                 :source "elpaca-menu-lock-file" :protocol https :inherit t :depth
                 treeless :ref "2d8f43e68125d9f7cf97ba182a5d266fe1a52c67"))
 (markdown-mode :source "elpaca-menu-lock-file" :recipe
                (:package "markdown-mode" :fetcher github :repo "jrblevin/markdown-mode"
                 :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                  "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                   "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :source "elpaca-menu-lock-file" :protocol https :inherit t :depth
                 treeless :ref "b524618c3ed28906a7522482727f121428ce7e2e"))
 (markdown-ts-mode :source "elpaca-menu-lock-file" :recipe
                   (:package "markdown-ts-mode" :repo "LionyxML/markdown-ts-mode"
                    :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                     "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                      "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :protocol https :inherit t :depth
                    treeless :host github :ref
                    "2f1ee8b94cdf53cebc31ae08ecfbba846193d5e1"))
 (no-littering :source "elpaca-menu-lock-file" :recipe
               (:package "no-littering" :fetcher github :repo
                "emacscollective/no-littering" :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                 "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                  "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                :source "elpaca-menu-lock-file" :protocol https :inherit t :depth
                treeless :host github :wait t :ref
                "1b185ebe7bfeae1c24fccc6f90eff3ffef94689b"))
 (orderless :source "elpaca-menu-lock-file" :recipe
            (:package "orderless" :repo "oantolin/orderless" :fetcher github :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
              "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
              "docs/*.info" "docs/*.texi" "docs/*.texinfo"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
               "LICENSE" "README*" "*-pkg.el"))
             :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
             :host github :ref "9cf1c90e2501566ceba59f3220b4630995004efd"))
 (posframe :source "elpaca-menu-lock-file" :recipe
           (:package "posframe" :fetcher github :repo "tumashu/posframe" :files
            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
             "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
             "docs/*.texi" "docs/*.texinfo"
             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
              "LICENSE" "README*" "*-pkg.el"))
            :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
            :ref "12f540c9ad5da09673b2bca1132b41f94c134e82"))
 (prettier-elisp :source "elpaca-menu-lock-file" :recipe
                 (:source nil :protocol https :inherit t :depth treeless :host github
                  :repo "KarimAziev/prettier-elisp" :package "prettier-elisp" :ref
                  "c7bd35f1ea9ec0675aab1d2c01e509a0fdeb6cde"))
 (s :source "elpaca-menu-lock-file" :recipe
    (:package "s" :fetcher github :repo "magnars/s.el" :files
     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
      "docs/*.texinfo"
      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
       "README*" "*-pkg.el"))
     :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless :ref
     "dda84d38fffdaf0c9b12837b504b402af910d01d"))
 (simple-httpd :source "elpaca-menu-lock-file" :recipe
               (:package "simple-httpd" :repo "skeeto/emacs-web-server" :fetcher github
                :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                 "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                  "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                :source "elpaca-menu-lock-file" :protocol https :inherit t :depth
                treeless :ref "3982c55e9061475038a3ccd61aecb2de3d407cec"))
 (spinner :source "elpaca-menu-lock-file" :recipe
          (:package "spinner" :repo
           ("https://github.com/Malabarba/spinner.el" . "spinner") :files
           ("*" (:exclude ".git")) :source "GNU ELPA" :protocol https :inherit t :depth
           treeless :ref "d4647ae87fb0cd24bc9081a3d287c860ff061c21"))
 (tempel :source "elpaca-menu-lock-file" :recipe
         (:package "tempel" :repo "minad/tempel" :fetcher github :files
          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
           "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
           "docs/*.texi" "docs/*.texinfo"
           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
            "LICENSE" "README*" "*-pkg.el"))
          :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
          :host github :ref "98ecdb566771b7727420b39a1ababc816e53b30a"))
 (transient :source "elpaca-menu-lock-file" :recipe
            (:package "transient" :fetcher github :repo "magit/transient" :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
              "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
              "docs/*.info" "docs/*.texi" "docs/*.texinfo"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
               "LICENSE" "README*" "*-pkg.el"))
             :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
             :host github :ref "c04d1dc76611b19e3b4c9f63e79a7a02aa320685"))
 (typst-ts-mode :source "elpaca-menu-lock-file" :recipe
                (:package "typst-ts-mode" :repo "meow_king/typst-ts-mode" :files
                 ("*" (:exclude ".git")) :source "NonGNU ELPA" :protocol https :inherit
                 t :depth treeless :host sourcehut :ref
                 "1367003e2ad55a2f6f9e43178584683028ab56e9"))
 (vertico :source "elpaca-menu-lock-file" :recipe
          (:package "vertico" :repo "minad/vertico" :files
           (:defaults "extensions/vertico-*.el") :fetcher github :source
           "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless :host
           github :ref "17c629087ea23466b10fd0dd4ecce53e17a810e3"))
 (websocket :source "elpaca-menu-lock-file" :recipe
            (:package "websocket" :repo "ahyatt/emacs-websocket" :fetcher github :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
              "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
              "docs/*.info" "docs/*.texi" "docs/*.texinfo"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
               "LICENSE" "README*" "*-pkg.el"))
             :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
             :ref "40c208eaab99999d7c1e4bea883648da24c03be3"))
 (with-editor :source "elpaca-menu-lock-file"
   :recipe
   (:package "with-editor" :fetcher github :repo "magit/with-editor" :files
    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
     "docs/*.texinfo"
     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
      "README*" "*-pkg.el"))
    :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless :ref
    "dbc694406c2fd8e9d3e6ffbc4f8aff4e8c28029f"))
 (ws-butler :source "elpaca-menu-lock-file" :recipe
            (:package "ws-butler" :fetcher git :url
             "https://git.savannah.gnu.org/git/emacs/nongnu.git" :branch "master" :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
              "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
              "docs/*.info" "docs/*.texi" "docs/*.texinfo"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
               "LICENSE" "README*" "*-pkg.el"))
             :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
             :host github :repo "lewang/ws-butler" :ref
             "67c49cfdf5a5a9f28792c500c8eb0017cfe74a3a"))
 (zmq :source "elpaca-menu-lock-file" :recipe
      (:package "zmq" :fetcher github :repo "nnicandro/emacs-zmq" :files
       (:defaults "Makefile" "src") :source "elpaca-menu-lock-file" :protocol https
       :inherit t :depth treeless :ref "fe856c43286674aa6770d95a81d915363f5df399")))
