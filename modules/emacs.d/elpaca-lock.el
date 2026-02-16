((acp :source "elpaca-menu-lock-file" :recipe
      (:package "acp" :fetcher github :repo "xenodium/acp.el" :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                 "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                           "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                :source "elpaca-menu-lock-file" :protocol https :inherit t :depth
                treeless :ref "e5dec95452de8b4cbacf41156d4dfa261668ae31"))
 (agent-shell :source "elpaca-menu-lock-file" :recipe
              (:package "agent-shell" :fetcher github :repo "xenodium/agent-shell"
                        :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                         "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                   "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :protocol https :inherit t
                        :depth treeless :host github :ref
                        "c617076cae65468de645abdef4c8d07d7cf26c49"))
 (cape :source "elpaca-menu-lock-file" :recipe
       (:package "cape" :repo "minad/cape" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                  "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                            "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :source "elpaca-menu-lock-file" :protocol https :inherit t :depth
                 treeless :host github :ref "f8682a046a57525754ebc812ba3ae9c973db083b"))
 (clojure-ts-mode :source "elpaca-menu-lock-file" :recipe
                  (:package "clojure-ts-mode" :repo "clojure-emacs/clojure-ts-mode"
                            :fetcher github :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                             "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                             "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                             "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                       "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                            :source "elpaca-menu-lock-file" :protocol https :inherit t
                            :depth treeless :host github :ref
                            "96fdffcbe9e1b8ebf9ad14e23b06f62cc3422e22"))
 (cond-let
   :source "elpaca-menu-lock-file" :recipe
   (:package "cond-let" :fetcher github :repo "tarsius/cond-let" :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
              "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
              "docs/*.info" "docs/*.texi" "docs/*.texinfo"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                        "LICENSE" "README*" "*-pkg.el"))
             :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
             :ref "0430bd1eb3493ea90d69feb6b7eb7dac3e10d0ba"))
 (consult :source "elpaca-menu-lock-file" :recipe
          (:package "consult" :repo "minad/consult" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                     "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :protocol https :inherit t :depth
                    treeless :host github :ref
                    "1de751087e348b24c05e0b1dca16ced752e8f505"))
 (corfu :source "elpaca-menu-lock-file" :recipe
        (:package "corfu" :repo "minad/corfu" :files
                  ("*" "extensions/*.el" (:exclude ".git")) :fetcher github :source
                  "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
                  :host github :ref "5d7c697613ed2399c71e34e9f1cbd1ce8f8324cc"))
 (csv-mode :source "elpaca-menu-lock-file" :recipe
           (:package "csv-mode" :repo "https://github.com/emacsmirror/csv-mode.git"
                     :branch "master" :files ("*" (:exclude ".git")) :source
                     "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
                     :type git :ref "ba5dc934b9dbdc2b57ab1917a669cdfd7d1838d3"))
 (dash :source "elpaca-menu-lock-file" :recipe
       (:package "dash" :fetcher github :repo "magnars/dash.el" :files
                 ("dash.el" "dash.texi") :source "elpaca-menu-lock-file" :protocol https
                 :inherit t :depth treeless :ref
                 "fb443e7a6e660ba849cafcd01021d9aac3ac6764"))
 (doom-themes :source "elpaca-menu-lock-file" :recipe
              (:package "doom-themes" :fetcher github :repo "doomemacs/themes" :files
                        (:defaults "themes/*.el" "themes/*/*.el" "extensions/*.el")
                        :source "elpaca-menu-lock-file" :protocol https :inherit t
                        :depth treeless :host github :ref
                        "ad9b1bd1c21e25f044a4d2c3db41734666b00d16"))
 (eglot :source "elpaca-menu-lock-file" :recipe
        (:source nil :host github :repo "joaotavora/eglot" :inherit nil :package "eglot"
                 :ref "ad7e201bb6d892895b07048f809fe336f6935109"))
 (eglot-tempel :source "elpaca-menu-lock-file" :recipe
               (:package "eglot-tempel" :fetcher github :repo "fejfighter/eglot-tempel"
                         :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                          "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                    "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                         :source "elpaca-menu-lock-file" :protocol https :inherit t
                         :depth treeless :host github :ref
                         "c6c9a18eba61f6bae7167fa62bab9b637592d20d"))
 (eldoc-mouse :source "elpaca-menu-lock-file" :recipe
              (:package "eldoc-mouse" :fetcher github :repo "huangfeiyu/eldoc-mouse"
                        :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                         "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                   "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :protocol https :inherit t
                        :depth treeless :host github :ref
                        "de48a1af46617861d38d9f96984869c21c0d887e"))
 (elisp-autofmt :source "elpaca-menu-lock-file" :recipe
                (:package "elisp-autofmt" :fetcher codeberg :repo
                          "ideasman42/emacs-elisp-autofmt" :files
                          (:defaults "elisp-autofmt.py" "elisp-autofmt.overrides.json")
                          :source "elpaca-menu-lock-file" :protocol https :inherit t
                          :depth treeless :host codeberg :ref
                          "7c092f61dbf9673c9947c7048255c95b9d929204"))
 (elisp-refs :source "elpaca-menu-lock-file" :recipe
             (:package "elisp-refs" :repo "Wilfred/elisp-refs" :fetcher github :files
                       (:defaults (:exclude "elisp-refs-bench.el")) :source
                       "elpaca-menu-lock-file" :protocol https :inherit t :depth
                       treeless :ref "541a064c3ce27867872cf708354a65d83baf2a6d"))
 (elpaca :source
   "elpaca-menu-lock-file" :recipe
   (:source nil :protocol https :inherit ignore :depth 1 :repo
            "https://github.com/progfolio/elpaca.git" :ref
            "1508298c1ed19c81fa4ebc5d22d945322e9e4c52" :files
            (:defaults "elpaca-test.el" (:exclude "extensions")) :build
            (:not elpaca--activate-package) :package "elpaca"))
 (elpaca-use-package :source "elpaca-menu-lock-file" :recipe
                     (:package "elpaca-use-package" :wait t :repo
                               "https://github.com/progfolio/elpaca.git" :files
                               ("extensions/elpaca-use-package.el") :main
                               "extensions/elpaca-use-package.el" :build
                               (:not elpaca--compile-info) :source
                               "elpaca-menu-lock-file" :protocol https :inherit t :depth
                               treeless :ref "1508298c1ed19c81fa4ebc5d22d945322e9e4c52"))
 (envrc :source "elpaca-menu-lock-file" :recipe
        (:package "envrc" :fetcher github :repo "purcell/envrc" :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                   "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                             "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                  :source "elpaca-menu-lock-file" :protocol https :inherit t :depth
                  treeless :host github :ref "06d72d141ac2e2990d80cdb8bb84f6cb54c628a5"))
 (evil :source "elpaca-menu-lock-file" :recipe
       (:package "evil" :repo "emacs-evil/evil" :fetcher github :files
                 (:defaults "doc/build/texinfo/evil.texi"
                            (:exclude "evil-test-helpers.el"))
                 :source "elpaca-menu-lock-file" :protocol https :inherit t :depth
                 treeless :host github :ref "729d9a58b387704011a115c9200614e32da3cefc"))
 (evil-nerd-commenter :source "elpaca-menu-lock-file" :recipe
                      (:package "evil-nerd-commenter" :fetcher github :repo
                                "redguardtoo/evil-nerd-commenter" :files
                                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                                 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                                 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                                 "docs/*.texinfo"
                                 (:exclude ".dir-locals.el" "test.el" "tests.el"
                                           "*-test.el" "*-tests.el" "LICENSE" "README*"
                                           "*-pkg.el"))
                                :source "elpaca-menu-lock-file" :protocol https :inherit
                                t :depth treeless :host github :ref
                                "ae52c5070a48793e2c24474c9c8dbf20175d18a0"))
 (evil-surround :source "elpaca-menu-lock-file" :recipe
                (:package "evil-surround" :repo "emacs-evil/evil-surround" :fetcher
                          github :old-names (surround) :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                           "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                           "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :protocol https :inherit t
                          :depth treeless :host github :ref
                          "da05c60b0621cf33161bb4335153f75ff5c29d91"))
 (expand-region :source "elpaca-menu-lock-file" :recipe
                (:package "expand-region" :repo "magnars/expand-region.el" :fetcher
                          github :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                           "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                           "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :protocol https :inherit t
                          :depth treeless :host github :ref
                          "351279272330cae6cecea941b0033a8dd8bcc4e8"))
 (f :source "elpaca-menu-lock-file" :recipe
    (:package "f" :fetcher github :repo "rejeep/f.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
               "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
               "docs/*.info" "docs/*.texi" "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                         "LICENSE" "README*" "*-pkg.el"))
              :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
              :ref "931b6d0667fe03e7bf1c6c282d6d8d7006143c52"))
 (flymake-childframe :source "elpaca-menu-lock-file" :recipe
                     (:source "elpaca-menu-lock-file" :protocol https :inherit t :depth
                              treeless :host github :repo "junyi-hou/flymake-childframe"
                              :package "flymake-childframe" :ref
                              "8f46751a25a130be7138ce7d6f4c53d7d5cf9989"))
 (gcmh :source "elpaca-menu-lock-file" :recipe
       (:package "gcmh" :repo "emacsmirror/gcmh" :fetcher gitlab :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                  "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                            "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :source "elpaca-menu-lock-file" :protocol https :inherit t :depth
                 treeless :host github :wait t :ref
                 "0089f9c3a6d4e9a310d0791cf6fa8f35642ecfd9"))
 (goto-chg :source "elpaca-menu-lock-file" :recipe
           (:package "goto-chg" :repo "emacs-evil/goto-chg" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                      "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :protocol https :inherit t :depth
                     treeless :ref "72f556524b88e9d30dc7fc5b0dc32078c166fda7"))
 (helpful :source "elpaca-menu-lock-file" :recipe
          (:package "helpful" :repo "Wilfred/helpful" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                     "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :protocol https :inherit t :depth
                    treeless :host github :ref
                    "03756fa6ad4dcca5e0920622b1ee3f70abfc4e39"))
 (hl-todo :source "elpaca-menu-lock-file" :recipe
          (:package "hl-todo" :repo "tarsius/hl-todo" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                     "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :protocol https :inherit t :depth
                    treeless :host github :ref
                    "9540fc414014822dde00f0188b74e17ac99e916d"))
 (indent-bars :source "elpaca-menu-lock-file" :recipe
              (:package "indent-bars" :repo "jdtsmith/indent-bars" :files
                        ("*" (:exclude ".git" "LICENSE")) :source
                        "elpaca-menu-lock-file" :protocol https :inherit t :depth
                        treeless :host github :ref
                        "d32cdba5b136c9697fbfd1d231d31a33c5dabf0e"))
 (inheritenv :source "elpaca-menu-lock-file" :recipe
             (:package "inheritenv" :fetcher github :repo "purcell/inheritenv" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                        "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                        "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                  "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :protocol https :inherit t :depth
                       treeless :ref "b9e67cc20c069539698a9ac54d0e6cc11e616c6f"))
 (jarchive :source "elpaca-menu-lock-file" :recipe
           (:package "jarchive" :repo "https://git.sr.ht/~dannyfreeman/jarchive" :files
                     ("*" (:exclude ".git" "LICENSE")) :source "elpaca-menu-lock-file"
                     :protocol https :inherit t :depth treeless :type git :ref
                     "9b5edcb9ac4937ad41bc830714534fec5ebe48ee"))
 (jupyter :source "elpaca-menu-lock-file" :recipe
          (:package "jupyter" :fetcher github :repo "nnicandro/emacs-jupyter" :files
                    (:defaults "Makefile" "widget.html" "js") :source
                    "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
                    :host github :ref "de89cbeca890db51ba84aee956658f89aaa0b642"))
 (llama :source "elpaca-menu-lock-file" :recipe
        (:package "llama" :fetcher github :repo "tarsius/llama" :files
                  ("llama.el" ".dir-locals.el") :source "elpaca-menu-lock-file"
                  :protocol https :inherit t :depth treeless :ref
                  "2a89ba755b0459914a44b1ffa793e57f759a5b85"))
 (lolipop :source "elpaca-menu-lock-file" :recipe
          (:source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
                   :host github :repo "RadioNoiseE/lolipop" :pre-build
                   (("make clean") ("make")) :files
                   ("lolipop-mode.el" "lolipop-core.dylib") :package "lolipop" :ref
                   "d8653cd5f63798b0869f6bee36a8a05a53277ed7"))
 (magit :source "elpaca-menu-lock-file" :recipe
        (:package "magit" :fetcher github :repo "magit/magit" :files
                  ("lisp/magit*.el" "lisp/git-*.el" "docs/magit.texi" "docs/AUTHORS.md"
                   "LICENSE" ".dir-locals.el" ("git-hooks" "git-hooks/*")
                   (:exclude "lisp/magit-section.el"))
                  :source "elpaca-menu-lock-file" :protocol https :inherit t :depth
                  treeless :host github :ref "9cbdaf5ddfe825c04ffa306dde6bdfea9ab6dff3"))
 (magit-section :source "elpaca-menu-lock-file" :recipe
                (:package "magit-section" :fetcher github :repo "magit/magit" :files
                          ("lisp/magit-section.el" "docs/magit-section.texi"
                           "magit-section-pkg.el")
                          :source "elpaca-menu-lock-file" :protocol https :inherit t
                          :depth treeless :ref
                          "9cbdaf5ddfe825c04ffa306dde6bdfea9ab6dff3"))
 (markdown-mode :source "elpaca-menu-lock-file" :recipe
                (:package "markdown-mode" :fetcher github :repo "jrblevin/markdown-mode"
                          :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                           "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                           "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :protocol https :inherit t
                          :depth treeless :host github :ref
                          "92802fae9ebbc8c2e4c281c06dcdbd74b8bca80e"))
 (markdown-ts-mode :source "elpaca-menu-lock-file" :recipe
                   (:package "markdown-ts-mode" :repo "LionyxML/markdown-ts-mode"
                             :fetcher github :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                              "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                              "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                              "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el" "tests.el"
                                        "*-test.el" "*-tests.el" "LICENSE" "README*"
                                        "*-pkg.el"))
                             :source "elpaca-menu-lock-file" :protocol https :inherit t
                             :depth treeless :host github :ref
                             "2f1ee8b94cdf53cebc31ae08ecfbba846193d5e1"))
 (msgpack :source "elpaca-menu-lock-file" :recipe
          (:package "msgpack" :fetcher github :repo "xuchunyang/msgpack.el" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                     "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                               "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :protocol https :inherit t :depth
                    treeless :host github :ref
                    "90e3086f259549b1667a3c5b9aa2d70aaeaa4d3d"))
 (no-littering :source "elpaca-menu-lock-file" :recipe
               (:package "no-littering" :fetcher github :repo
                         "emacscollective/no-littering" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                          "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                    "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                         :source "elpaca-menu-lock-file" :protocol https :inherit t
                         :depth treeless :host github :wait t :ref
                         "303999eb940e58bb96fe0424ef393fe3b24e8f16"))
 (orderless :source "elpaca-menu-lock-file" :recipe
            (:package "orderless" :repo "oantolin/orderless" :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                       "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                 "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :protocol https :inherit t :depth
                      treeless :host github :ref
                      "aba87f2576e1d61e8f60ce8acdf6aca5c2da370c"))
 (page-break-lines :source "elpaca-menu-lock-file" :recipe
                   (:package "page-break-lines" :fetcher github :repo
                             "purcell/page-break-lines" :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                              "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                              "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                              "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el" "tests.el"
                                        "*-test.el" "*-tests.el" "LICENSE" "README*"
                                        "*-pkg.el"))
                             :source "elpaca-menu-lock-file" :protocol https :inherit t
                             :depth treeless :host github :ref
                             "f54aa2b96f6ed249e103346cdb872c97c3c98054"))
 (posframe :source "elpaca-menu-lock-file" :recipe
           (:package "posframe" :fetcher github :repo "tumashu/posframe" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                      "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :protocol https :inherit t :depth
                     treeless :ref "d93828bf6c36383c365bd564ad3bab5a4403804c"))
 (s :source "elpaca-menu-lock-file" :recipe
    (:package "s" :fetcher github :repo "magnars/s.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
               "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
               "docs/*.info" "docs/*.texi" "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                         "LICENSE" "README*" "*-pkg.el"))
              :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
              :ref "dda84d38fffdaf0c9b12837b504b402af910d01d"))
 (shell-maker :source "elpaca-menu-lock-file" :recipe
              (:package "shell-maker" :fetcher github :repo "xenodium/shell-maker"
                        :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                         "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                   "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :protocol https :inherit t
                        :depth treeless :ref "a7ff78f8cd29fba9a694b8d7bbee448c7a51472d"))
 (simple-httpd :source "elpaca-menu-lock-file" :recipe
               (:package "simple-httpd" :repo "skeeto/emacs-web-server" :fetcher github
                         :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                          "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                    "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                         :source "elpaca-menu-lock-file" :protocol https :inherit t
                         :depth treeless :ref "3982c55e9061475038a3ccd61aecb2de3d407cec"))
 (tempel :source "elpaca-menu-lock-file" :recipe
         (:package "tempel" :repo "minad/tempel" :fetcher github :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                    "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                              "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :source "elpaca-menu-lock-file" :protocol https :inherit t :depth
                   treeless :host github :ref "abc41419e9eff719ccc8a156defb0e6db3b7f460"))
 (tramp-rpc :source "elpaca-menu-lock-file" :recipe
            (:source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
                     :host github :repo "ArthurHeymans/emacs-tramp-rpc" :package
                     "tramp-rpc" :ref "9c4380bf860cc2224781d5faae2064949ca9ff3d"))
 (transient :source "elpaca-menu-lock-file" :recipe
            (:package "transient" :fetcher github :repo "magit/transient" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                       "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                 "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :protocol https :inherit t :depth
                      treeless :host github :ref
                      "06d5e8be1b3f1c9b4f184909edfa857cfbcab3c0"))
 (typst-ts-mode :source "elpaca-menu-lock-file" :recipe
                (:package "typst-ts-mode" :repo "meow_king/typst-ts-mode" :files
                          ("*" (:exclude ".git")) :source "elpaca-menu-lock-file"
                          :protocol https :inherit t :depth treeless :host sourcehut
                          :ref "1367003e2ad55a2f6f9e43178584683028ab56e9"))
 (vertico :source "elpaca-menu-lock-file" :recipe
          (:package "vertico" :repo "minad/vertico" :files
                    (:defaults "extensions/vertico-*.el") :fetcher github :source
                    "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
                    :host github :ref "709597a39f337b8448c50c8824e34b8ce98cfaca"))
 (vterm :source "elpaca-menu-lock-file" :recipe
        (:package "vterm" :fetcher github :repo "akermu/emacs-libvterm" :files
                  ("CMakeLists.txt" "elisp.c" "elisp.h" "emacs-module.h" "etc" "utf8.c"
                   "utf8.h" "vterm.el" "vterm-module.c" "vterm-module.h")
                  :source "elpaca-menu-lock-file" :protocol https :inherit t :depth
                  treeless :host github :ref "a01a2894a1c1e81a39527835a9169e35b7ec5dec"))
 (websocket :source "elpaca-menu-lock-file" :recipe
            (:package "websocket" :repo "ahyatt/emacs-websocket" :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                       "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                 "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :protocol https :inherit t :depth
                      treeless :ref "40c208eaab99999d7c1e4bea883648da24c03be3"))
 (with-editor :source "elpaca-menu-lock-file"
   :recipe
   (:package "with-editor" :fetcher github :repo "magit/with-editor" :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
              "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
              "docs/*.info" "docs/*.texi" "docs/*.texinfo"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                        "LICENSE" "README*" "*-pkg.el"))
             :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
             :ref "902b4d572af2c2f36060da01e3c33d194cdec32b"))
 (ws-butler :source "elpaca-menu-lock-file" :recipe
            (:package "ws-butler" :fetcher git :url
                      "https://git.savannah.gnu.org/git/emacs/nongnu.git" :branch
                      "master" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                       "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                 "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :protocol https :inherit t :depth
                      treeless :host github :repo "lewang/ws-butler" :ref
                      "67c49cfdf5a5a9f28792c500c8eb0017cfe74a3a"))
 (zmq :source "elpaca-menu-lock-file" :recipe
      (:package "zmq" :fetcher github :repo "nnicandro/emacs-zmq" :files
                (:defaults "Makefile" "src") :source "elpaca-menu-lock-file" :protocol
                https :inherit t :depth treeless :ref
                "fe856c43286674aa6770d95a81d915363f5df399")))
