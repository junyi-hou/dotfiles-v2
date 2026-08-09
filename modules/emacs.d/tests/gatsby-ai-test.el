;;; gatsby-ai-test.el --- tests for gatsby-ai.el  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require 'gatsby-ai)

(ert-deftest gatsby>>agent-shell-new-worktree--commands--subproject ()
  "Commands for a project subdirectory use sparse checkout for that directory."
  (let ((cmds (gatsby>>agent-shell-new-worktree--commands
               "/repo" "modules/emacs.d" "/repo/.agent-shell/worktrees/foo")))
    (should (= (length cmds) 3))
    (let ((add (nth 0 cmds))
          (sparse (nth 1 cmds))
          (checkout (nth 2 cmds)))
      (should (eq (plist-get add :kind) 'add))
      (should (equal (plist-get add :cwd) "/repo"))
      (should (equal (plist-get add :args)
                     '("git" "worktree" "add" "--no-checkout"
                       "/repo/.agent-shell/worktrees/foo")))
      (should (eq (plist-get sparse :kind) 'sparse))
      (should (equal (plist-get sparse :cwd) "/repo/.agent-shell/worktrees/foo"))
      (should (equal (plist-get sparse :args)
                     '("git" "sparse-checkout" "set" "modules/emacs.d")))
      (should (eq (plist-get checkout :kind) 'checkout))
      (should (equal (plist-get checkout :cwd) "/repo/.agent-shell/worktrees/foo"))
      (should (equal (plist-get checkout :args)
                     '("git" "checkout" "HEAD"))))))

(ert-deftest gatsby>>agent-shell-new-worktree--commands--repo-root ()
  "Commands for the repository root use `.' as the sparse checkout path."
  (let ((cmds (gatsby>>agent-shell-new-worktree--commands
               "/repo" "." "/repo/.agent-shell/worktrees/bar")))
    (should (= (length cmds) 3))
    (should (equal (plist-get (nth 1 cmds) :args)
                   '("git" "sparse-checkout" "set" ".")))))

(provide 'gatsby-ai-test)
;;; gatsby-ai-test.el ends here
