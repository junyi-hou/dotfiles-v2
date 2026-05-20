;;; config.el --- Example config for scripts/run-agent  -*- lexical-binding: t; -*-
;;
;; ;; Copy to ~/.config/agent-shell-to-go/config.el and fill in your values.
;; ;; Environment variables (SLACK_BOT_TOKEN, etc.) can be used to override anything set here.

(load (expand-file-name "user-lisp/sops-secret-management/sops-secret-management.el" user-emacs-directory))

;;; Discord

(setq agent-shell-to-go-discord-guild-id (sops-retrieve-secret "env/DISCORD_GUILD_ID")
      agent-shell-to-go-discord-bot-token (sops-retrieve-secret "env/DISCORD_BOT_TOKEN")
      agent-shell-to-go-discord-authorized-users
      (list (sops-retrieve-secret "env/DISCORD_USER_ID"))
      agent-shell-to-go-projects-directory "~/Projects/"
      agent-shell-to-go-default-transport 'discord)

;;; agent-shell

;; Authentication: agent-shell runs the underlying agent (claude-code, gemini-cli,
;; etc.) as a subprocess.  Any credentials the agent needs must be available in the
;; environment before run-agent starts.  For Claude Code that means ANTHROPIC_API_KEY;
;; for Gemini CLI it means GOOGLE_CLOUD_PROJECT / gcloud ADC; for codex it means
;; OPENAI_API_KEY.  Export them in your shell profile, pass them via
;; agent-shell-command-prefix ("env" "KEY=val" ...), or load them from your secrets
;; manager before this config is evaluated.
(setq agent-shell-to-go-show-tool-output nil)
;; Which agent to use — skips the selection prompt on startup.
;; Common values: claude-code, gemini-cli, goose, opencode, codex
(setq agent-shell-preferred-agent-config 'claude-code)

;; Session strategy when the agent starts.
;; Use 'new to always start fresh; 'latest to resume the last session.
(setq agent-shell-session-strategy 'new)

;; Prefix every agent command with these args — useful for Docker/devcontainer.
;; (setq agent-shell-command-prefix '("docker" "exec" "-i" "my-container" "--"))
