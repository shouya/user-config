UNAME := $(shell uname)
HOSTNAME := $(shell hostname)

help:
	@grep -E '^[a-zA-Z0-9_-]+:.*?# .*$$' $(MAKEFILE_LIST) \
	| sort \
	| awk 'BEGIN {FS = ":.*?# "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

install-home-manager:
	@which home-manager || echo Please refer to https://nix-community.github.io/home-manager/index.xhtml for installation instruction

home-manager: install-home-manager # Home Manager
	ln -fnrs . ~/.config/home-manager
# home manager can't clone emacs config for us, doing it manually
	[ -e emacs ] || git clone --recursive git@git.lain.li:shouya/emacs.d.git emacs
	[ -e ~/.password-store ] || git clone git@git.lain.li:shouya/pass.git ~/.password-store
	@echo Symlinked to home-manager, feel free to run 'home-manager switch'

unlock: # Unlock with git-crypt
	@git crypt unlock || echo "git-crypt not found, skipping"

emacs: # Emacs config (requires git-crypt)
	git clone --recursive git@git.lain.li:shouya/emacs.d.git emacs
	cd emacs
	mkdir -p ~/.emacs.d
	ln -rsf emacs/* ~/.emacs.d

cachix: # Cache for nix
	@which nix-shell || echo Please install nix first.
	nix-shell -p cachix --run "cachix use nix-community"
	nix-shell -p cachix --run "cachix use spray8696"

pass: # Password store
	git clone git@git.lain.li:shouya/pass.git ~/.password-store || true
	cd ~/.password-store; git pull --rebase

.PHONY : $(MAKECMDGOALS)
