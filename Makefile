help:
	@grep -E '^[a-zA-Z_-]+:.*?# .*$$' $(MAKEFILE_LIST) \
	| sort \
	| awk 'BEGIN {FS = ":.*?# "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

base: git ssh

git: # Git config
	git crypt unlock
ifndef NO_GIT_CRYPT
	ln -frs base/gitconfig.private ~/.gitconfig.private
endif
	ln -frs base/gitconfig ~/.gitconfig
	ln -frs base/gitignore ~/.gitignore

ssh: # SSH config, requires git config
ifndef NO_GIT_CRYPT
	git crypt unlock
	mkdir -p ~/.ssh
	chmod 700 ~/.ssh
	ln -frs base/ssh_config.private ~/.ssh/config
endif

vim: # Vim RC
	ln -frs base/vimrc ~/.vimrc

emacs: # Emacs, requires submodule and git crypt
	git submodule update --init --recursive emacs
	cd emacs && git crypt unlock
	mkdir -p ~/.emacs.d
	ln -rsf emacs/* ~/.emacs.d

shell:
	git submodule update --init --recursive shell
	cd shell && git crypt unlock
	ln -Trsf shell ~/.shell
	ln -rsf ~/.shell/zshrc ~/.zshrc

linux-gui:
	PREFIX=~/.config ./utils/backup tools/linux-gui/*
	ln -rsf linux-gui/xdg_config/* ~/.config

tools:
	PREFIX=~/.config ./utils/backup tools/xdg_config/*
	ln -rsf tools/xdg_config/* ~/.config
	mkdir -p ~/.calendar

systemd:
	./utils/backup ~/.config/systemd/user
	./utils/backup ~/.config/systemd/user-tmpfiles.d

	mkdir -p ~/.config/systemd
	ln -rsf systemd/user ~/.config/systemd/user

	ln -rsf systemd/user-tmpfiles.d ~/.config/user-tmpfiles.d

	@echo "Please run [systemctl --user daemon-reload]"
	@echo "Also [systemctl enable <unit>]"

.PHONY : $(MAKECMDGOALS)
