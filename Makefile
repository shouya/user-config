UNAME := $(shell uname)

help:
	@grep -E '^[a-zA-Z_-]+:.*?# .*$$' $(MAKEFILE_LIST) \
	| sort \
	| awk 'BEGIN {FS = ":.*?# "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

base: git ssh

git: # Git config (requires git-crypt)
	git crypt unlock
ifndef NO_GIT_CRYPT
	ln -frs base/gitconfig.private ~/.gitconfig.private
endif
	ln -frs base/gitconfig ~/.gitconfig
	ln -frs base/gitignore ~/.gitignore

ssh: # SSH config (requires git-crypt)
ifndef NO_GIT_CRYPT
	git crypt unlock
	mkdir -p ~/.ssh
	chmod 700 ~/.ssh
	ln -frs base/ssh_config.private ~/.ssh/config
	mkdir -p ~/.ssh/config.d
ifeq ($(UNAME),Darwin)
	ln -rs base/ssh_config_macos.private ~/.ssh/config.d/macos.conf
endif
ifeq ($(UNAME),Linux)
	ln -rs base/ssh_config_linux.private ~/.ssh/config.d/linux.conf
endif
endif

vim: # Vim RC
	ln -frs base/vimrc ~/.vimrc

neovim: # Neovim
	./utils/backup ~/.config/nvim
	ln -rsf xdg/nvim ~/.config

emacs: # Emacs config (requires git-crypt)
	git clone --recursive git@github.com:shouya/emacs.d.git emacs
	cd emacs && git crypt unlock
	mkdir -p ~/.emacs.d
	ln -rsf emacs/* ~/.emacs.d

shell: # Zsh config (requires git-crypt)
	./utils/backup ~/.config/starship.toml
	ln -rsf xdg/starship.toml ~/.config

	git clone --recursive git@git.lain.li:shouya/dot-shell.git shell
	cd shell && git crypt unlock
	ln -Trsf shell ~/.shell
	ln -rsf ~/.shell/zshrc ~/.zshrc


calendar: # Khal and vdirsyncer
	./utils/backup ~/.config/{khal,vdirsyncer}
	ln -rsf xdg/{khal,vdirsyncer} ~/.config

sway: # Sway, waybar, kanshi, rofi
	./utils/backup ~/.config/{rofi,kanshi,sway,waybar}
	ln -rsf xdg/{rofi,kanshi,sway,waybar} ~/.config

fcitx5: # Fcitx 5
	./utils/backup ~/.config/fcitx5
	ln -rsf xdg/fcitx5 ~/.config

systemd-laptop: # Systemd for graphical laptop, including Wayland related daemon
	./utils/backup ~/.config/systemd
	ln -Trsf xdg/systemd-laptop ~/.config/systemd
	ln -Trsf user-tmpfiles.d ~/.config/user-tmpfiles.d
	@echo "Please run [systemctl --user daemon-reload]"
	@echo "Also [systemctl enable <unit>]"

gpg: # GnuPG agent config
	mkdir -p ~/.gnupg
	chmod 700 ~/.gnupg
	ln -rsf gnupg/gpg-agent.conf ~/.gnupg/
	killall -9 gpg-agent

tmux: # Tmux
	./utils/backup ~/.tmux.conf
	ln -rsf tmux/tmux.conf ~/.tmux.conf

rclone: # Rclone config (requires git-crypt)
	git crypt unlock
	./utils/backup ~/.config/rclone
	ln -rsf xdg/rclone ~/.config

pass: # Password store
	git clone git@git.lain.li:shouya/pass.git ~/.password-store || true
	cd ~/.password-store; git pull --rebase


.PHONY : $(MAKECMDGOALS)
