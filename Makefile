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
endif

vim: # Vim RC
	ln -frs base/vimrc ~/.vimrc

neovim: # Neovim
	./utils/backup ~/.config/nvim
	ln -rsf xdg/nvim ~/.config

emacs: # Emacs, requires submodule and git-crypt
	git submodule update --init --recursive emacs
	cd emacs && git crypt unlock
	mkdir -p ~/.emacs.d
	ln -rsf emacs/* ~/.emacs.d

shell: # Emacs, requires submodule and git-crypt
	git submodule update --init --recursive shell
	cd shell && git crypt unlock
	ln -Trsf shell ~/.shell
	ln -rsf ~/.shell/zshrc ~/.zshrc

calendar: # Khal and vdirsyncer
	./utils/backup ~/.config/{khal,vdirsyncer}
	ln -rsf xdg/{khal,vdirsyncer} ~/.config

sway: # Sway, waybar, kanshi, rofi
	./utils/backup ~/.config/{rofi,kanshi,sway,waybar}
	ln -rsf xdg/{rofi,kanshi,sway,waybar} ~/.config

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

.PHONY : $(MAKECMDGOALS)
