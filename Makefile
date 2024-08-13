UNAME := $(shell uname)
HOSTNAME := $(shell hostname)

help:
	@grep -E '^[a-zA-Z0-9_-]+:.*?# .*$$' $(MAKEFILE_LIST) \
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
	git clone --recursive git@git.lain.li:shouya/emacs.d.git emacs
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
	./utils/backup ~/.config/khal
	./utils/backup ~/.config/vdirsyncer
	ln -rsf xdg/khal ~/.config
	ln -rsf xdg/vdirsyncer ~/.config

sway: # Sway, waybar, kanshi, rofi
	./utils/backup ~/.config/kanshi
	./utils/backup ~/.config/sway
	./utils/backup ~/.config/waybar
	ln -rsf xdg/kanshi ~/.config
	ln -rsf xdg/sway ~/.config
	ln -rsf xdg/waybar ~/.config

rofi: # rofi
	./utils/backup ~/.config/rofi
	ln -rsf xdg/rofi ~/.config


i3: # i3
	./utils/backup ~/.config/i3
	ln -rsf xdg/i3 ~/.config

xmonad: # XMonad
	./utils/backup ~/.xmonad
	ln -rsf xdg/xmonad ~/.xmonad

polybar: # Polybar
	./utils/backup ~/.config/polybar
	ln -rsf xdg/polybar ~/.config/polybar

dunst: # Dunst
	./utils/backup ~/.config/dunst
	ln -rsf xdg/dunst ~/.config/dunst

fcitx5: # Fcitx 5
	./utils/backup ~/.config/fcitx5
	ln -rsf xdg/fcitx5 ~/.config

alacritty: # Alacritty
	./utils/backup ~/.config/alacritty
	ln -rsf xdg/alacritty ~/.config

hime: # Hime input method
	./utils/backup ~/.config/hime
	ln -rsf xdg/hime ~/.config

eww: # Eww status bar and widgets
	./utils/backup ~/.config/eww
	ln -rsf xdg/eww ~/.config

stalonetray: # Stalonetray
	./utils/backup ~/.stalonetrayrc
	ln -rsf xdg/stalonetrayrc ~/.stalonetrayrc

fish: # Fish shell
	./utils/backup ~/.config/fish
	ln -rsf xdg/fish ~/.config/fish

systemd-laptop: # Systemd for graphical laptop, including Wayland related daemon
	./utils/backup ~/.config/systemd
	ln -Trsf xdg/systemd-laptop ~/.config/systemd
	ln -Trsf user-tmpfiles.d ~/.config/user-tmpfiles.d
	@echo "Please run [systemctl --user daemon-reload]"
	@echo "Also [systemctl enable <unit>]"

systemd-tubian: # Systemd for graphical laptop running debian/gnome-flashback
	./utils/backup ~/.config/systemd
	ln -Trsf xdg/systemd-tubian ~/.config/systemd
	@echo "Please run [systemctl --user daemon-reload]"
	@echo "Also [systemctl enable <unit>]"

x11: # User x11 config (deps on fcitx and xmonad)
ifneq (,$(wildcard x11/Xresources.$(HOSTNAME)))
	ln -rsf x11/Xresources.$(HOSTNAME) ~/.Xresources
else
	@echo "Xresources not found for $(HOSTNAME), skipping"
endif
	ln -rsf x11/xsession ~/.xsession
	ln -rsf x11/xsession ~/.xinitrc
	sudo install -m 644 x11/Xsession.desktop /usr/share/xsessions/Xsession.desktop

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

offlineimap: # Software to sync email via imap
	ln -rsf mail/offlineimaprc ~/.offlineimaprc
	ln -rsf mail/offlineimap.py ~/.offlineimap.py
	@echo You may want to install mu/mu4e.

picom: # Picom, an X compositor
	./utils/backup ~/.config/picom
	ln -rsf xdg/picom ~/.config

hypr: # Hyprland and other Hypr ecosystem utilities
	./utils/backup ~/.config/hypr
	ln -rsf xdg/hypr ~/.config

.PHONY : $(MAKECMDGOALS)
