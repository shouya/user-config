default:
	@echo Please run "make <repo>"

# Cheatsheet:
#
# ln -f :: force
#    -r :: relative
#    -s :: symbolic
#    -T :: otherwise ln may create a symlink inside the target as folder

base: git ssh

git:
	ln -frs base/gitconfig.private ~/.gitconfig.private
	ln -frs base/gitconfig ~/.gitconfig
	ln -frs base/gitignore ~/.gitignore

ssh:
	mkdir -p ~/.ssh
	chmod 700 ~/.ssh
	ln -frs base/ssh_config.private ~/.ssh/config

emacs:
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
	mv -f ~/.config/systemd{,.bak}
	ln -rsf systemd ~/.config/systemd
	echo "Please run [systemctl --user daemon-reload]"
	echo "Also [systemctl enable <unit>]"

.PHONY : $(MAKECMDGOALS)
