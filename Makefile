default:
	@echo Please run "make <repo>"

# Cheatsheet:
#
# ln -f :: force
#    -r :: relative
#    -s :: symbolic

base: git ssh

git:
	ln -frs base/gitconfig.private ~/.gitconfig.private
	ln -frs base/gitconfig ~/.gitconfig
	ln -frs base/gitignore ~/.gitignore

ssh:
	mkdir -p ~/.ssh
	chmod 700 ~/.ssh
	ln -frs base/ssh_config.private ~/.ssh/config

.PHONY : base default
