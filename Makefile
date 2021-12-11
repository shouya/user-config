default:
	@echo Please ensure all goals are idempotent


base:
	# gitconfig
	ln -frs base/gitconfig.private ~/.gitconfig.private
	ln -frs base/gitconfig ~/.gitconfig


.PHONY : base default
