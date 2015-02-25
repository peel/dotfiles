FILES = ideavimrc zshrc.after tmux.conf.user vimrc.after

HOME := ~/wrk/dotfiles

default: update

setup: download update link source

download:
	sh -c "`curl -fsSL https://raw.githubusercontent.com/skwp/dotfiles/master/install.sh`"
	sh $HOME/Brewfile

update:
	cd ~/.yadr
	git pull --rebase
	rake update

link:
	@for f in $(FILES) ; do ln -fvs $(HOME)/$$f ~/.$$f; done

unlink:
	@for f in $(LIST) ; do rm ~/.$$f; done

source:
	@for f in $(FILES) ; do source ~/.$$f; done
