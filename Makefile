FILES = ideavimrc tmux.conf.user vimrc.after spacemacs
ZSH_FILES = my.zsh
REPO := "~/wrk/dotfiles"
default: update

install: download update link source

download:
	sh -c "`curl -fsSL https://raw.githubusercontent.com/skwp/dotfiles/master/install.sh`"
	sudo sh $HOME/Brewfile

update:
	cd ~/.yadr
	git pull --rebase
	rake update

link:
	@for f in $(FILES) ; do ln -fvs $(REPO)/$$f ~/.$$f; done
	@for f in $(ZSH_LIST) ; do ln -fvs $(REPO)/$$f ~/.zsh.after/$$f; done

unlink:
	@for f in $(LIST) ; do rm -f ~/.$$f; done
	@for f in $(ZSH_LIST) ; do rm -f ~/.zsh.after/$$f; done

source:
	@for f in $(FILES) ; do source ~/.$$f; done
	@for f in $(ZSH_FILES) ; do source ~/.zsh.after/$$f; done
