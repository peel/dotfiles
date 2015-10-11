FILES = ideavimrc tmux.conf.user vimrc.after spacemacs
ZSH_FILES = my.zsh
REPO := "~/wrk/dotfiles"
IDEA_V := 14
IDEA_DIRS = colors fileTemplates inspection keymaps options quicklists templates
default: update

install: download config update link source

download:
		sh -c "`curl -fsSL https://raw.githubusercontent.com/skwp/dotfiles/master/install.sh`"
		sudo sh $HOME/Brewfile
		git clone --recursive http://github.com/syl20bnr/spacemacs ~/.emacs.d

update:
		cd ~/.emacs.d
		git pull -r -u
		git submodule sync; git submodule update
		cd ~/.yadr
		git pull -r -u
		rake update
		unzip -o $(REPO)/settings.jar -d $(REPO)/idea

config:
		defaults write com.apple.PowerChime ChimeOnAllHardware -bool true; open /System/Library/CoreServices/PowerChime.app &

link:
		@for f in $(FILES) ; do ln -fvs $(REPO)/$$f ~/.$$f; done
		@for f in $(ZSH_LIST) ; do ln -fvs $(REPO)/$$f ~/.zsh.after/$$f; done
		@for d in $(IDEA_DIRS) ; do ln -fvs idea/$$d ~/Library/Preferences/IntelliJIdea$(IDEA_V)/$$d; done

unlink:
		@for f in $(LIST) ; do rm -f ~/.$$f; done
		@for f in $(ZSH_LIST) ; do rm -f ~/.zsh.after/$$f; done

source:
		@for f in $(FILES) ; do source ~/.$$f; done
		@for f in $(ZSH_FILES) ; do source ~/.zsh.after/$$f; done

uninstall:
		defaults write com.apple.PowerChime ChimeOnAllHardware -bool false;killall PowerChime
