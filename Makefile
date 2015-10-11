FILES = ideavimrc tmux.conf.user vimrc.after spacemacs
ZSH_FILES = my.zsh
REPO := "wrk/dotfiles"
IDEA_V := 14
IDEA_DIRS = colors fileTemplates inspection keymaps options quicklists templates
default: update

install: init config brew update link source

init:
		sh -c "`curl -fsSL https://raw.githubusercontent.com/skwp/dotfiles/master/install.sh`"
		sudo sh $HOME/Brewfile
		git clone --recursive http://github.com/syl20bnr/spacemacs ~/.emacs.d
		sh -c "unzip -o ~/$(REPO)/settings.jar -d ~/$(REPO)/idea"
		sh -c "curl -o ~/$(REPO)/idea/BlueForest.xml https://raw.githubusercontent.com/sirthias/BlueForest/master/BlueForest.xml"

update: update-deps unlink link

update-deps:
		cd ~/.emacs.d && git pull -r && git submodule sync; git submodule update
		cd ~/.yadr && git pull -r && rake update
		sh -c "rm -rf ~/$(REPO)/idea && unzip -o ~/$(REPO)/settings.jar -d ~/$(REPO)/idea"
		sh -c "curl -o ~/$(REPO)/idea/BlueForest.xml https://raw.githubusercontent.com/sirthias/BlueForest/master/BlueForest.xml"

config:
		defaults write com.apple.PowerChime ChimeOnAllHardware -bool true; open /System/Library/CoreServices/PowerChime.app &

brew:
	sh ~/$(REPO)/Brewfile

link:
		@for f in $(FILES) ; do ln -fvs ~/$(REPO)/$$f ~/.$$f; done
		@for f in $(ZSH_LIST) ; do ln -fvs ~/$(REPO)/$$f ~/.zsh.after/$$f; done
		@for d in $(IDEA_DIRS) ; do mv ~/Library/Preferences/IntelliJIdea$(IDEA_V)/$$d ~/Library/Preferences/IntelliJIdea$(IDEA_V)/$$d.bak && ln -fvs ~/$(REPO)/idea/$$d ~/Library/Preferences/IntelliJIdea$(IDEA_V)/$$d; done

unlink:
		@for f in $(LIST) ; do rm -f ~/.$$f; done
		@for f in $(ZSH_LIST) ; do rm -f ~/.zsh.after/$$f; done
		@for d in $(IDEA_DIRS) ; do rm -f ~/Library/Preferences/IntelliJIdea$(IDEA_V)/$$d && mv ~/Library/Preferences/IntelliJIdea$(IDEA_V)/$$d.bak ~/Library/Preferences/IntelliJIdea$(IDEA_V)/$$d ; done

source:
		@for f in $(FILES) ; do source ~/.$$f; done
		@for f in $(ZSH_FILES) ; do source ~/.zsh.after/$$f; done

uninstall:
		defaults write com.apple.PowerChime ChimeOnAllHardware -bool false;killall PowerChime
