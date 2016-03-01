FILES = tmux tmux.conf.user vimrc.after spacemacs  emacs.d/private/magit-gh-issues kwm/kwmrc
PRIVATE_FILES = wakatime.cfg floorc.json
ZSH_BEFORE = before.zsh
ZSH_AFTER = after.zsh
SBT_V := 0.13
REPO := "wrk/dotfiles"
PRIVATE_REPO := git@github.com:peel/dotfiles-private.git
KARABINER_DIR := ~/Library/Application\ Support/Karabiner
default: update

install: init config private brew update link source

init:
		sh -c "`curl -fsSL https://raw.githubusercontent.com/skwp/dotfiles/master/install.sh`" -s ask
		sudo sh $HOME/Brewfile
		git clone --recursive http://github.com/syl20bnr/spacemacs ~/.emacs.d
		sh -c "mkdir -p ~/.sbt/$(SBT_V)/plugins/"
		ln -sfv /usr/local/opt/kwm/*.plist ~/Library/LaunchAgents
		sh -c "launchctl load ~/Library/LaunchAgents/homebrew.mxcl.kwm.plist"
		sh -c "xcode-select --install"
		sh -c echo "[INFO] Remember to map Caps to 80 in Seil"

update: update-deps unlink link

update-deps:
		cd ~/.emacs.d && git pull -r && git submodule sync; git submodule update
		cd ~/.yadr && git pull -r && rake update

config:
		defaults write com.apple.PowerChime ChimeOnAllHardware -bool true; open /System/Library/CoreServices/PowerChime.app &
		defaults write -g ApplePressAndHoldEnabled -bool false #disable default hold-button behaviour
		defaults write NSGlobalDomain KeyRepeat -int 1
		defaults write -g InitialKeyRepeat -int 10
		defaults write com.apple.desktopservices DSDontWriteUSBStores -bool true # no .DS_Store on USB
		defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true # no DS_Store on network
		defaults write -g NSAutomaticSpellingCorrectionEnabled -bool false # disable autocorrect
		defaults write com.apple.finder ShowExternalHardDrivesOnDesktop -bool true # display icons on desktop
		defaults write com.apple.finder ShowHardDrivesOnDesktop -bool true
		defaults write com.apple.finder ShowMountedServersOnDesktop -bool true
		defaults write com.apple.finder ShowRemovableMediaOnDesktop -bool true
		chflags nohidden ~/Library
		launchctl unload -w /System/Library/LaunchDaemons/com.apple.metadata.mds.plist # disable spotlight

brew:
		sh ~/$(REPO)/Brewfile

link:
		@for f in $(FILES) ; do ln -s ~/$(REPO)/$$f ~/.$$f; done
		@for f in $(ZSH_BEFORE) ; do ln -fvs ~/$(REPO)/$$f ~/.zsh.before/$$f; done
		@for f in $(ZSH_AFTER) ; do ln -fvs ~/$(REPO)/$$f ~/.zsh.after/$$f; done
		ln -fvs ~/$(REPO)/sbt/plugins.sbt ~/.sbt/$(SBT_V)/plugins/plugins.sbt
		@for f in $(PRIVATE_FILES) ; do ln -fvs ~/$(REPO)/private/$$f ~/.$$f; done
		@for f in $(REPO)/bin/* ; do chmod +x ~/$(REPO)/bin/$$f && ln -fvs ~/$(REPO)/bin/$$f /usr/local/bin/$$f; done
		mv $(KARABINER_DIR)/private.xml $(KARABINER_DIR)/private.xml.bak && ln -fvs ~/$(REPO)/karabiner/private.xml $(KARABINER_DIR)/private.xml

unlink:
		@for f in $(LIST) ; do rm -f ~/.$$f; done
		@for f in $(ZSH_BEFORE) ; do rm -f ~/.zsh.before/$$f; done
		@for f in $(ZSH_AFTER) ; do rm -f ~/.zsh.after/$$f; done
		rm -f ~/.sbt/$(SBT_V)/plugins/plugins.sbt
		@for f in $(REPO)/bin/* ; do rm -f /usr/local/bin/$$f; done
		rm -f ~/$(KARABINER_DIR)/private.xml && mv $(KARABINER_DIR)/private.xml.bak $(KARABINER_DIR)/private.xml

source:
		@for f in $(FILES) ; do source ~/.$$f; done
		@for f in $(ZSH_FILES) ; do source ~/.zsh.after/$$f; done

private:
		git clone $(PRIVATE_REPO) ~/$(REPO)/private || true

uninstall:
		defaults write com.apple.PowerChime ChimeOnAllHardware -bool false;killall PowerChime
