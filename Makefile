FILES = tmux tmux.conf config/fish vimrc spacemacs.d emacs.d/private/magit-gh-issues kwm khdrc config/karabiner/karabiner.json gitconfig config/git/ignore inputrc editorconfig ctags
PRIVATE_FILES = wakatime.cfg floorc.json
SBT_V := 0.13
REPO := "wrk/dotfiles"
PRIVATE_REPO := git@github.com:peel/dotfiles-private.git
ELIXIR_EXTRAS := git@github.com:peel/dcdeps.gt
default: update

install: brew init osx private update

brew:
		/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
		sh -c "sudo xcodebuild -license" || true

init:
		brew install mas
		brew bundle
		echo "/usr/local/bin/fish" | sudo tee -a /etc/shells
		sh -c "chsh -s /usr/local/bin/fish"
		curl -Lo $(HOME)/.config/fish/functions/fisher.fish --create-dirs git.io/fisher
		git clone --recursive http://github.com/syl20bnr/spacemacs $(HOME)/.emacs.d
		git clone https://github.com/Malabarba/ox-jekyll-subtree.git spacemacs.d/ox-jekyll-subtree
		sh -c "mkdir -p $(HOME)/.sbt/$(SBT_V)/plugins/"

update: update-spacemacs unlink link private-update

update-spacemacs:
		cd $(HOME)/.emacs.d && git pull -r && git submodule sync; git submodule update

osx:
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
		defaults write NSGlobalDomain _HIHideMenuBar -bool true # hide menu bar
		chflags nohidden $(HOME)/Library
		launchctl unload -w /System/Library/LaunchDaemons/com.apple.metadata.mds.plist # disable spotlight

link:
		@for f in $(FILES) ; do ln -s $(HOME)/$(REPO)/$$f $(HOME)/.$$f; done
		ln -fvs $(HOME)/$(REPO)/sbt/plugins.sbt $(HOME)/.sbt/$(SBT_V)/plugins/plugins.sbt
		@for f in $(wildcard $(REPO)/bin/*) ; do chmod +x $(HOME)/$(REPO)/bin/$$f && ln -fvs $(HOME)/$(REPO)/bin/$$f /usr/local/bin/$$f; done
		@for f in $(wildcard $(REPO)/LaunchAgents/*) ; do ln -fvs $$f $(HOME)/Library/LaunchAgents/$$f; done

unlink:
		@for f in $(FILES) ; do rm -f $(HOME)/.$$f; done
		rm -f $(HOME)/.sbt/$(SBT_V)/plugins/plugins.sbt
		@for f in $(wildcard $(REPO)/bin/*) ; do rm -f /usr/local/bin/$$f; done
		@for f in $(wildcard $(REPO)/LaunchAgents/*) ; do rm -f $(HOME)/Library/LaunchAgents/$$f; done

extras:
		@for f in $(ELIXIR_EXTRAS) ; do git clone $$f $(HOME)/wrk/$$f && mix escript.build && mix escript.install; done

private-clone:
		git clone $(PRIVATE_REPO) $(HOME)/$(REPO)/private || true

private-update: private-pull private-unlink private-link

private-pull:
	cd $(HOME)/$(REPO)/private && git pull

private-link:
		@for f in $(PRIVATE_FILES) ; do ln -fvs $(HOME)/$(REPO)/private/$$f $(HOME)/.$$f; done
		ln -fvs $(HOME)/$(REPO)/private/spacemacs.d/peel $(HOME)/.spacemacs.d/peel

private-unlink:
		@for f in $(PRIVATE_FILES) ; do rm -f $(HOME)/.$$f; done
		rm -rf $(HOME)/.spacemacs.d/peel
