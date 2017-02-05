DOTFILES := $(HOME)/Documents/wrk/dotfiles
IGNORED := Brewfile Makefile README.org
PRIVATE_REPO := git@github.com:peel/dotfiles-private.git
ELIXIR_EXTRAS := git@github.com:peel/dcdeps.gt

install: brew utils link

utils: osx packages shell editor

brew:
		/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
		sh -c "sudo xcodebuild -license" || true

packages:
		brew bundle

shell:
		echo "/usr/local/bin/fish" | sudo tee -a /etc/shells
		sh -c "chsh -s /usr/local/bin/fish"
		curl -Lo $(HOME)/.config/fish/functions/fisher.fish --create-dirs git.io/fisher

editor:
		git clone --recursive http://github.com/syl20bnr/spacemacs $(HOME)/.emacs.d
		git clone https://github.com/Malabarba/ox-jekyll-subtree.git spacemacs.d/ox-jekyll-subtree

link:
		@for f in $(filter-out $(IGNORED),$(notdir $(wildcard $(PWD)/*))) ; do stow -t ~ $$f; done

clean:
		@for f in $(filter-out $(IGNORED),$(notdir $(wildcard $(PWD)/*))) ; do stow -t ~ -D $$f; done

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
