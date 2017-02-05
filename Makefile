BREW := $(shell command -v brew 2> /dev/null)
NEW_SHELL := $(shell which fish)
CURRENT_SHELL := $(shell dscl . -read /Users/$(USER) UserShell | awk '{ print $$(2) }' | tr -d [:blank:])
DOTFILES := $(HOME)/Documents/wrk/dotfiles
IGNORED := Brewfile Makefile README.org CNAME install.sh docs
PRIVATE_REPO := git@github.com:peel/dotfiles-private.git
ELIXIR_EXTRAS := git@github.com:peel/dcdeps.gt

default: install
install: minimal osx brew-packages
minimal: brew brew-minimal shell editor link

brew:
ifndef BREW
		@echo "Installing homebrew"
		@ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
		@sh -c "sudo xcodebuild -license" || true
else
		@echo "homebrew already installed."
endif

brew-minimal:
		@echo "Installing base packages"
		@brew install git stow fish tmux

brew-packages:
		@echo "Installing packages"
		@brew bundle

shell:
ifneq ($(NEW_SHELL),$(CURRENT_SHELL))
		@echo "Setting up .$(NEW_SHELL). for $(USER) instead of.$(CURRENT_SHELL)."
		@echo "/usr/local/bin/fish" | sudo tee -a /etc/shells
		@sh -c "chsh -s /usr/local/bin/fish"
		@curl -Lo $(HOME)/.config/fish/functions/fisher.fish --create-dirs git.io/fisher
else
		@echo "Not setting up shell for $(USER) instead of $(CURRENT_SHELL)"
endif

editor:
ifeq ("$(wildcard $(HOME)/.emacs.d/)","")
		@echo "Setting up Emacs"
		@git clone --recursive http://github.com/syl20bnr/spacemacs $(HOME)/.emacs.d
		@git clone https://github.com/Malabarba/ox-jekyll-subtree.git spacemacs.d/ox-jekyll-subtree
else
		@echo "Emacs already set up"
endif

link:
		@echo "Linking dotfiles"
		@for f in $(filter-out $(IGNORED),$(notdir $(wildcard $(PWD)/*))) ; do stow -t ~ $$f; done

clean:
		@echo "Removing dotfiles"
		@for f in $(filter-out $(IGNORED),$(notdir $(wildcard $(PWD)/*))) ; do stow -t ~ -D $$f; done

osx:
		@echo "Power dis-/connected chime"
		@defaults write com.apple.PowerChime ChimeOnAllHardware -bool true; open /System/Library/CoreServices/PowerChime.app &
		@echo "disable default hold-button behaviour"
		@defaults write -g ApplePressAndHoldEnabled -bool false
		@defaults write NSGlobalDomain KeyRepeat -int 1
		@defaults write -g InitialKeyRepeat -int 10
		@echo "No .DS_Store"
		@defaults write com.apple.desktopservices DSDontWriteUSBStores -bool true #USB
		@defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true # network
		@echo "Disable autocorrect"
		@defaults write -g NSAutomaticSpellingCorrectionEnabled -bool false
		@echo "Display icons on desktop"
		@defaults write com.apple.finder ShowExternalHardDrivesOnDesktop -bool true
		@defaults write com.apple.finder ShowHardDrivesOnDesktop -bool true
		@defaults write com.apple.finder ShowMountedServersOnDesktop -bool true
		@defaults write com.apple.finder ShowRemovableMediaOnDesktop -bool true
		@echo "Hide menu bar"
		@defaults write NSGlobalDomain _HIHideMenuBar -bool true
		@chflags nohidden $(HOME)/Library
		@echo "Disable Spotlight"
		@sudo launchctl unload -w /System/Library/LaunchDaemons/com.apple.metadata.mds.plist
		@echo "Setting up Activity Monitor"
		@defaults write com.apple.ActivityMonitor OpenMainWindow -bool true # Show the main window when launching Activity Monitor
		@defaults write com.apple.ActivityMonitor IconType -int 5 # Visualize CPU usage in the Activity Monitor Dock icon
		@defaults write com.apple.ActivityMonitor ShowCategory -int 0 # Show all processes in Activity Monitor
		@defaults write com.apple.ActivityMonitor SortColumn -string "CPUUsage" # Sort Activity Monitor results by CPU usage
		@defaults write com.apple.ActivityMonitor SortDirection -int 0
		@echo "Setting up Transmission"
		@defaults write org.m0k.transmission DownloadAsk -bool false # Don’t prompt for confirmation before downloading
		@defaults write org.m0k.transmission MagnetOpenAsk -bool false # Don’t prompt for confirmation before downloading
		@defaults write org.m0k.transmission DeleteOriginalTorrent -bool true # Trash original torrent files
		@defaults write org.m0k.transmission WarningDonate -bool false # Hide the donate message
		@defaults write org.m0k.transmission WarningLegal -bool false # Hide the legal disclaimer
		@defaults write org.m0k.transmission BlocklistNew -bool true # IP block list.
		@defaults write org.m0k.transmission BlocklistURL -string "http://john.bitsurge.net/public/biglist.p2p.gz"
		@defaults write org.m0k.transmission BlocklistAutoUpdate -bool true
