install:
link:
	ln -s ideavimrc ~/.ideavimrc
	ln -s zshrc.after ~/.zshrc.after
	ln -s tmux.conf.user ~/.tmux.conf.user
	ln -s vimrc.after ~/.vimrc.after
source:
	source ~/.zshrc.after
	source ~/.tmux.conf.user
	source ~/.vimrc.after
