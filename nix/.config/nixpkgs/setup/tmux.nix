''
set-option -g status-position top

set -g history-limit 10000

set -g base-index 1
setw -g pane-base-index 1

setw -g automatic-rename on

set-option -g status-justify "left"
set-option -g status-left-length 200
set-option -g status-right-length 200

set -g status-fg brightwhite
set -g status-bg black

setw -g window-status-format "#[bg=black, fg=cyan, noreverse] #I #[bg=brightblack, fg=brightcyan, noreverse] #W "
setw -g window-status-current-format "#[bg=cyan, fg=white, noreverse] #I #[fg=red, bg=white] #W "

set-option -g status-left ""
set-option -g status-right '#(TZ=GMT date +"%F %R") '
''
