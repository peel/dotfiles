{ colors, tmux-prompt }:
with colors;
''
# Bigger history
set -g history-limit 10000

#### status and window appearance and style
set-option -g status-justify "left"
set-option -g status-left-length 200
set-option -g status-right-length 200
set -g status-fg brightwhite
set -g status-bg black
set -g pane-border-fg blue
set -g pane-active-border-fg blue
set -g message-fg black
set -g message-bg white
set -g message-attr bold

# start indexing windows from 1, just like tabs
set -g base-index 1
setw -g pane-base-index 1

#### status bar
setw -g window-status-format "#[bg=black, fg=cyan, noreverse] #I #[bg=brightblack, fg=brightcyan, noreverse] #W "
setw -g window-status-current-format "#[bg=brightblue, fg=white, noreverse] #I #[fg=brightcyan, bg=brightgreen] #W "
setw -g window-status-current-attr dim
setw -g window-status-bg green
setw -g window-status-fg black
set -g window-status-attr reverse
set -g window-status-activity-attr bold

set-option -g status-left '#[fg=black, fg=cyan, noreverse] λ '
set-option -g status-right "#(${tmux-prompt}/bin/tmux-prompt)"

#### bindings
# screen prefix
unbind C-b
set -g prefix C-a
bind a send-prefix

# resize panes
bind-key -r < resize-pane -L 5
bind-key -r > resize-pane -R 5
bind-key -r + resize-pane -U 10
bind-key -r = resize-pane -D 10

# visual notification of activity in other windows
setw -g monitor-activity on
set -g visual-activity on

# splits and vertical splits
bind-key | split-window -h -p 50 -c "#{pane_current_path}"
bind-key - split-window -p 50 -c "#{pane_current_path}"

# force a reload of the config file
unbind r
bind r source-file ~/.tmux.conf \; display "Reloaded!"
# quick pane cycling
unbind ^A
bind ^A select-pane -t :.+

set -g @tpm_plugins '             \
  tmux-plugins/tpm                \
  tmux-plugins/tmux-resurrect     \
  tmux-plugins/tmux-yank          \
  tmux-plugins/tmux-copycat       \
'

run '~/.tmux/plugins/tpm/tpm'
''
