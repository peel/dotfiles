{ config, pkgs, ... }:

{
  environment.variables.EDITOR = "emacsclient -tc";
  environment.etc."per-user/peel/editorconfig".text = ''
    # top-most EditorConfig file
    root = true

    [*]
    end_of_line = lf
    insert_final_newline = true
    trim_trailing_whitespace = true
    indent_style = space
    indent_size = 2
    charset = utf-8
  '';
  environment.etc."per-user/peel/gitconfig".text = ''
    [include]
      path = ~/.gitconfig.secret
    [color]
      ui = true
    [color "branch"]
      current = yellow reverse
      local = yellow
      remote = green
    [color "diff"]
      meta = yellow bold
      frag = magenta bold
      old = red
      new = green
    [alias]
      # add
      a = add                           # add
      chunkyadd = add --patch           # stage commits chunk by chunk

      # via http://blog.apiaxle.com/post/handy-git-tips-to-stop-you-getting-fired/
      snapshot = !git stash save "snapshot: $(date)" && git stash apply "stash@{0}"
      snapshots = !git stash list --grep snapshot

      #via http://stackoverflow.com/questions/5188320/how-can-i-get-a-list-of-git-branches-ordered-by-most-recent-commit
      recent-branches = !git for-each-ref --count=15 --sort=-committerdate refs/heads/ --format='%(refname:short)'

      # branch
      b = branch -v                     # branch (verbose)

      # commit
      c = commit -m                     # commit with message
      ca = commit -am                   # commit all with message
      ci = commit                       # commit
      amend = commit --amend            # ammend your last commit
      ammend = commit --amend           # ammend your last commit

      # checkout
      co = checkout                     # checkout
      nb = checkout -b                  # create and switch to a new branch (mnemonic: "git new branch branchname...")

      # cherry-pick
      cp = cherry-pick -x               # grab a change from a branch

      # diff
      d = diff                          # diff unstaged changes
      dc = diff --cached                # diff staged changes
      last = diff HEAD^                 # diff last committed change

      # log
      l = log --graph --date=short
      changes = log --pretty=format:\"%h %cr %cn %Cgreen%s%Creset\" --name-status
      short = log --pretty=format:\"%h %cr %cn %Cgreen%s%Creset\"
      simple = log --pretty=format:\" * %s\"
      shortnocolor = log --pretty=format:\"%h %cr %cn %s\"

      # pull
      pl = pull                         # pull

      # push
      ps = push                         # push

      # rebase
      rc = rebase --continue            # continue rebase
      rs = rebase --skip                # skip rebase

      # remote
      r = remote -v                     # show remotes (verbose)

      # reset
      unstage = reset HEAD              # remove files from index (tracking)
      uncommit = reset --soft HEAD^     # go back before last commit, with files in uncommitted state
      filelog = log -u                  # show changes to a file
      mt = mergetool                    # fire up the merge tool

      # stash
      ss = stash                        # stash changes
      sl = stash list                   # list stashes
      sa = stash apply                  # apply stash (restore changes)
      sd = stash drop                   # drop stashes (destory changes)

      # status
      s = status                        # status
      st = status                       # status
      stat = status                     # status

      # tag
      t = tag -n                        # show tags with <n> lines of each tag message
    [format]
      pretty = format:%C(blue)%ad%Creset %C(yellow)%h%C(green)%d%Creset %C(blue)%s %C(magenta) [%an]%Creset
    [mergetool]
      prompt = false
    [mergetool "mvimdiff"]
      cmd="mvim -c 'Gdiff' $MERGED"     # use fugitive.vim for 3-way merge
      keepbackup=false
    [merge]
      summary = true
      verbosity = 1
      tool = mvimdiff
      ff = only
      conflictstyle = diff3
    [apply]
      whitespace = nowarn
    [branch]
      autosetupmerge = true
    [push]
      # 'git push' will push the current branch to its tracking branch
      # the usual default is to push all branches
      default = upstream
    [core]
      autocrlf = false
      editor = vim
      excludesfile = ~/.yadr/git/gitignore
    [advice]
      statusHints = false
    [diff]
      # Git diff will use (i)ndex, (w)ork tree, (c)ommit and (o)bject
      # instead of a/b/c/d as prefixes for patches
      mnemonicprefix = true
      algorithm = patience
    [rerere]
      # Remember my merges
      # http://gitfu.wordpress.com/2008/04/20/git-rerere-rereremember-what-you-did-last-time/
      enabled = true
    [url "git@github.com:"]
      insteadOf = "gh:"
      pushInsteadOf = "github:"
      pushInsteadOf = "git://github.com/"
    [url "git://github.com/"]
      insteadOf = "github:"
    [url "git@gist.github.com:"]
      insteadOf = "gst:"
      pushInsteadOf = "gist:"
      pushInsteadOf = "git://gist.github.com/"
    [url "git://gist.github.com/"]
      insteadOf = "gist:"
  '';
  environment.etc."per-user/peel/ctags".text = ''
    --exclude=*.js
    --exclude=*.git*
    --links=no
    --recurse=yes

    --langdef=scala
    --langmap=scala:.scala
    --regex-scala=/^[ \t]*((abstract|final|sealed|implicit|lazy)[ \t]*)*(private|protected)?[ \t]*class[ \t]+([a-zA-Z0-9_]+)/\4/c,classes/
    --regex-scala=/^[ \t]*((abstract|final|sealed|implicit|lazy)[ \t]*)*(private|protected)?[ \t]*object[ \t]+([a-zA-Z0-9_]+)/\4/c,objects/
    --regex-scala=/^[ \t]*((abstract|final|sealed|implicit|lazy)[ \t]*)*(private|protected)?[ \t]*case class[ \t]+([a-zA-Z0-9_]+)/\4/c,case classes/
    --regex-scala=/^[ \t]*((abstract|final|sealed|implicit|lazy)[ \t]*)*(private|protected)?[ \t]*case object[ \t]+([a-zA-Z0-9_]+)/\4/c,case objects/
    --regex-scala=/^[ \t]*((abstract|final|sealed|implicit|lazy)[ \t]*)*(private|protected)?[ \t]*trait[ \t]+([a-zA-Z0-9_]+)/\4/t,traits/
    --regex-scala=/^[ \t]*type[ \t]+([a-zA-Z0-9_]+)/\1/T,types/
    --regex-scala=/^[ \t]*((abstract|final|sealed|implicit|lazy)[ \t]*)*def[ \t]+([a-zA-Z0-9_]+)/\3/m,methods/
    --regex-scala=/^[ \t]*((abstract|final|sealed|implicit|lazy)[ \t]*)*val[ \t]+([a-zA-Z0-9_]+)/\3/l,constants/
    --regex-scala=/^[ \t]*((abstract|final|sealed|implicit|lazy)[ \t]*)*var[ \t]+([a-zA-Z0-9_]+)/\3/l,variables/
    --regex-scala=/^[ \t]*package[ \t]+([a-zA-Z0-9_.]+)/\1/p,packages/

    --langdef=Clojure
    --langmap=Clojure:.clj
    --regex-clojure=/\([ \t]*create-ns[ \t]+([-[:alnum:]*+!_:\/.?]+)/\1/n,namespace/
    --regex-clojure=/\([ \t]*def[ \t]+([-[:alnum:]*+!_:\/.?]+)/\1/d,definition/
    --regex-clojure=/\([ \t]*defn[ \t]+([-[:alnum:]*+!_:\/.?]+)/\1/f,function/
    --regex-clojure=/\([ \t]*defn-[ \t]+([-[:alnum:]*+!_:\/.?]+)/\1/p,private function/
    --regex-clojure=/\([ \t]*defmacro[ \t]+([-[:alnum:]*+!_:\/.?]+)/\1/m,macro/
    --regex-clojure=/\([ \t]*definline[ \t]+([-[:alnum:]*+!_:\/.?]+)/\1/i,inline/
    --regex-clojure=/\([ \t]*defmulti[ \t]+([-[:alnum:]*+!_:\/.?]+)/\1/a,multimethod definition/
    --regex-clojure=/\([ \t]*defmethod[ \t]+([-[:alnum:]*+!_:\/.?]+)/\1/b,multimethod instance/
    --regex-clojure=/\([ \t]*defonce[ \t]+([-[:alnum:]*+!_:\/.?]+)/\1/c,definition (once)/
    --regex-clojure=/\([ \t]*defstruct[ \t]+([-[:alnum:]*+!_:\/.?]+)/\1/s,struct/
    --regex-clojure=/\([ \t]*intern[ \t]+([-[:alnum:]*+!_:\/.?]+)/\1/v,intern/
    --regex-clojure=/\([ \t]*ns[ \t]+([-[:alnum:]*+!_:\/.?]+)/\1/n,namespace/
  '';
  environment.etc."per-user/peel/vimrc".text = ''
    set nonumber
    set relativenumber
    colorscheme default
  '';
  environment.shellAliases = {
    e = "em";
    cx = "chmod +x";
    c = "clear";
    cls = "clear;ls";

    psa = "ps aux";
    pasg = "psa | grep";
    k9 = "kill -9";

    pbc = "pbcopy";
    pbp = "pbpaste";

    #edit config
    ee = "$EDITOR $HOME/emacs.d/init.el";
    et = "$EDITOR $HOME/.tmux.conf";
    ez = "$EDITOR $HOME/.config/fish/config.fish";
    ea = "$EDITOR $fhome/aliases/";

    #docker
    dm = "docker-machine";
    dmi = "dm ip";
    dme = "dm env";
    dms = "dm start and dme";
    dmR = "dm rm default";
    dmc = "dm create --driver virtualbox default";
    dc = "docker-compose";
    dcu = "dc up";
    dck = "dc kill";
    dcR = "dc rm -f";
    dckR = "dck and dcR";
    d = "docker";
    ds = "d start";
    dS = "d stop";
    dr = "d restart";
    dl = "d log";
    di = "d images";
    diR = "d rmi -f (di -aq)";
    dps = "d ps";
    dex = "d exec";
    nwtmenu = "d run -it --rm  -v ~/.ssh/ansible.pem:/root/.ssh/id_rsa -v ~/.ssh/known_hosts:/root/.ssh/known_hosts reg.nwt.se/nwt/nwtmenu";
    #navigation
    o = "open";
    ":q" = "exit";
    ".." = "cd ..";
    "..." = "../..";
    "...." = "../../..";
    "....." = "../../../..";
    #browsing;
    less = "less -R";
    tailf = "tail -f";
    ls = "ls -Gh";
    ll = "ls -al";
    lsg = "ll | grep";
    rr = "ranger";
    #disk;
    df = "df -h";
    du = "du -h -d 2";
    #fish
    fr = "fish_reload";

    #hub
    git = "hub";
    #git
    gs = "git status";
    gst = "git stash";
    gsp = "git stash pop";
    gsh = "git show";
    gi = "e .gitignore";
    gcm = "git ci -m";
    gcim = "git ci -m";
    gci = "git ci";
    gco = "git co";
    gcp = "git cp";
    ga = "git add -A";
    gap = "git add -p";
    guns = "git unstage";
    gunc = "git uncommit";
    gm = "git merge";
    gms = "git merge --squash";
    gam = "git amend --reset-author";
    grv = "git remote -v";
    grr = "git remote rm";
    grad = "git remote add";
    gr = "git rebase";
    gra = "git rebase --abort";
    ggrc = "git rebase --continue";
    gbi = "git rebase --interactive";
    gl = "git l";
    glg = "git l";
    glog = "git l";
    co = "git co";
    gf = "git fetch";
    gfp = "git fetch --prune";
    gfa = "git fetch --all";
    gfap = "git fetch --all --prune";
    gfch = "git fetch";
    gd = "git diff";
    gb = "git b";
    # = Staged and cached are the same thing;
    gdc = "git diff --cached -w";
    gds = "git diff --staged -w";
    gpl = "git pull";
    gplr = "git pull --rebase";
    gps = "git push";
    gpsh = "git push -u origin `git rev-parse --abbrev-ref HEAD`";
    gnb = "git nb"; # new branch aka checkout -b;
    grs = "git reset";
    grsh = "git reset --hard";
    gcln = "git clean";
    gclndf = "git clean -df";
    gclndfx = "git clean -dfx";
    gsm = "git submodule";
    gsmi = "git submodule init";
    gsmu = "git submodule update";
    gt = "git t";
    gbg = "git bisect good";
    gbb = "git bisect bad";
    gdmb = "git branch --merged | grep -v \"\*\" | xargs -n 1 git branch -d";
    #git workflow aliases;
    gp = "ghi-epic-push";
    ghio = "ghi-epic-issue-open";
    ghios = "ghi-opens";
    ghic = "ghi-close";
    ghe = "ghi-epic-open";
    ghE = "ghi-epic-close";
    gn = "ghi-nows";
    #kubernetes
    kk = "kubectl config use-context";
    kc = "kubectl config current-context";
  };
  programs.bash.enable = false;
  programs.fish.enable = true;
  programs.fish.variables.cfg = "$HOME/.nixpkgs/darwin-configuration.nix";
  programs.fish.variables.darwin = "$HOME/.nix-defexpr/darwin";
  programs.fish.variables.pkgs = "$HOME/.nix-defexpr/channels/nixpkgs";
  programs.fish.variables.fish_key_bindings = "fish_vi_key_bindings";
  programs.tmux.enable = true;
  programs.tmux.enableSensible = true;
  programs.tmux.enableMouse = true;
  programs.tmux.enableVim = true;
  programs.tmux.tmuxConfig = ''
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

    set-option -g status-left '#[fg=black, fg=cyan, noreverse]λ '
    set-option -g status-right "#(~/.tmux/prompt.sh right)"

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
  '';
  services.mopidy.package = "/usr/local";
  services.mopidy.enable = true;
  services.mopidy.mediakeys.package = "/usr/local";
  services.mopidy.mediakeys.enable = true;
}
