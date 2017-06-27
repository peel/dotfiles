{ config, pkgs, ... }:

{
  environment.variables.EDITOR = "emacsclient -tc";
  environment.variables.HOMEBREW_CASK_OPTS = "--appdir=/Applications/cask";
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
  environment.loginShell = "/run/current-system/sw/bin/fish";
  programs.fish.enable = true;
  programs.fish.variables.cfg = "$HOME/.nixpkgs/darwin-configuration.nix";
  programs.fish.variables.darwin = "$HOME/.nix-defexpr/darwin";
  programs.fish.variables.pkgs = "$HOME/.nix-defexpr/channels/nixpkgs";
  programs.fish.variables.fish_key_bindings = "fish_vi_key_bindings";
  services.mopidy.package = "/usr/local";
  services.mopidy.enable = true;
  services.mopidy.mediakeys.package = "/usr/local";
  services.mopidy.mediakeys.enable = true;
}
