{ config, pkgs, ... }:

{
  imports = [ ./fish.nix ];
  time.timeZone = "Europe/Warsaw";
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };
  services.emacs.enable = true;
  services.emacs.package = pkgs.emacs;
  environment.variables.EDITOR = "${pkgs.emacs}/bin/emacsclient -tc";
  environment.variables.SHELL = "${pkgs.fish}/bin/fish";
  environment.etc."editorconfig".text = ''
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
  environment.etc."gitignore".text = ''
    ### Tags ###
    # Ignore tags created by etags, ctags, gtags (GNU global) and cscope
    TAGS
    .TAGS
    !TAGS/
    tags
    .tags
    !tags/
    gtags.files
    GTAGS
    GRTAGS
    GPATH
    GSYMS
    cscope.files
    cscope.out
    cscope.in.out
    cscope.po.out

    ### Ensime ###
    # Ensime specific
    .ensime
    .ensime_cache/
    .ensime_lucene/
  '';
  environment.etc."gitconfig".text = ''
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
  environment.etc."ctags".text = ''
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
  environment.etc."gtags.conf".text = ''
    #
    # Copyright (c) 1998, 1999, 2000, 2001, 2002, 2003, 2010, 2011, 2013,
    #	2015, 2016, 2017
    #	Tama Communications Corporation
    #
    # This file is part of GNU GLOBAL.
    #
    # This file is free software; as a special exception the author gives
    # unlimited permission to copy and/or distribute it, with or without
    # modifications, as long as this notice is preserved.
    #
    # This program is distributed in the hope that it will be useful, but
    # WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
    # implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    #
    # *
    # Configuration file for GNU GLOBAL source code tagging system.
    #
    # Basically, GLOBAL doesn't need this configuration file ('gtags.conf'),
    # because it has default values in itself. If you have this file as
    # '/etc/gtags.conf' or "$HOME/.globalrc" then GLOBAL overwrite the default
    # values with values in the file.
    # Configuration file is also necessary to use plug-in parsers.
    #
    # The format is similar to termcap(5). You can specify a target with
    # GTAGSLABEL environment variable. Default target is 'default'.
    #
    # If you want to have default values for yourself, it is recommended to
    # use the following method:
    #
    # default:\
    #	:tc=default@~/.globalrc:\	<= includes default values from ~/.globalrc.
    #	:tc=native:
    #
    # Please refer to gtags.conf(5) for details.
    #
    default:\
      :tc=native:
    native:\
      :tc=gtags:tc=htags:
    user:\
      :tc=user-custom:tc=htags:
    ctags:\
      :tc=exuberant-ctags:tc=htags:
    new-ctags:\
      :tc=universal-ctags:tc=htags:
    pygments:\
      :tc=pygments-parser:tc=htags:
    #
    # [How to merge two or more parsers?]
    #
    # Rule: The first matched langmap is adopted.
    #
    # ":tc=builtin-parser:tc=pygments-parser:" means:
    #	If built-in parser exists for the target, it is used.
    #	Else if pygments parser exists it is used.
    #
    native-pygments:\
      :tc=builtin-parser:tc=pygments-parser:tc=htags:
    #---------------------------------------------------------------------
    # Configuration for gtags(1)
    # See gtags(1).
    #---------------------------------------------------------------------
    common:\
      :skip=HTML/,HTML.pub/,tags,TAGS,ID,y.tab.c,y.tab.h,gtags.files,cscope.files,cscope.out,cscope.po.out,cscope.in.out,SCCS/,RCS/,CVS/,CVSROOT/,{arch}/,autom4te.cache/,*.orig,*.rej,*.bak,*~,#*#,*.swp,*.tmp,*_flymake.*,*_flymake,*.o,*.a,*.so,*.lo,*.zip,*.gz,*.bz2,*.xz,*.lzh,*.Z,*.tgz,*.min.js,*min.css:
    #
    # Built-in parsers.
    #
    gtags:\
      :tc=common:\
      :tc=builtin-parser:
    builtin-parser:\
      :langmap=c\:.c.h,yacc\:.y,asm\:.s.S,java\:.java,cpp\:.c++.cc.hh.cpp.cxx.hxx.hpp.C.H,php\:.php.php3.phtml:
    #
    # skeleton for user's custom parser.
    #
    user-custom|User custom plugin parser:\
      :tc=common:\
      :langmap=c\:.c.h:\
      :gtags_parser=c\:$libdir/gtags/user-custom.la:
    #
    # Plug-in parser to use Exuberant Ctags.
    #
    # Most of the following definitions were automatically generated by this command line:
    # $ perl maps2conf.pl /user/local/bin/ctags '$libdir/gtags/exuberant-ctags.la'
    # ('/user/local/bin/ctags' should be replaced with the path of Exuberant Ctags.)
    #
    exuberant-ctags|plugin-example|setting to use Exuberant Ctags plug-in parser:\
      :tc=common:\
      :ctagscom=/nix/store/qknwh7w0f3n59hywpdgg1hqd53mgrh6r-ctags-816/bin/ctags:\
      :ctagslib=$libdir/gtags/exuberant-ctags.la:\
      :langmap=Ant\:(*.build.xml):\
      :langmap=Asm\:.asm.ASM.s.S.A51(*.29[kK])(*.[68][68][kKsSxX])(*.[xX][68][68]):\
      :langmap=Asp\:.asp.asa:\
      :langmap=Awk\:.awk.gawk.mawk:\
      :langmap=Basic\:.bas.bi.bb.pb:\
      :langmap=BETA\:.bet:\
      :langmap=C\:.c:\
      :langmap=C++\:.c++.cc.cp.cpp.cxx.h.h++.hh.hp.hpp.hxx.C.H:\
      :langmap=C#\:.cs:\
      :langmap=Cobol\:.cbl.cob.CBL.COB:\
      :langmap=DosBatch\:.bat.cmd:\
      :langmap=Eiffel\:.e:\
      :langmap=Erlang\:.erl.ERL.hrl.HRL:\
      :langmap=Flex\:.as.mxml:\
      :langmap=Fortran\:.f.for.ftn.f77.f90.f95.F.FOR.FTN.F77.F90.F95:\
      :langmap=HTML\:.htm.html:\
      :langmap=Java\:.java:\
      :langmap=JavaScript\:.js:\
      :langmap=Lisp\:.cl.clisp.el.l.lisp.lsp:\
      :langmap=Lua\:.lua:\
      :langmap=Make\:.mak.mk([Mm]akefile)(GNUmakefile):\
      :langmap=MatLab\:.m:\
      :langmap=OCaml\:.ml.mli:\
      :langmap=Pascal\:.p.pas:\
      :langmap=Perl\:.pl.pm.plx.perl:\
      :langmap=PHP\:.php.php3.phtml:\
      :langmap=Python\:.py.pyx.pxd.pxi.scons:\
      :langmap=REXX\:.cmd.rexx.rx:\
      :langmap=Ruby\:.rb.ruby:\
      :langmap=Scala\:.scala:\ :gtags_parser=Scala\:${pkgs.global}/lib/gtags/exuberant-ctags.la:\
      :langmap=Scheme\:.SCM.SM.sch.scheme.scm.sm:\
      :langmap=Sh\:.sh.SH.bsh.bash.ksh.zsh:\
      :langmap=SLang\:.sl:\
      :langmap=SML\:.sml.sig:\
      :langmap=SQL\:.sql:\
      :langmap=Tcl\:.tcl.tk.wish.itcl:\
      :langmap=Tex\:.tex:\
      :langmap=Vera\:.vr.vri.vrh:\
      :langmap=Verilog\:.v:\
      :langmap=VHDL\:.vhdl.vhd:\
      :langmap=Vim\:.vim:\
      :langmap=YACC\:.y:\
      :gtags_parser=Ant\:$ctagslib:\
      :gtags_parser=Asm\:$ctagslib:\
      :gtags_parser=Asp\:$ctagslib:\
      :gtags_parser=Awk\:$ctagslib:\
      :gtags_parser=Basic\:$ctagslib:\
      :gtags_parser=BETA\:$ctagslib:\
      :gtags_parser=C\:$ctagslib:\
      :gtags_parser=C++\:$ctagslib:\
      :gtags_parser=C#\:$ctagslib:\
      :gtags_parser=Cobol\:$ctagslib:\
      :gtags_parser=DosBatch\:$ctagslib:\
      :gtags_parser=Eiffel\:$ctagslib:\
      :gtags_parser=Erlang\:$ctagslib:\
      :gtags_parser=Flex\:$ctagslib:\
      :gtags_parser=Fortran\:$ctagslib:\
      :gtags_parser=HTML\:$ctagslib:\
      :gtags_parser=Java\:$ctagslib:\
      :gtags_parser=JavaScript\:$ctagslib:\
      :gtags_parser=Lisp\:$ctagslib:\
      :gtags_parser=Lua\:$ctagslib:\
      :gtags_parser=Make\:$ctagslib:\
      :gtags_parser=MatLab\:$ctagslib:\
      :gtags_parser=OCaml\:$ctagslib:\
      :gtags_parser=Pascal\:$ctagslib:\
      :gtags_parser=Perl\:$ctagslib:\
      :gtags_parser=PHP\:$ctagslib:\
      :gtags_parser=Python\:$ctagslib:\
      :gtags_parser=REXX\:$ctagslib:\
      :gtags_parser=Ruby\:$ctagslib:\
      :gtags_parser=Scheme\:$ctagslib:\
      :gtags_parser=Sh\:$ctagslib:\
      :gtags_parser=SLang\:$ctagslib:\
      :gtags_parser=SML\:$ctagslib:\
      :gtags_parser=SQL\:$ctagslib:\
      :gtags_parser=Tcl\:$ctagslib:\
      :gtags_parser=Tex\:$ctagslib:\
      :gtags_parser=Vera\:$ctagslib:\
      :gtags_parser=Verilog\:$ctagslib:\
      :gtags_parser=VHDL\:$ctagslib:\
      :gtags_parser=Vim\:$ctagslib:\
      :gtags_parser=YACC\:$ctagslib:
    #
    # Plug-in parser to use Universal Ctags.
    #
    # Most of the following definitions were automatically generated by this command line:
    # $ perl maps2conf.pl /usr/local/bin/ctags '$libdir/gtags/universal-ctags.la'
    # ('/user/local/bin/ctags' should be replaced with the path of Universal Ctags.)
    #
    universal-ctags|setting to use Universal Ctags plug-in parser:\
      :tc=common:\
      :ctagscom=/nix/store/spgmx4alk0p2ppd1kygcmvp166xyrg7l-universal-ctags-2018-01-05/bin/ctags:\
      :ctagslib=$libdir/gtags/universal-ctags.la:\
      :langmap=Ada\:.adb.ads.Ada:\
      :langmap=Ant\:(build.xml)(*.build.xml).ant.xml:\
      :langmap=Asm\:.A51(*.29[kK])(*.[68][68][kKsSxX])(*.[xX][68][68]).asm.ASM.s.S:\
      :langmap=Asp\:.asp.asa:\
      :langmap=Autoconf\:(configure.in).ac:\
      :langmap=Automake\:(Makefile.am).am:\
      :langmap=Awk\:.awk.gawk.mawk:\
      :langmap=Basic\:.bas.bi.bb.pb:\
      :langmap=BETA\:.bet:\
      :langmap=Clojure\:.clj.cljs.cljc:\
      :langmap=C\:.c:\
      :langmap=C++\:.c++.cc.cp.cpp.cxx.h.h++.hh.hp.hpp.hxx.inl:\
      :langmap=CSS\:.css:\
      :langmap=C#\:.cs:\
      :langmap=ctags\:.ctags:\
      :langmap=Cobol\:.cbl.cob.CBL.COB:\
      :langmap=CUDA\:.cu.cuh:\
      :langmap=D\:.d.di:\
      :langmap=Diff\:.diff.patch:\
      :langmap=DTD\:.dtd.mod:\
      :langmap=DTS\:.dts.dtsi:\
      :langmap=DosBatch\:.bat.cmd:\
      :langmap=Eiffel\:.e:\
      :langmap=elm\:.elm:\
      :langmap=Erlang\:.erl.ERL.hrl.HRL:\
      :langmap=Falcon\:.fal.ftd:\
      :langmap=Flex\:.as.mxml:\
      :langmap=Fortran\:.f.for.ftn.f77.f90.f95.f03.f08.f15:\
      :langmap=gdbinit\:(.gdbinit).gdb:\
      :langmap=Go\:.go:\
      :langmap=HTML\:.htm.html:\
      :langmap=Iniconf\:.ini.conf:\
      :langmap=ITcl\:.itcl:\
      :langmap=Java\:.java:\
      :langmap=JavaProperties\:.properties:\
      :langmap=JavaScript\:.js.jsx:\
      :langmap=JSON\:.json:\
      :langmap=LdScript\:(*.lds.S)(ld.*).lds.scr.ld:\
      :langmap=Lisp\:.cl.clisp.el.l.lisp.lsp:\
      :langmap=Lua\:.lua:\
      :langmap=M4\:.m4.spt:\
      :langmap=man\:.1.2.3.4.5.6.7.8.9:\
      :langmap=Make\:([Mm]akefile)(GNUmakefile).mak.mk:\
      :langmap=MatLab\:.m:\
      :langmap=Myrddin\:.myr:\
      :langmap=ObjectiveC\:.mm.m.h:\
      :langmap=OldC++\:.c++.cc.cp.cpp.cxx.h.h++.hh.hp.hpp.hxx.inl:\
      :langmap=OldC\:.c:\
      :langmap=OCaml\:.ml.mli.aug:\
      :langmap=passwd\:(passwd):\
      :langmap=Pascal\:.p.pas:\
      :langmap=Perl\:.pl.pm.ph.plx.perl:\
      :langmap=Perl6\:.p6.pm6.pm.pl6:\
      :langmap=PHP\:.php.php3.php4.php5.php7.phtml:\
      :langmap=pod\:.pod:\
      :langmap=Protobuf\:.proto:\
      :langmap=puppetManifest\:.pp:\
      :langmap=Python\:.py.pyx.pxd.pxi.scons:\
      :langmap=QemuHX\:.hx:\
      :langmap=R\:.r.R.s.q:\
      :langmap=REXX\:.cmd.rexx.rx:\
      :langmap=Robot\:.robot:\
      :langmap=RpmSpec\:.spec:\
      :langmap=reStructuredText\:.rest.reST.rst:\
      :langmap=Ruby\:.rb.ruby:\
      :langmap=Rust\:.rs:\
      :langmap=Scheme\:.SCM.SM.sch.scheme.scm.sm:\
      :langmap=Sh\:.sh.SH.bsh.bash.ksh.zsh.ash:\
      :langmap=SLang\:.sl:\
      :langmap=SML\:.sml.sig:\
      :langmap=SQL\:.sql:\
      :langmap=SystemdUnit\:.unit.service.socket.device.mount.automount.swap.target.path.timer.snapshot.scope.slice.time:\
      :langmap=Tcl\:.tcl.tk.wish.exp:\
      :langmap=Tex\:.tex:\
      :langmap=TTCN\:.ttcn.ttcn3:\
      :langmap=Vera\:.vr.vri.vrh:\
      :langmap=Verilog\:.v:\
      :langmap=SystemVerilog\:.sv.svh.svi:\
      :langmap=VHDL\:.vhdl.vhd:\
      :langmap=Vim\:(vimrc)([._]vimrc)(gvimrc)([._]gvimrc).vim.vba:\
      :langmap=WindRes\:.rc:\
      :langmap=YACC\:.y:\
      :langmap=YumRepo\:.repo:\
      :langmap=Zephir\:.zep:\
      :langmap=DBusIntrospect\:.xml:\
      :langmap=Glade\:.glade:\
      :langmap=Maven2\:(pom.xml).pom.xml:\
      :langmap=PlistXML\:.plist:\
      :langmap=RelaxNG\:.rng:\
      :langmap=SVG\:.svg:\
      :langmap=XSLT\:.xsl.xslt:\
      :gtags_parser=Ada\:$ctagslib:\
      :gtags_parser=Ant\:$ctagslib:\
      :gtags_parser=Asm\:$ctagslib:\
      :gtags_parser=Asp\:$ctagslib:\
      :gtags_parser=Autoconf\:$ctagslib:\
      :gtags_parser=Automake\:$ctagslib:\
      :gtags_parser=Awk\:$ctagslib:\
      :gtags_parser=Basic\:$ctagslib:\
      :gtags_parser=BETA\:$ctagslib:\
      :gtags_parser=Clojure\:$ctagslib:\
      :gtags_parser=C\:$ctagslib:\
      :gtags_parser=C++\:$ctagslib:\
      :gtags_parser=CSS\:$ctagslib:\
      :gtags_parser=C#\:$ctagslib:\
      :gtags_parser=ctags\:$ctagslib:\
      :gtags_parser=Cobol\:$ctagslib:\
      :gtags_parser=CUDA\:$ctagslib:\
      :gtags_parser=D\:$ctagslib:\
      :gtags_parser=Diff\:$ctagslib:\
      :gtags_parser=DTD\:$ctagslib:\
      :gtags_parser=DTS\:$ctagslib:\
      :gtags_parser=DosBatch\:$ctagslib:\
      :gtags_parser=Eiffel\:$ctagslib:\
      :gtags_parser=elm\:$ctagslib:\
      :gtags_parser=Erlang\:$ctagslib:\
      :gtags_parser=Falcon\:$ctagslib:\
      :gtags_parser=Flex\:$ctagslib:\
      :gtags_parser=Fortran\:$ctagslib:\
      :gtags_parser=gdbinit\:$ctagslib:\
      :gtags_parser=Go\:$ctagslib:\
      :gtags_parser=HTML\:$ctagslib:\
      :gtags_parser=Iniconf\:$ctagslib:\
      :gtags_parser=ITcl\:$ctagslib:\
      :gtags_parser=Java\:$ctagslib:\
      :gtags_parser=JavaProperties\:$ctagslib:\
      :gtags_parser=JavaScript\:$ctagslib:\
      :gtags_parser=JSON\:$ctagslib:\
      :gtags_parser=LdScript\:$ctagslib:\
      :gtags_parser=Lisp\:$ctagslib:\
      :gtags_parser=Lua\:$ctagslib:\
      :gtags_parser=M4\:$ctagslib:\
      :gtags_parser=man\:$ctagslib:\
      :gtags_parser=Make\:$ctagslib:\
      :gtags_parser=MatLab\:$ctagslib:\
      :gtags_parser=Myrddin\:$ctagslib:\
      :gtags_parser=ObjectiveC\:$ctagslib:\
      :gtags_parser=OldC++\:$ctagslib:\
      :gtags_parser=OldC\:$ctagslib:\
      :gtags_parser=OCaml\:$ctagslib:\
      :gtags_parser=passwd\:$ctagslib:\
      :gtags_parser=Pascal\:$ctagslib:\
      :gtags_parser=Perl\:$ctagslib:\
      :gtags_parser=Perl6\:$ctagslib:\
      :gtags_parser=PHP\:$ctagslib:\
      :gtags_parser=pod\:$ctagslib:\
      :gtags_parser=Protobuf\:$ctagslib:\
      :gtags_parser=puppetManifest\:$ctagslib:\
      :gtags_parser=Python\:$ctagslib:\
      :gtags_parser=QemuHX\:$ctagslib:\
      :gtags_parser=R\:$ctagslib:\
      :gtags_parser=REXX\:$ctagslib:\
      :gtags_parser=Robot\:$ctagslib:\
      :gtags_parser=RpmSpec\:$ctagslib:\
      :gtags_parser=reStructuredText\:$ctagslib:\
      :gtags_parser=Ruby\:$ctagslib:\
      :gtags_parser=Rust\:$ctagslib:\
      :gtags_parser=Scheme\:$ctagslib:\
      :gtags_parser=Sh\:$ctagslib:\
      :gtags_parser=SLang\:$ctagslib:\
      :gtags_parser=SML\:$ctagslib:\
      :gtags_parser=SQL\:$ctagslib:\
      :gtags_parser=SystemdUnit\:$ctagslib:\
      :gtags_parser=Tcl\:$ctagslib:\
      :gtags_parser=Tex\:$ctagslib:\
      :gtags_parser=TTCN\:$ctagslib:\
      :gtags_parser=Vera\:$ctagslib:\
      :gtags_parser=Verilog\:$ctagslib:\
      :gtags_parser=SystemVerilog\:$ctagslib:\
      :gtags_parser=VHDL\:$ctagslib:\
      :gtags_parser=Vim\:$ctagslib:\
      :gtags_parser=WindRes\:$ctagslib:\
      :gtags_parser=YACC\:$ctagslib:\
      :gtags_parser=YumRepo\:$ctagslib:\
      :gtags_parser=Zephir\:$ctagslib:\
      :gtags_parser=DBusIntrospect\:$ctagslib:\
      :gtags_parser=Glade\:$ctagslib:\
      :gtags_parser=Maven2\:$ctagslib:\
      :gtags_parser=PlistXML\:$ctagslib:\
      :gtags_parser=RelaxNG\:$ctagslib:\
      :gtags_parser=SVG\:$ctagslib:\
      :gtags_parser=XSLT\:$ctagslib:
    #
    # Plug-in parser to use Pygments.
    #
    pygments-parser|Pygments plug-in parser:\
      :tc=common:\
      :ctagscom=/nix/store/qknwh7w0f3n59hywpdgg1hqd53mgrh6r-ctags-816/bin/ctags:\
      :pygmentslib=$libdir/gtags/pygments-parser.la:\
      :langmap=ABAP\:.abap:\
      :langmap=ANTLR\:.G.g:\
      :langmap=ActionScript3\:.as:\
      :langmap=Ada\:.adb.ads.ada:\
      :langmap=AppleScript\:.applescript:\
      :langmap=AspectJ\:.aj:\
      :langmap=Aspx-cs\:.aspx.asax.ascx.ashx.asmx.axd:\
      :langmap=Asymptote\:.asy:\
      :langmap=AutoIt\:.au3:\
      :langmap=Awk\:.awk.gawk.mawk:\
      :langmap=BUGS\:.bug:\
      :langmap=Bash\:.sh.ksh.bash.ebuild.eclass:\
      :langmap=Bat\:.bat.cmd:\
      :langmap=BlitzMax\:.bmx:\
      :langmap=Boo\:.boo:\
      :langmap=Bro\:.bro:\
      :langmap=C#\:.cs:\
      :langmap=C++\:.c++.cc.cp.cpp.cxx.h.h++.hh.hp.hpp.hxx.C.H:\
      :langmap=COBOLFree\:.cbl.CBL:\
      :langmap=COBOL\:.cob.COB.cpy.CPY:\
      :langmap=CUDA\:.cu.cuh:\
      :langmap=C\:.c.h:\
      :langmap=Ceylon\:.ceylon:\
      :langmap=Cfm\:.cfm.cfml.cfc:\
      :langmap=Clojure\:.clj:\
      :langmap=CoffeeScript\:.coffee:\
      :langmap=Common-Lisp\:.cl.lisp.el:\
      :langmap=Coq\:.v:\
      :langmap=Croc\:.croc:\
      :langmap=Csh\:.tcsh.csh:\
      :langmap=Cython\:.pyx.pxd.pxi:\
      :langmap=Dart\:.dart:\
      :langmap=Dg\:.dg:\
      :langmap=Duel\:.duel.jbst:\
      :langmap=Dylan\:.dylan.dyl.intr:\
      :langmap=ECL\:.ecl:\
      :langmap=EC\:.ec.eh:\
      :langmap=ERB\:.erb:\
      :langmap=Elixir\:.ex.exs:\
      :langmap=Erlang\:.erl.hrl.es.escript:\
      :langmap=Evoque\:.evoque:\
      :langmap=FSharp\:.fs.fsi:\
      :langmap=Factor\:.factor:\
      :langmap=Fancy\:.fy.fancypack:\
      :langmap=Fantom\:.fan:\
      :langmap=Felix\:.flx.flxh:\
      :langmap=Fortran\:.f.f90.F.F90:\
      :langmap=GAS\:.s.S:\
      :langmap=GLSL\:.vert.frag.geo:\
      :langmap=Genshi\:.kid:\
      :langmap=Gherkin\:.feature:\
      :langmap=Gnuplot\:.plot.plt:\
      :langmap=Go\:.go:\
      :langmap=GoodData-CL\:.gdc:\
      :langmap=Gosu\:.gs.gsx.gsp.vark:\
      :langmap=Groovy\:.groovy:\
      :langmap=Gst\:.gst:\
      :langmap=HaXe\:.hx:\
      :langmap=Haml\:.haml:\
      :langmap=Haskell\:.hs:\
      :langmap=Hxml\:.hxml:\
      :langmap=Hybris\:.hy.hyb:\
      :langmap=IDL\:.pro:\
      :langmap=Io\:.io:\
      :langmap=Ioke\:.ik:\
      :langmap=JAGS\:.jag.bug:\
      :langmap=Jade\:.jade:\
      :langmap=JavaScript\:.js:\
      :langmap=Java\:.java:\
      :langmap=Jsp\:.jsp:\
      :langmap=Julia\:.jl:\
      :langmap=Koka\:.kk.kki:\
      :langmap=Kotlin\:.kt:\
      :langmap=LLVM\:.ll:\
      :langmap=Lasso\:.lasso:\
      :langmap=Literate-Haskell\:.lhs:\
      :langmap=LiveScript\:.ls:\
      :langmap=Logos\:.x.xi.xm.xmi:\
      :langmap=Logtalk\:.lgt:\
      :langmap=Lua\:.lua.wlua:\
      :langmap=MOOCode\:.moo:\
      :langmap=MXML\:.mxml:\
      :langmap=Mako\:.mao:\
      :langmap=Mason\:.m.mhtml.mc.mi:\
      :langmap=Matlab\:.m:\
      :langmap=Modelica\:.mo:\
      :langmap=Modula2\:.mod:\
      :langmap=Monkey\:.monkey:\
      :langmap=MoonScript\:.moon:\
      :langmap=MuPAD\:.mu:\
      :langmap=Myghty\:.myt:\
      :langmap=NASM\:.asm.ASM:\
      :langmap=NSIS\:.nsi.nsh:\
      :langmap=Nemerle\:.n:\
      :langmap=NewLisp\:.lsp.nl:\
      :langmap=Newspeak\:.ns2:\
      :langmap=Nimrod\:.nim.nimrod:\
      :langmap=OCaml\:.ml.mli.mll.mly:\
      :langmap=Objective-C++\:.mm.hh:\
      :langmap=Objective-C\:.m.h:\
      :langmap=Objective-J\:.j:\
      :langmap=Octave\:.m:\
      :langmap=Ooc\:.ooc:\
      :langmap=Opa\:.opa:\
      :langmap=OpenEdge\:.p.cls:\
      :langmap=PHP\:.php.php3.phtml:\
      :langmap=Pascal\:.pas:\
      :langmap=Perl\:.pl.pm:\
      :langmap=PostScript\:.ps.eps:\
      :langmap=PowerShell\:.ps1:\
      :langmap=Prolog\:.prolog.pro.pl:\
      :langmap=Python\:.py.pyw.sc.tac.sage:\
      :langmap=QML\:.qml:\
      :langmap=REBOL\:.r.r3:\
      :langmap=RHTML\:.rhtml:\
      :langmap=Racket\:.rkt.rktl:\
      :langmap=Ragel\:.rl:\
      :langmap=Redcode\:.cw:\
      :langmap=RobotFramework\:.robot:\
      :langmap=Ruby\:.rb.rbw.rake.gemspec.rbx.duby:\
      :langmap=Rust\:.rs.rc:\
      :langmap=S\:.S.R:\
      :langmap=Scala\:.scala:\
      :langmap=Scaml\:.scaml:\
      :langmap=Scheme\:.scm.ss:\
      :langmap=Scilab\:.sci.sce.tst:\
      :langmap=Smalltalk\:.st:\
      :langmap=Smarty\:.tpl:\
      :langmap=Sml\:.sml.sig.fun:\
      :langmap=Snobol\:.snobol:\
      :langmap=SourcePawn\:.sp:\
      :langmap=Spitfire\:.spt:\
      :langmap=Ssp\:.ssp:\
      :langmap=Stan\:.stan:\
      :langmap=SystemVerilog\:.sv.svh:\
      :langmap=Tcl\:.tcl:\
      :langmap=TeX\:.tex.aux.toc:\
      :langmap=Tea\:.tea:\
      :langmap=Treetop\:.treetop.tt:\
      :langmap=TypeScript\:.ts:\
      :langmap=UrbiScript\:.u:\
      :langmap=VB.net\:.vb.bas:\
      :langmap=VGL\:.rpf:\
      :langmap=Vala\:.vala.vapi:\
      :langmap=Velocity\:.vm.fhtml:\
      :langmap=Verilog\:.v:\
      :langmap=Vhdl\:.vhdl.vhd:\
      :langmap=Vim\:.vim:\
      :langmap=XBase\:.PRG.prg:\
      :langmap=XQuery\:.xqy.xquery.xq.xql.xqm:\
      :langmap=XSLT\:.xsl.xslt.xpl:\
      :langmap=Xtend\:.xtend:\
      :gtags_parser=ABAP\:$pygmentslib:\
      :gtags_parser=ANTLR\:$pygmentslib:\
      :gtags_parser=ActionScript3\:$pygmentslib:\
      :gtags_parser=Ada\:$pygmentslib:\
      :gtags_parser=AppleScript\:$pygmentslib:\
      :gtags_parser=AspectJ\:$pygmentslib:\
      :gtags_parser=Aspx-cs\:$pygmentslib:\
      :gtags_parser=Asymptote\:$pygmentslib:\
      :gtags_parser=AutoIt\:$pygmentslib:\
      :gtags_parser=Awk\:$pygmentslib:\
      :gtags_parser=BUGS\:$pygmentslib:\
      :gtags_parser=Bash\:$pygmentslib:\
      :gtags_parser=Bat\:$pygmentslib:\
      :gtags_parser=BlitzMax\:$pygmentslib:\
      :gtags_parser=Boo\:$pygmentslib:\
      :gtags_parser=Bro\:$pygmentslib:\
      :gtags_parser=C#\:$pygmentslib:\
      :gtags_parser=C++\:$pygmentslib:\
      :gtags_parser=COBOLFree\:$pygmentslib:\
      :gtags_parser=COBOL\:$pygmentslib:\
      :gtags_parser=CUDA\:$pygmentslib:\
      :gtags_parser=C\:$pygmentslib:\
      :gtags_parser=Ceylon\:$pygmentslib:\
      :gtags_parser=Cfm\:$pygmentslib:\
      :gtags_parser=Clojure\:$pygmentslib:\
      :gtags_parser=CoffeeScript\:$pygmentslib:\
      :gtags_parser=Common-Lisp\:$pygmentslib:\
      :gtags_parser=Coq\:$pygmentslib:\
      :gtags_parser=Croc\:$pygmentslib:\
      :gtags_parser=Csh\:$pygmentslib:\
      :gtags_parser=Cython\:$pygmentslib:\
      :gtags_parser=Dart\:$pygmentslib:\
      :gtags_parser=Dg\:$pygmentslib:\
      :gtags_parser=Duel\:$pygmentslib:\
      :gtags_parser=Dylan\:$pygmentslib:\
      :gtags_parser=ECL\:$pygmentslib:\
      :gtags_parser=EC\:$pygmentslib:\
      :gtags_parser=ERB\:$pygmentslib:\
      :gtags_parser=Elixir\:$pygmentslib:\
      :gtags_parser=Erlang\:$pygmentslib:\
      :gtags_parser=Evoque\:$pygmentslib:\
      :gtags_parser=FSharp\:$pygmentslib:\
      :gtags_parser=Factor\:$pygmentslib:\
      :gtags_parser=Fancy\:$pygmentslib:\
      :gtags_parser=Fantom\:$pygmentslib:\
      :gtags_parser=Felix\:$pygmentslib:\
      :gtags_parser=Fortran\:$pygmentslib:\
      :gtags_parser=GAS\:$pygmentslib:\
      :gtags_parser=GLSL\:$pygmentslib:\
      :gtags_parser=Genshi\:$pygmentslib:\
      :gtags_parser=Gherkin\:$pygmentslib:\
      :gtags_parser=Gnuplot\:$pygmentslib:\
      :gtags_parser=Go\:$pygmentslib:\
      :gtags_parser=GoodData-CL\:$pygmentslib:\
      :gtags_parser=Gosu\:$pygmentslib:\
      :gtags_parser=Groovy\:$pygmentslib:\
      :gtags_parser=Gst\:$pygmentslib:\
      :gtags_parser=HaXe\:$pygmentslib:\
      :gtags_parser=Haml\:$pygmentslib:\
      :gtags_parser=Haskell\:$pygmentslib:\
      :gtags_parser=Hxml\:$pygmentslib:\
      :gtags_parser=Hybris\:$pygmentslib:\
      :gtags_parser=IDL\:$pygmentslib:\
      :gtags_parser=Io\:$pygmentslib:\
      :gtags_parser=Ioke\:$pygmentslib:\
      :gtags_parser=JAGS\:$pygmentslib:\
      :gtags_parser=Jade\:$pygmentslib:\
      :gtags_parser=JavaScript\:$pygmentslib:\
      :gtags_parser=Java\:$pygmentslib:\
      :gtags_parser=Jsp\:$pygmentslib:\
      :gtags_parser=Julia\:$pygmentslib:\
      :gtags_parser=Koka\:$pygmentslib:\
      :gtags_parser=Kotlin\:$pygmentslib:\
      :gtags_parser=LLVM\:$pygmentslib:\
      :gtags_parser=Lasso\:$pygmentslib:\
      :gtags_parser=Literate-Haskell\:$pygmentslib:\
      :gtags_parser=LiveScript\:$pygmentslib:\
      :gtags_parser=Logos\:$pygmentslib:\
      :gtags_parser=Logtalk\:$pygmentslib:\
      :gtags_parser=Lua\:$pygmentslib:\
      :gtags_parser=MAQL\:$pygmentslib:\
      :gtags_parser=MOOCode\:$pygmentslib:\
      :gtags_parser=MXML\:$pygmentslib:\
      :gtags_parser=Mako\:$pygmentslib:\
      :gtags_parser=Mason\:$pygmentslib:\
      :gtags_parser=Matlab\:$pygmentslib:\
      :gtags_parser=MiniD\:$pygmentslib:\
      :gtags_parser=Modelica\:$pygmentslib:\
      :gtags_parser=Modula2\:$pygmentslib:\
      :gtags_parser=Monkey\:$pygmentslib:\
      :gtags_parser=MoonScript\:$pygmentslib:\
      :gtags_parser=MuPAD\:$pygmentslib:\
      :gtags_parser=Myghty\:$pygmentslib:\
      :gtags_parser=NASM\:$pygmentslib:\
      :gtags_parser=NSIS\:$pygmentslib:\
      :gtags_parser=Nemerle\:$pygmentslib:\
      :gtags_parser=NewLisp\:$pygmentslib:\
      :gtags_parser=Newspeak\:$pygmentslib:\
      :gtags_parser=Nimrod\:$pygmentslib:\
      :gtags_parser=OCaml\:$pygmentslib:\
      :gtags_parser=Objective-C++\:$pygmentslib:\
      :gtags_parser=Objective-C\:$pygmentslib:\
      :gtags_parser=Objective-J\:$pygmentslib:\
      :gtags_parser=Octave\:$pygmentslib:\
      :gtags_parser=Ooc\:$pygmentslib:\
      :gtags_parser=Opa\:$pygmentslib:\
      :gtags_parser=OpenEdge\:$pygmentslib:\
      :gtags_parser=PHP\:$pygmentslib:\
      :gtags_parser=Pascal\:$pygmentslib:\
      :gtags_parser=Perl\:$pygmentslib:\
      :gtags_parser=PostScript\:$pygmentslib:\
      :gtags_parser=PowerShell\:$pygmentslib:\
      :gtags_parser=Prolog\:$pygmentslib:\
      :gtags_parser=Python\:$pygmentslib:\
      :gtags_parser=QML\:$pygmentslib:\
      :gtags_parser=REBOL\:$pygmentslib:\
      :gtags_parser=RHTML\:$pygmentslib:\
      :gtags_parser=Racket\:$pygmentslib:\
      :gtags_parser=Ragel\:$pygmentslib:\
      :gtags_parser=Redcode\:$pygmentslib:\
      :gtags_parser=RobotFramework\:$pygmentslib:\
      :gtags_parser=Ruby\:$pygmentslib:\
      :gtags_parser=Rust\:$pygmentslib:\
      :gtags_parser=S\:$pygmentslib:\
      :gtags_parser=Scala\:$pygmentslib:\
      :gtags_parser=Scaml\:$pygmentslib:\
      :gtags_parser=Scheme\:$pygmentslib:\
      :gtags_parser=Scilab\:$pygmentslib:\
      :gtags_parser=Smalltalk\:$pygmentslib:\
      :gtags_parser=Smarty\:$pygmentslib:\
      :gtags_parser=Sml\:$pygmentslib:\
      :gtags_parser=Snobol\:$pygmentslib:\
      :gtags_parser=SourcePawn\:$pygmentslib:\
      :gtags_parser=Spitfire\:$pygmentslib:\
      :gtags_parser=Ssp\:$pygmentslib:\
      :gtags_parser=Stan\:$pygmentslib:\
      :gtags_parser=SystemVerilog\:$pygmentslib:\
      :gtags_parser=Tcl\:$pygmentslib:\
      :gtags_parser=TeX\:$pygmentslib:\
      :gtags_parser=Tea\:$pygmentslib:\
      :gtags_parser=Treetop\:$pygmentslib:\
      :gtags_parser=TypeScript\:$pygmentslib:\
      :gtags_parser=UrbiScript\:$pygmentslib:\
      :gtags_parser=VB.net\:$pygmentslib:\
      :gtags_parser=VGL\:$pygmentslib:\
      :gtags_parser=Vala\:$pygmentslib:\
      :gtags_parser=Velocity\:$pygmentslib:\
      :gtags_parser=Verilog\:$pygmentslib:\
      :gtags_parser=Vhdl\:$pygmentslib:\
      :gtags_parser=Vim\:$pygmentslib:\
      :gtags_parser=XBase\:$pygmentslib:\
      :gtags_parser=XQuery\:$pygmentslib:\
      :gtags_parser=XSLT\:$pygmentslib:\
      :gtags_parser=Xtend\:$pygmentslib:
    #
    # Drupal configuration.
    #
    drupal|Drupal content management platform:\
      :tc=common:\
      :langmap=php\:.php.module.inc.profile.install.test:
    #---------------------------------------------------------------------
    # Configuration for htags(1)
    #---------------------------------------------------------------------
    htags:\
      ::
  '';
  environment.etc."vimrc".text = ''
    set nonumber
    set relativenumber
    colorscheme default
  '';
  system.activationScripts.extraUserActivation.text = ''
    ln -sfn /etc/static/gitconfig $HOME/.gitconfig
    ln -sfn /etc/static/gitignore $HOME/.gitignore
    ln -sfn /etc/static/ctags $HOME/.ctags
    ln -sfn /etc/static/vimrc $HOME/.vimrc
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
    #kubernetes
    kk = "kubectl config use-context";
    kc = "kubectl config current-context";

    # nix
    ne = "nix-env";
    neg = "ne -qaP | grep";
    ns = "nix-shell";
    nr = (if pkgs.stdenv.isDarwin then "darwin-rebuild" else "sudo nixos-rebuild");

    dotfiles = "sh $HOME/wrk/dotfiles/result/bin/dotfiles";
    pass = "${pkgs.gopass}/bin/gopass";
    zenity = "${pkgs.qarma}/bin/qarma";
    vim = "${pkgs.emacs}/bin/emacsclient -n";
    r = "${pkgs.ranger}/bin/ranger";
    grep = "${pkgs.ripgrep}/bin/rg";
    alacritty = "${pkgs.alacritty}/bin/alacritty -e ${pkgs.tmux}/bin/tmux -2 new-session -A -s main";
    qmk = ''${pkgs.scripts}/bin/qmk $HOME/wrk/qmk_firmware/layouts/community/ortho_4x12/peel/keymap.c'';
  };
  # environment.interactiveShellInit = ''
  #   eval "$(${pkgs.fasd}/bin/fasd --init auto)"
  # '';
  # programs.bash.enableCompletion = true;
  programs.tmux.enable = true;
}
