{ colors }:
with colors;
''
URxvt.depth:                32
URxvt.geometry:             90x30
URxvt.transparent:          false
URxvt.fading:               0
! URxvt.urgentOnBell:         true
! URxvt.visualBell:           true
URxvt.loginShell:           true
URxvt.saveLines:            5000
URxvt.internalBorder:       7
URxvt.borderColor:          ${background}
URxvt.lineSpace:            0

! Fonts
URxvt.allow_bold:           true
/* URxvt.font:                 -*-terminus-medium-r-normal-*-12-120-72-72-c-60-iso8859-1 */
!!URxvt.font: xft:Droid Sans Mono for Powerline:size=10:antialias=true
URxvt.font: xft:PragmataPro:pixelsize=28:antialias=true
!!URxvt*font: xft:Monospace:pixelsize=14
!!URxvt*boldFont: xft:Monospace:pixelsize=14

! Fix font space
URxvt*letterSpace: 0

! Scrollbar
URxvt.scrollStyle:          rxvt
URxvt.scrollBar:            false

! Perl extensions
URxvt.perl-ext-common:      default,matcher
URxvt.matcher.button:       1
URxvt.urlLauncher:          firefox

! Cursor
URxvt.cursorBlink:          false
URxvt.cursorColor:          ${cursorColor}
URxvt.cursorUnderline:      false

! Pointer
URxvt.pointerBlank:         true

*background: ${background}
*foreground: ${foreground}
!*foreground: #657b83
!!*fading: 40
!!*fadeColor: #002b36
!!*cursorColor: #93a1a1
!!*pointerColorBackground: #586e75
!!*pointerColorForeground: #93a1a1

!! black dark/light
*color0: ${color0}
*color8: ${color8}

!! red dark/light
*color1: ${color1}
*color9: ${color9}

!! green dark/light
*color2: ${color2}
*color10: ${color10}

!! yellow dark/light
*color3: ${color3}
*color11: ${color11}

!! blue dark/light
*color4: ${color4}
*color12: ${color12}

!! magenta dark/light
*color5: ${color5}
*color13: ${color13}

!! cyan dark/light
*color6: ${color6}
*color14: ${color14}

!! white dark/light
*color7: ${color7}
*color15: ${color15}

!! bold color
!!URxvt.colorDB: #A85C28
!!URxvt.colorIT: #A85C28
!!URxvt.underlineColor: #A85C28
!!URxvt.highlightColor: #A85C28
!!URxvt.highlightTextColor: #B5AF87
''
