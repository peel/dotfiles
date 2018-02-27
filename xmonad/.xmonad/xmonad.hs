{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses #-}
---------------------------------------------------------------------------
--                                                                       --
--     _|      _|  _|      _|                                      _|    --
--       _|  _|    _|_|  _|_|    _|_|    _|_|_|      _|_|_|    _|_|_|    --
--         _|      _|  _|  _|  _|    _|  _|    _|  _|    _|  _|    _|    --
--       _|  _|    _|      _|  _|    _|  _|    _|  _|    _|  _|    _|    --
--     _|      _|  _|      _|    _|_|    _|    _|    _|_|_|    _|_|_|    --
--                                                                       --
---------------------------------------------------------------------------
-- Ethan Schoonover <es@ethanschoonover.com> @ethanschoonover            --
-- https://github.com/altercation                                        --
---------------------------------------------------------------------------
-- current as of XMonad 0.12

------------------------------------------------------------------------}}}
-- TODO                                                                 {{{
---------------------------------------------------------------------------
{-|

 GENERAL
 
 * look into X.H.Scripts -- there are things I want to run at startup, for example
 * X.U.SpawnNamedPipe? xmobars. multiple screens.
 * X.U.WindowState
 * review XMonad.ManageHook https://hackage.haskell.org/package/xmonad-0.12/docs/XMonad-ManageHook.html
 * ? X.A.LinkWorkspaces
 * ? X.A.Search
 * ? X.A.ShowText
 * ? X.A.SimpleDate
 * ? X.A.Warp
 * ? X.A.WindowBringer
 * ? X.A.WorkspaceCursors
 * ? XMonad.Hooks.Minimize / X.H.Minimize or XMonad.Layout.Hidden
 * ? X.L.avoidFloats - tried and couldn't get it to work immediately but seemed interesting
 * XMonad.Hooks.DynamicBars looks useful

 NON XMONAD SPECIFIC

 * fix unplug events that throw false battery warning
 * switch to urxvt with dynamic font sizing?
 * screen locker
 * audio tweaking ... volume working in all cases? output selected intelligently
 * ssd cloning via btrfs
 * go through sections of https://wiki.archlinux.org/index.php/List_of_applications and identify category selections, adding them to personal wiki
 
 ACTIVE

 * try out XMonad.Layout.Hidden
 * check out https://github.com/paul-axe/dotfiles/blob/master/.xmonad/xmonad.hs for dual xmobar?
 * would be nice to have a couple project spaces that are sys:1 sys:2 etc and related keybindings
 * use toggle float for M-t and make M-S-t sinkAll or toggle float for all
 * Refine bindings. consider greater use of submaps
 * work on helper scripts in general (vol, etc.)
 * xdb / localectl in lieu of xmodmap
 * test hybrid graphics again?
 * power (test tlp again? need way to see if it's doing a whole lot of good, or should I just use manual options... either way nvidia is power hungry)
 * screensaver and screen stuff, caffeine
 * check on avoidmaster for float issues https://wiki.haskell.org/Xmonad/Frequently_asked_questions
 * consider inserting chrome above instead of below on stack
 * either an M-s d style submap for system operations, or top level M4-d M4-s style bindings
 * on project space default action, I'd like to spawn a couple terminals on SYS and group them immediately, then spawn another terminal or browser. how?

 DEFER (should do but uncertain how to solve after initial cursory review, so will defer till have more time to research)

 * just like toggleWS' from CycleWS, it would be nice to make a custom prev/nextWS' that would skip NSP
 * could make a new set of PerScreen width layouts for "small screens" (1280 and under) (non critical...)
 * look intot X.*.PositionStore* as well as whatever the other method of retaining float pos was
 * quickly swapping two windows between master and slave works nicely. I get a little of this with promote, but I'm
   sure there is a more comprehensive solution I could implement (cycle windows / recent windows?)
 ! Want to be able to spawn a new window directly into a sublayout, not
   spawn/merge as I'm doing now (this would be a SIGNIFICANT improvement)
 ! add tab/alt-tab cycling through windows
 * add a shutdown hook to spin down tray/other processes that throw unnecessary errors into xorg
 * add withall to send all windows to different workspace
 * look fully into resizing current layout including vertical on 3Col
 * make focused window master automatically on floating
 * see if there is a way to maintain tiled focus post toggle of scratchpad (cf X.L.TrackFloating)
 * move NSP windows that are tiled into workspace AT END or AS MASTER depending on management
 * https://github.com/pjones/xmonadrc has a focus-follows in the tiled layer only
   also has some dynamic project helper functions
 * if not using dynamic workspaces (just projects) then remove the dynamic workspaces functions from window shifting
 * consider IfMax for further dynamic layout properties
 * revisit mouse resizing of windows in tiled layouts (nice to have not crit)
 * any utility in XMonad-Hooks-ServerMode (tested briefly, couldn't get it working properly)
 * work on my handling of x selection for utility functions ... timer/delay issue?

 DONE 

 * DynamicWorkspaces ... will DynamicProjects replace it entirely? Do I not need it
 * keybindings for unmerge are weird... sublayout not great for what might be a common op
   could do M-u and M-S-u for mergeall
 * capture f11 and pass it along to window, then shift window (or come up with other way to redraw boundaries)
 * set conditional key bindings depending on layout for tabs view
   (other pseudo-conditional bindings are handled with a trymessage construct)
 * add in full tabbed layout in standard sequence?
 * fix scratchpad float position - more or less ok now
 * test alternate sublayout style in order to explode current view
 * XMonad.Hooks.DynamicProperty - could be used for Chrome windows that pop up
   if not already assigned a custom class via flags
 * change keybinding for cycling through tabs quickly.... this should be "top level" mod+something
 * would be nice to have fullscreen work the way I had it where I could fit it in a window as desired
 * make a partial full screen that respects struts
 * X.H.InsertPosition ... do I want to use this for different spawn location? can I use
   it for only certain windows?
 * XMonad-Hooks-ToggleHook
 ! hotplug monitor scripts
 * fix alert styles
 * dealing with screens/workspaces (binding to move/shift to workspace)
 ! XMonad-Actions-Navigation2D has a lot of features I'm not yet using.
   E.g. screen related
   Review the documentation and consider adding.

 TESTED/REJECTED/WONTFIX

 * consider switching to X.L.SimpleFloat + SimpleDecoration for titlebars
 * planekeys? also the new ws project thing i read in change log. also link workspaces
   RESULT: for now just using projects the prompt to move around ws
 * revisit whether my current use of top level tabbed layout is confusing or best case
   - does it make sense?
   - do I actually switch to it a lot? would I?
   - maybe I could just use Tabs as an orphan layout that I jump to
   RESULT: i'm ocnvinced the current top level tabs which is always in the
   layout cycle is, if not optimal, the best I'm going to get for now

 * ? X.A.RotSlaves - not much use since I just use nav2D

 NON XMONAD SPECIFIC TODO

 * check if unclutter is being launched and if the new version is crashing

 -}

------------------------------------------------------------------------}}}
-- Modules                                                              {{{
---------------------------------------------------------------------------
--import Control.Monad (liftM2)             -- myManageHookShift
import Control.Monad (liftM, liftM2, join)  -- myManageHookShift
import Data.List
import qualified Data.Map as M
import Data.Monoid
import System.Exit
import System.IO                            -- for xmonbar
import System.Posix.Process(executeFile)

import XMonad hiding ( (|||) )              -- ||| from X.L.LayoutCombinators
import qualified XMonad.StackSet as W       -- myManageHookShift

import XMonad.Actions.Commands
import XMonad.Actions.ConditionalKeys       -- bindings per workspace or layout
import qualified XMonad.Actions.ConstrainedResize as Sqr
import XMonad.Actions.CopyWindow            -- like cylons, except x windows
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicProjects
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.FloatSnap
import XMonad.Actions.MessageFeedback       -- pseudo conditional key bindings
import XMonad.Actions.Navigation2D
import XMonad.Actions.Promote               -- promote window to master
import XMonad.Actions.SinkAll
import XMonad.Actions.SpawnOn
import XMonad.Actions.Volume
import XMonad.Actions.WindowGo
import XMonad.Actions.WithAll               -- action all the things

import XMonad.Hooks.DynamicLog              -- for xmobar
import XMonad.Hooks.DynamicProperty         -- 0.12 broken; works with github version
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks             -- avoid xmobar
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook

--import XMonad.Layout hiding ( (|||) )       -- ||| from X.L.LayoutCombinators
import XMonad.Layout.Accordion
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.BorderResize
import XMonad.Layout.Column
import XMonad.Layout.Combo
import XMonad.Layout.ComboP
import XMonad.Layout.DecorationMadness      -- testing alternative accordion styles
import XMonad.Layout.Dishes
import XMonad.Layout.DragPane
import XMonad.Layout.Drawer
import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import XMonad.Layout.Hidden
import XMonad.Layout.LayoutBuilder
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LayoutScreens
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.OneBig
import XMonad.Layout.PerScreen              -- Check screen width & adjust layouts
import XMonad.Layout.PerWorkspace           -- Configure layouts on a per-workspace
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile          -- Resizable Horizontal border
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing                -- this makes smart space around windows
import XMonad.Layout.StackTile
import XMonad.Layout.SubLayouts             -- Layouts inside windows. Excellent.
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ToggleLayouts          -- Full window at any time
import XMonad.Layout.TrackFloating
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation

import XMonad.Prompt                        -- to get my old key bindings working
import XMonad.Prompt.ConfirmPrompt          -- don't just hard quit

import XMonad.Util.Cursor
import XMonad.Util.EZConfig                 -- removeKeys, additionalKeys
import XMonad.Util.Loggers
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows
import XMonad.Util.Paste as P               -- testing
import XMonad.Util.Run                      -- for spawnPipe and hPutStrLn
import XMonad.Util.SpawnOnce
import XMonad.Util.WorkspaceCompare         -- custom WS functions filtering NSP
import XMonad.Util.XSelection


-- experimenting with tripane
import XMonad.Layout.Decoration
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.Maximize
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders

-- taffybar specific
-- import System.Taffybar.Hooks.PagerHints (pagerHints)
-- to demo and comment out or remove
-- import XMonad.Layout.Master -- used to test a dynamic layout. worked, but will remove in lieu of sublayouts
-- import XMonad.Actions.CycleSelectedLayouts -- nice but doesn't work well with sublayouts
-- import XMonad.Actions.Plane
-- import XMonad.Layout.IndependentScreens
-- import XMonad.Util.Timer
-- recent windows from cycle windows -- couldn't get it working on quick try: revisit this
-- import XMonad.Actions.CycleWindows
-- testing -- not a lot of value added, or am I missing something
-- import XMonad.Hooks.Place
----
-- following for the combocombo test from
-- http://xmonad.org/xmonad-docs/xmonad-contrib/src/XMonad-Config-Droundy.html
-- import XMonad.Layout.Square ( Square(Square) )
-- import XMonad.Layout.BoringWindows
-- import XMonad.Layout.Grid
----
-- import XMonad.Layout.SimpleDecoration
-- testing -- couldn't get this to work
-- import XMonad.Layout.TrackFloating
-- testing
-- import XMonad.Hooks.ServerMode
-- import XMonad.Actions.Commands 
-- import Control.Concurrent (threadDelay)

------------------------------------------------------------------------}}}
-- Main                                                                 {{{
---------------------------------------------------------------------------

main = do

    xmproc <- spawnPipe myStatusBar

    -- for independent screens
    -- nScreens <- countScreens

    -- for taffybar, add pagerHints below

    xmonad 
        $ dynamicProjects projects
        $ withNavigation2DConfig myNav2DConf
        -- $ withUrgencyHook NoUrgencyHook
        $ withUrgencyHook LibNotifyUrgencyHook
        $ ewmh
        $ addDescrKeys' ((myModMask, xK_F1), showKeybindings) myKeys
        $ myConfig xmproc

myConfig p = def
        { borderWidth        = border
        , clickJustFocuses   = myClickJustFocuses
        , focusFollowsMouse  = myFocusFollowsMouse
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , manageHook         = myManageHook
        , handleEventHook    = myHandleEventHook
        , layoutHook         = myLayoutHook
        , logHook            = myLogHook p
        , modMask            = myModMask
        , mouseBindings      = myMouseBindings
        , startupHook        = myStartupHook
        , terminal           = myTerminal
        , workspaces         = myWorkspaces
        }


------------------------------------------------------------------------}}}
-- Workspaces                                                           {{{
---------------------------------------------------------------------------

wsAV    = "AV"
wsBSA   = "BSA"
wsCOM   = "COM"
wsDOM   = "DOM"
wsDMO   = "DMO"
wsFLOAT = "FLT"
wsGEN   = "GEN"
wsGCC   = "GCC"
wsMON   = "MON"
wsOSS   = "OSS"
wsRAD   = "RAD"
wsRW    = "RW"
wsSYS   = "SYS"
wsTMP   = "TMP"
wsVIX   = "VIX"
wsWRK   = "WRK"
wsWRK2  = "WRK:2"
wsGGC   = "GGC"

-- myWorkspaces = map show [1..9]
myWorkspaces = [wsGEN, wsWRK, wsWRK2, wsSYS, wsMON, wsFLOAT, wsRW, wsTMP]

projects :: [Project]
projects =

    [ Project   { projectName       = wsGEN
                , projectDirectory  = "~/"
                , projectStartHook  = Nothing
                }

    , Project   { projectName       = wsSYS
                , projectDirectory  = "~/"
                , projectStartHook  = Just $ do spawnOn wsSYS myTerminal
                                                spawnOn wsSYS myTerminal
                                                spawnOn wsSYS myTerminal
                }

    , Project   { projectName       = wsDMO
                , projectDirectory  = "~/"
                -- , projectStartHook  = Just $ do spawn "/usr/lib/xscreensaver/binaryring"
                , projectStartHook  = Just $ do spawn "/usr/lib/xscreensaver/spheremonics"
                                                runInTerm "-name top" "top"
                                                runInTerm "-name top" "htop"
                                                runInTerm "-name glances" "glances"
                                                spawn "/usr/lib/xscreensaver/cubicgrid"
                                                spawn "/usr/lib/xscreensaver/surfaces"
                }

    , Project   { projectName       = wsVIX
                , projectDirectory  = "~/.xmonad"
                , projectStartHook  = Just $ do runInTerm "-name vix" "vim ~/.xmonad/xmonad.hs"
                                                spawnOn wsVIX myTerminal
                                                spawnOn wsVIX myTerminal
                }

    , Project   { projectName       = wsMON
                , projectDirectory  = "~/"
                , projectStartHook  = Just $ do runInTerm "-name glances" "glances"
                }

    , Project   { projectName       = wsWRK
                , projectDirectory  = "~/wrk"
                , projectStartHook  = Just $ do spawnOn wsWRK myTerminal
                                                spawnOn wsWRK myBrowser
                }

    , Project   { projectName       = wsRAD
                , projectDirectory  = "~/"
                , projectStartHook  = Just $ do spawn myBrowser
                }

    , Project   { projectName       = wsTMP
                , projectDirectory  = "~/"
                -- , projectStartHook  = Just $ do spawn $ myBrowser ++ " https://mail.google.com/mail/u/0/#inbox/1599e6883149eeac"
                , projectStartHook  = Just $ do return ()
                }
    ]

------------------------------------------------------------------------}}}
-- Applications                                                         {{{
---------------------------------------------------------------------------

-- | Uses supplied function to decide which action to run depending on current workspace name.

--myTerminal          = "terminator"
--myTerminalClass     = "Terminator"
myTerminal          = "urxvt"
myAltTerminal       = "cool-retro-term"
myBrowser           = "browser" -- chrome with WS profile dirs
myBrowserClass      = "firefox"
myStatusBar         = "xmobar -x0 $HOME/.xmonad/xmobar.conf"
--myLauncher          = "dmenu_run"
--myLauncher          = "rofi -matching fuzzy -show run"
myLauncher          = "rofi -matching fuzzy -modi combi -show combi -combi-modi run,drun"
myNetwork           = "$HOME/bin/wm/rofi-wifi-menu/rofi-wifi-menu.sh"
myEmoji             = "rofi-emoji"
myPasswordManager   = "gopass ls --flat | rofi -dmenu | xargs --no-run-if-empty gopass show -c"


-- I'm using a custom browser launching script (see myBrowser above) that
-- is workspace aware. It launches an instance of Chrome that is unique
-- on specific workspaces. Thus on "GEN" workspace I use my "normal"
-- browser profile, while on "WRK" I use a different profile. This is
-- roughly equivalent to using Chrome's built in profiles, but has the
-- benefit of launching immediately with the correct profile.
--
-- In addition to this, I use per workspace bindings to toggle Hangouts
-- chat windows and Trello windows based on whether I'm on, for example,
-- my personal or work workspace.
--
-- This is particularly useful for Trello since I can launch a project
-- related Trello "app" instance on a project workspace.
--
-- This system utilizes:
-- * my workspace aware browser script
-- * X.U.NamedScratchPads
-- * bindOn via X.A.PerWorkspaceKeys (NO... now using ConditionalKeys custom module)
-- * bindOn via X.A.ConditionalKeys

-- TODO: change this to a lookup for all workspaces
hangoutsCommand     = myBrowser ++ " --app-id=knipolnnllmklapflnccelgolnpehhpl"
hangoutsTitle     = "Google Hangouts - es@peelschoonover.com"
hangoutsPrefix      = "Google Hangouts"
hangoutsResource    = "crx_nckgahadagoaajjgafhacjanaoiihapd"
isHangoutsFor s     = (className =? myBrowserClass
                      <&&> fmap (isPrefixOf hangoutsPrefix) title
                      <&&> fmap (isInfixOf s) title)
isPersonalHangouts  = isHangoutsFor "peel"
isWorkHangouts      = isHangoutsFor "limanowski"

-- TODO: change this to a lookup for all workspaces
trelloCommand       = "dex $HOME/.local/share/applications/Trello.desktop"
trelloWorkCommand   = "dex $HOME/.local/share/applications/TrelloWork.desktop"
trelloWork2Command  = "dex $HOME/.local/share/applications/TrelloWork2.desktop"
trelloInfix         = "Trello"
trelloResource      = "crx_jijnmpkkfkjaihbhffejemnpbbglahim"
trelloWorkResource  = "crx_fkbbihpadkgbnhphndjgblgelahbiede"
trelloWork2Resource = "crx_bgemgoheeofmogacohnlmpldjlogegoh"
isTrello            = (resource =? trelloResource)
isTrelloWork        = (resource =? trelloWorkResource)
isTrelloWork2       = (resource =? trelloWork2Resource)

googleMusicCommand  = "dex $HOME/.local/share/applications/Music.desktop"
googleMusicInfix    = "Google Play Music"
googleMusicResource = "crx_ioljlgoncmlkbcepmminebblkddfjofl"
isGoogleMusic       = (resource =? googleMusicResource)

plexCommand         = "dex $HOME/.local/share/applications/Plex.desktop"
plexInfix           = "Plex"
plexResource        = "crx_fpniocchabmgenibceglhnfeimmdhdfm"
isPlex              = (resource =? plexResource)

isConsole           = (className =? "Terminator")
                    <&&> (stringProperty "WM_WINDOW_ROLE" =? "Scratchpad")
myConsole           = "terminator -T console -p console --role=Scratchpad"

scratchpads =
    [   (NS "hangoutsPersonal"  hangoutsCommand isPersonalHangouts defaultFloating)
    ,   (NS "hangoutsWork"  hangoutsCommand isWorkHangouts defaultFloating)
    ,   (NS "trello"  trelloCommand isTrello nonFloating)
    ,   (NS "trelloWork"  trelloWorkCommand isTrelloWork nonFloating)
    ,   (NS "googleMusic"  googleMusicCommand isGoogleMusic nonFloating)
    ,   (NS "plex"  plexCommand isPlex defaultFloating)
    ,   (NS "console"  myConsole isConsole nonFloating)
    ,   (NS "xawtv" "xawtv" (resource =? "xawtv") (customFloating $ W.RationalRect (2/3) (1/6) (1/5) (1/3)) )
    ] 

------------------------------------------------------------------------}}}
-- Theme                                                                {{{
---------------------------------------------------------------------------

myFocusFollowsMouse  = False
myClickJustFocuses   = True

base03  = "#002b36"
base02  = "#073642"
base01  = "#586e75"
base00  = "#657b83"
base0   = "#839496"
base1   = "#93a1a1"
base2   = "#eee8d5"
base3   = "#fdf6e3"
yellow  = "#b58900"
orange  = "#cb4b16"
red     = "#dc322f"
magenta = "#d33682"
violet  = "#6c71c4"
blue    = "#268bd2"
cyan    = "#2aa198"
green       = "#859900"

-- sizes
gap         = 10
topbar      = 10
border      = 0
prompt      = 20
status      = 20

myNormalBorderColor     = "#000000"
myFocusedBorderColor    = active

active      = blue
activeWarn  = red
inactive    = base02
focusColor  = blue
unfocusColor = base02

myFont      = "-*-pragmatapro-*-*-*-*-160-*-*-*-*-*-*"
myBigFont   = "xft:PragmataPro:size=18:antialias=true"
myWideFont  = "xft:PragmataPro:"
            ++ "style=Regular:pixelsize=180:hinting=true"

-- this is a "fake title" used as a highlight bar in lieu of full borders
-- (I find this a cleaner and less visually intrusive solution)
topBarTheme = def
    { fontName              = myFont
    , inactiveBorderColor   = base03
    , inactiveColor         = base03
    , inactiveTextColor     = base03
    , activeBorderColor     = active
    , activeColor           = active
    , activeTextColor       = active
    , urgentBorderColor     = red
    , urgentTextColor       = yellow
    , decoHeight            = topbar
    }

myTabTheme = def
    { fontName              = myFont
    , activeColor           = active
    , inactiveColor         = base02
    , activeBorderColor     = active
    , inactiveBorderColor   = base02
    , activeTextColor       = base03
    , inactiveTextColor     = base00
    }

myPromptTheme = def
    { font                  = myFont
    , bgColor               = base03
    , fgColor               = active
    , fgHLight              = base03
    , bgHLight              = active
    , borderColor           = base03
    , promptBorderWidth     = 0
    , height                = prompt
    , position              = Top
    }

warmPromptTheme = myPromptTheme
    { bgColor               = yellow
    , fgColor               = base03
    , position              = Top
    }

hotPromptTheme = myPromptTheme
    { bgColor               = red
    , fgColor               = base3
    , position              = Top
    }

myShowWNameTheme = def
    { swn_font              = myWideFont
    , swn_fade              = 0.5
    , swn_bgcolor           = "#000000"
    , swn_color             = "#FFFFFF"
    }

------------------------------------------------------------------------}}}
-- Layouts                                                              {{{
--
-- WARNING: WORK IN PROGRESS AND A LITTLE MESSY
---------------------------------------------------------------------------

-- Tell X.A.Navigation2D about specific layouts and how to handle them

myNav2DConf = def
    { defaultTiledNavigation    = centerNavigation
    , floatNavigation           = centerNavigation
    , screenNavigation          = lineNavigation
    , layoutNavigation          = [("Full",          centerNavigation)
    -- line/center same results   ,("Simple Tabs", lineNavigation)
    --                            ,("Simple Tabs", centerNavigation)
                                  ]
    , unmappedWindowRect        = [("Full", singleWindowRect)
    -- works but breaks tab deco  ,("Simple Tabs", singleWindowRect)
    -- doesn't work but deco ok   ,("Simple Tabs", fullScreenRect)
                                  ]
    }


data FULLBAR = FULLBAR deriving (Read, Show, Eq, Typeable)
instance Transformer FULLBAR Window where
    transform FULLBAR x k = k barFull (\_ -> x)

-- tabBarFull = avoidStruts $ noFrillsDeco shrinkText topBarTheme $ addTabs shrinkText myTabTheme $ Simplest
barFull = avoidStruts $ Simplest

-- cf http://xmonad.org/xmonad-docs/xmonad-contrib/src/XMonad-Config-Droundy.html

myLayoutHook = showWorkspaceName
             $ onWorkspace wsFLOAT floatWorkSpace
             $ fullscreenFloat -- fixes floating windows going full screen, while retaining "bounded" fullscreen
             $ fullScreenToggle
             $ fullBarToggle
             $ mirrorToggle
             $ reflectToggle
             $ flex ||| tabs
  where

--    testTall = Tall 1 (1/50) (2/3)
--    myTall = subLayout [] Simplest $ trackFloating (Tall 1 (1/20) (1/2))

    floatWorkSpace      = simplestFloat
    fullBarToggle       = mkToggle (single FULLBAR)
    fullScreenToggle    = mkToggle (single FULL)
    mirrorToggle        = mkToggle (single MIRROR)
    reflectToggle       = mkToggle (single REFLECTX)
    smallMonResWidth    = 1920
    showWorkspaceName   = showWName' myShowWNameTheme

    named n             = renamed [(XMonad.Layout.Renamed.Replace n)]
    trimNamed w n       = renamed [(XMonad.Layout.Renamed.CutWordsLeft w),
                                   (XMonad.Layout.Renamed.PrependWords n)]
    suffixed n          = renamed [(XMonad.Layout.Renamed.AppendWords n)]
    trimSuffixed w n    = renamed [(XMonad.Layout.Renamed.CutWordsRight w),
                                   (XMonad.Layout.Renamed.AppendWords n)]

    addTopBar           = noFrillsDeco shrinkText topBarTheme

    mySpacing           = spacing gap
    sGap                = quot gap 2
    myGaps              = gaps [(U, gap),(D, gap),(L, gap),(R, gap)]
    mySmallGaps         = gaps [(U, sGap),(D, sGap),(L, sGap),(R, sGap)]
    myBigGaps           = gaps [(U, gap*2),(D, gap*2),(L, gap*2),(R, gap*2)]

    --------------------------------------------------------------------------
    -- Tabs Layout                                                          --
    --------------------------------------------------------------------------

    threeCol = named "Unflexed"
         $ avoidStruts
         $ addTopBar
         $ myGaps
         $ mySpacing
         $ ThreeColMid 1 (1/10) (1/2)

    tabs = named "Tabs"
         $ avoidStruts
         $ addTopBar
         $ addTabs shrinkText myTabTheme
         $ Simplest

    -----------------------------------------------------------------------
    -- Flexi SubLayouts                                                  --
    -----------------------------------------------------------------------
    --
    -- In many ways the best solution. Acts like ThreeColumns, Tall, BSP,
    -- or any other container layout style. Can use this layout just as you
    -- would those without tabs at all, or you can easily merge any windows
    -- into a tabbed group.
    --
    -- Diagrams:
    --
    -- (examples only... this is a very flexible layout and as such the
    -- layout style and arrangement isn't limited as much as the other
    -- attempts below)
    --
    -- Ultrawide:
    -- --------------------------------------------
    -- |          |                    |          |
    -- |          |                    |   Tabs   |
    -- |          |                    |          |
    -- |----------|       Master       |----------|
    -- |          |                    |          |
    -- |   Tabs   |                    |          |
    -- |          |                    |          |
    -- --------------------------------------------
    --
    -- Standard:
    -- ---------------------------------
    -- |                    |          |
    -- |                    |          |
    -- |                    |          |
    -- |       Master       |----------|
    -- |                    |          |
    -- |                    |   Tabs   |
    -- |                    |          |
    -- ---------------------------------
    --
    --
    -- Advantages
    --
    --   * tab group is movable as a unit and acts like any other window
    --
    --   * this is the "cleanest" of the dynamic layouts I've worked with
    --     and leaves no "pixel dust" on the screen when switching to a WS
    --     on a different monitor
    --
    --   * navigation and window/group movement is trivial with
    --     X.A.Navigation2D
    --
    --   * master window remains master when switching screens (unlike
    --     the "X.L.Master" based solution below)
    --
    --   * unlike some of the other solutions, it is trivial to change
    --     the exterior layout format and so I could potentially add in
    --     some layout change to BSP or other layout that I want to test
    --     while still retaining the tab functionality
    --
    -- Disadvantages
    --
    --   * layout starts without any tabs (could be considered a feature
    --     since in that case the layout performs exactly as the parent/
    --     container layout does)
    --
    --   * To move a window into or out of the tabbed group requires
    --     special key bindings unique to X.L.SubLayouts
    --
    --  Understanding XMonad.Layouts.SubLayouts
    --
    --  It took me a while to grok this.
    --
    --  the subLayout hook is used with the following format:
    --
    --    subLayout advanceInnerLayouts innerLayout outerLayout
    --
    --  It works like this: subLayout modifies an entire other layout (or
    --  layouts), enabling you to turn what would be a normal window into
    --  a little group of windows managed by an entirely different layout.
    --
    --  In my case, I'm using layouts like "Three Column" and "Tall" as the
    --  nominal "container" layout (what SubLayouts calls the "outerLayout").
    --
    --  The "inner layout" in my case is just "Simplest". I'm also adding tabs
    --  which are only applied to my sublayouts. Not sure how that works
    --  but it's apparent from the X.L.SubLayouts documentation that this is
    --  the intended use/behavior. Essential X.L.SubLayouts is hijacking these
    --  added tabs and applying them just to the Simplest layout, and then that
    --  in turn is stuck inside the rectangle that would normally hold a window
    --  in my normal layouts.
    --
    --  One of the confusing things for me at first was that the layout doesn't
    --  start with any subLayouts. So it appears to just be a normal layout.
    --  You have to "merge all" to suck everything up into a Simplest tabbed
    --  group and then you can add other windows normally and you'll
    --  have a sublayout with tabs.
    --
    --  Note: subLayouts has some other features. For example, you can give it
    --  a list of layouts to work through and it will advance through them in
    --  series (or possibly in an order your provide) and will apply different
    --  layouts to different subLayout groups. Each time you add a new window
    --  to your layout, it acquires the sublayout, even if you don't know it.
    --
    --  In my case, my list is one long and is just the first window I add.
    --
    --  Ex. The second group is Tall, the third is Circle, all others are
    --  tabbed with:
    --
    --  myLayout = addTabs shrinkText def
    --           $ subLayout [0,1,2] (Simplest ||| Tall 1 0.2 0.5 ||| Circle)
    --                    $ Tall 1 0.2 0.5 ||| Full
   
    -- this is a flexible sublayout layout that has only one container
    -- layout style (depending on screen)
    --     flexiSub = named "Flexi SubLayouts"
    --               $ avoidStruts
    --               $ windowNavigation
    --               $ addTopBar
    --               $ myGaps
    --               $ addTabs shrinkText myTabTheme
    --               $ mySpacing
    --               $ subLayout [] Simplest
    --               $ ifWider smallMonResWidth wideLayout standardLayout
    --               where
    --                   wideLayout = ThreeColMid 1 (1/100) (1/2)
    --                   standardLayout = ResizableTall 1 (1/50) (2/3) []

    -- retained during development: safe to remove later

    flex = trimNamed 5 "Flex"
              $ avoidStruts
              -- don't forget: even though we are using X.A.Navigation2D
              -- we need windowNavigation for merging to sublayouts
              $ windowNavigation
              $ addTopBar
              $ addTabs shrinkText myTabTheme
              -- $ subLayout [] (Simplest ||| (mySpacing $ Accordion))
              $ subLayout [] (Simplest ||| Accordion)
              $ ifWider smallMonResWidth wideLayouts standardLayouts
              where
                  wideLayouts = myGaps $ mySpacing
                      $ (suffixed "Wide 3Col" $ ThreeColMid 1 (1/20) (1/2))
                    ||| (trimSuffixed 1 "Wide BSP" $ hiddenWindows emptyBSP)
                  --  ||| fullTabs
                  standardLayouts = myGaps $ mySpacing
                      $ (suffixed "Std 2/3" $ ResizableTall 1 (1/20) (2/3) [])
                    ||| (suffixed "Std 1/2" $ ResizableTall 1 (1/20) (1/2) [])

                  --  ||| fullTabs
                  --fullTabs = suffixed "Tabs Full" $ Simplest
                  --
                  -- NOTE: removed this from the two (wide/std) sublayout
                  -- sequences. if inside the ifWider, the ||| combinator
                  -- from X.L.LayoutCombinators can't jump to it directly (
                  -- or I'm doing something wrong, either way, it's simpler
                  -- to solve it by just using a tabbed layout in the main
                  -- layoutHook). The disadvantage is that I lose the "per
                  -- screen" memory of which layout was where if using the
                  -- tabbed layout (if using the the ifWider construct as
                  -- I am currently, it seems to work fine)
                  --
                  -- Using "Full" here (instead of Simplest) will retain the
                  -- tabbed sublayout structure and allow paging through each
                  -- group/window in full screen mode. However my preference
                  -- is to just see all the windows as tabs immediately.  
                  -- Using "Simplest" here will do this: display all windows
                  -- as tabs across the top, no "paging" required. However
                  -- this is misleading as the sublayouts are of course still
                  -- there and you will have to use the nornmal W.focusUp/Down
                  -- to successfully flip through them. Despite this
                  -- limitation I prefer this to the results with "Full".

{-|
    -----------------------------------------------------------------------
    -- Simple Flexi                                                      --
    -----------------------------------------------------------------------
    --
    -- Simple dynamically resizing layout as with the other variations in
    -- this config. This layout has not tabs in it and simply uses
    -- Resizable Tall and Three Column layouts.

    simpleFlexi = named "Simple Flexible"
              $ ifWider smallMonResWidth simpleThree simpleTall

    simpleTall = named "Tall"
              $ addTopBar
              $ avoidStruts
              $ mySpacing
              $ myGaps
              $ ResizableTall 1 (1/300) (2/3) []
              
    simpleThree = named "Three Col"
              $ avoidStruts
              $ addTopBar
              $ mySpacing
              $ myGaps
              $ ThreeColMid 1 (3/100) (1/2)

    -----------------------------------------------------------------------
    -- Other Misc Layouts                                                --
    -----------------------------------------------------------------------
    --
    --

    masterTabbedP   = named "MASTER TABBED"
              $ addTopBar
              $ avoidStruts
              $ mySpacing
              $ myGaps
              $ mastered (1/100) (1/2) $ tabbed shrinkText myTabTheme

    bsp       = named "BSP"
              $ borderResize (avoidStruts
              $ addTopBar
              $ mySpacing
              $ myGaps
              $ emptyBSP )
              -- $ borderResize (emptyBSP)

    oneBig    = named "1BG"
              $ avoidStruts
              $ addTopBar
              $ mySpacing
              $ myGaps
              $ OneBig (3/4) (3/4)

    tiledP    = named "TILED"
              $ addTopBar
              $ avoidStruts
              $ mySpacing
              $ myGaps
              $ consoleOn
              $ tiled'

    oneUp =   named "1UP"
              $ avoidStruts
              $ myGaps
              $ combineTwoP (ThreeCol 1 (3/100) (1/2))
                            (Simplest)
                            (Tall 1 0.03 0.5)
                            (ClassName "Google-chrome-beta")

    -----------------------------------------------------------------------
    -- Master-Tabbed Dymamic                                             --
    -----------------------------------------------------------------------
    --
    -- Dynamic 3 pane layout with one tabbed panel using X.L.Master
    -- advantage is that it can do a nice 3-up on both ultrawide and
    -- standard (laptop in my case) screen sizes, where the layouts
    -- look like this:
    --
    -- Ultrawide:
    -- --------------------------------------------
    -- |          |                    |          |
    -- |          |                    |          |
    -- |          |                    |          |
    -- |  Master  |       Master       |   Tabs   |
    -- |          |                    |          |
    -- |          |                    |          |
    -- |          |                    |          |
    -- --------------------------------------------
    -- \____________________ _____________________/
    --                      '
    --                 all one layout
    --
    -- Standard:
    -- ---------------------------------
    -- |                    |          |
    -- |                    |          |
    -- |                    |          |
    -- |       Master       |   Tabs   |
    -- |                    |          |
    -- |                    |          |
    -- |                    |          |
    -- ---------------------------------
    -- \_______________ _______________/
    --                 '
    --            all one layout
    --
    -- Advantages to this use of X.L.Master to created this dynamic
    -- layout include:
    --
    --   * No fussing with special keys to swap windows between the
    --     Tabs and Master zones
    --
    --   * Window movement and resizing is very straightforward
    --
    --   * Limited need to maintain a mental-map of the layout
    --     (pretty easy to understand... it's just a layout)
    --
    -- Disadvantages include:
    --
    --   * Swapping a window from tabbed area will of necessity swap
    --     one of the Master windows back into tabs (since there can
    --     only be two master windows)
    --
    --   * Master area can have only one/two windows in std/wide modes
    --     respectively
    --
    --   * When switching from wide to standard, the leftmost pane
    --     (which is visually secondary to the large central master
    --     window) becomes the new dominant master window on the
    --     standard display (this is easy enough to deal with but
    --     is a non-intuitive effect)

    masterTabbedDynamic = named "Master-Tabbed Dynamic"
              $ ifWider smallMonResWidth masterTabbedWide masterTabbedStd

    masterTabbedStd = named "Master-Tabbed Standard"
              $ addTopBar
              $ avoidStruts
              $ gaps [(U, gap*2),(D, gap*2),(L, gap*2),(R, gap*2)]
              $ mastered (1/100) (2/3)
              $ gaps [(U, 0),(D, 0),(L, gap*2),(R, 0)]
              $ tabbed shrinkText myTabTheme

    masterTabbedWide = named "Master-Tabbed Wide"
              $ addTopBar
              $ avoidStruts
              $ gaps [(U, gap*2),(D, gap*2),(L, gap*2),(R, gap*2)]
              $ mastered (1/100) (1/4)
              $ gaps [(U, 0),(D, 0),(L, gap*2),(R, 0)]
              $ mastered (1/100) (2/3)
              $ gaps [(U, 0),(D, 0),(L, gap*2),(R, 0)]
              $ tabbed shrinkText myTabTheme

    -----------------------------------------------------------------------
    -- Tall-Tabbed Dymamic                                               --
    -----------------------------------------------------------------------
    --
    -- Dynamic 3 pane layout with one tabbed panel using X.L.ComboP
    -- advantage is that it can do a nice 3-up on both ultrawide and
    -- standard (laptop in my case) screen sizes, where the layouts
    -- look like this:
    --
    -- Ultrawide:
    -- --------------------------------------------
    -- |          |                    |          |
    -- |          |                    |          |
    -- |          |                    |          |
    -- |----------|       Master       |   Tabs   |
    -- |          |                    |          |
    -- |          |                    |          |
    -- |          |                    |          |
    -- --------------------------------------------
    -- \______________ _______________/\____ _____/
    --                '                     '
    --        this set of panes is      This is a
    --        its' own layout in a      separate
    --        Tall configuration        tab format
    --                                  layout
    --
    -- Standard:
    -- ---------------------------------
    -- |                    |          |
    -- |                    |          |
    -- |                    |          |
    -- |       Master       |   Tabs   |
    -- |                    |          |
    -- |--------------------|          |
    -- |         |          |          |
    -- ---------------------------------
    -- \_________ _________/\____ _____/
    --           '               '
    -- this set of panes is  This is a
    -- its' own layout in a  separate
    -- Tall configuration    tab format
    --                       layout
    --
    -- Advantages to this use of ComboP to created this dynamic
    -- layout include:
    --
    --   * the center Master stays the same when the layout
    --     changes (unlike the X.L.Master based dyn. layout)
    --
    --   * the Master can have a set of panes under it on the
    --     small screen (standard) layout
    --
    --   * on ultrawide the leftmost pane may be divided into
    --     multiple windows
    --
    --   * possible to toss a tabbed window to the "Master" area
    --     without swapping a window back into tabs
    --
    --   * use of ComboP allows redirection windows to either
    --     left or right section
    --
    -- Disadvantages include:
    --
    --   * normal window swaps fail between the two separate
    --     layouts. There must be a special swap-between-layouts
    --     binding (normal window NAVIGATION works, at least using
    --     X.A.Navigation2D).
    --
    --   * switching between screens can leave title bar clutter
    --     that hasn't been cleaned up properly (restarting
    --     XMonad works to clean this up, but that's hacky)
    --
    --   * somewhat greater need to maintain a mental-map of the
    --     layout (you need to have a sense for the windows being
    --     in separate sections of the different layouts)

    smartTallTabbed = named "Smart Tall-Tabbed"
            $ avoidStruts
            $ ifWider smallMonResWidth wideScreen normalScreen
            where
            wideScreen   = combineTwoP (TwoPane 0.03 (3/4))
                           (smartTall)
                           (smartTabbed)
                           (ClassName "Google-chrome-beta")
            normalScreen = combineTwoP (TwoPane 0.03 (2/3))
                           (smartTall)
                           (smartTabbed)
                           (ClassName "Google-chrome-beta")

    smartTall = named "Smart Tall"
            $ addTopBar
        $ mySpacing
            $ myGaps
        $ boringAuto
            $ ifWider smallMonResWidth wideScreen normalScreen
            where
                wideScreen = reflectHoriz $ Tall 1 0.03 (2/3)
                normalScreen = Mirror $ Tall 1 0.03 (4/5)

    smartTabbed = named "Smart Tabbed"
              $ addTopBar
              $ myCustomGaps
              $ tabbed shrinkText myTabTheme
-}
    -----------------------------------------------------------------------
    -- Flexi Combinators                                                 --
    -----------------------------------------------------------------------
    --
    -- failed attempt. creates a nice looking layout but I'm not sure
    -- how to actually direct tabs to the tabbed area
    --
    --     flexiCombinators = named "Flexi Combinators"
    --             $ avoidStruts
    --             $ ifWider smallMonResWidth wideScreen normalScreen
    --             where
    --             wideScreen   = smartTall ****||* smartTabbed
    --             normalScreen = smartTall ***||** smartTabbed




------------------------------------------------------------------------}}}
-- Bindings                                                             {{{
---------------------------------------------------------------------------

myModMask = mod4Mask -- super (and on my system, hyper) keys

-- Display keyboard mappings using zenity
-- from https://github.com/thomasf/dotfiles-thomasf-xmonad/
--              blob/master/.xmonad/lib/XMonad/Config/A00001.hs
showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
    h <- spawnPipe "zenity --text-info --font=PragmataPro"
    hPutStr h (unlines $ showKm x)
    hClose h
    return ()

-- some of the structure of the following cribbed from 
-- https://github.com/SimSaladin/configs/blob/master/.xmonad/xmonad.hs
-- https://github.com/paul-axe/dotfiles/blob/master/.xmonad/xmonad.hs
-- https://github.com/pjones/xmonadrc (+ all the dyn project stuff)

-- wsKeys = map (\x -> "; " ++ [x]) ['1'..'9']
-- this along with workspace section below results in something link
-- M1-semicolon         1 View      ws
-- M1-semicolon         2 View      ws
-- M1-Shift-semicolon   1 Move w to ws
-- M1-Shift-semicolon   2 Move w to ws
-- M1-C-Shift-semicolon 1 Copy w to ws
-- M1-C-Shift-semicolon 2 Copy w to ws

wsKeys = map show $ [1..9] ++ [0]

-- any workspace but scratchpad
notSP = (return $ ("NSP" /=) . W.tag) :: X (WindowSpace -> Bool)
shiftAndView dir = findWorkspace getSortByIndex dir (WSIs notSP) 1
        >>= \t -> (windows . W.shift $ t) >> (windows . W.greedyView $ t)

-- hidden, non-empty workspaces less scratchpad
shiftAndView' dir = findWorkspace getSortByIndexNoSP dir HiddenNonEmptyWS 1
        >>= \t -> (windows . W.shift $ t) >> (windows . W.greedyView $ t)
nextNonEmptyWS = findWorkspace getSortByIndexNoSP Next HiddenNonEmptyWS 1
        >>= \t -> (windows . W.view $ t)
prevNonEmptyWS = findWorkspace getSortByIndexNoSP Prev HiddenNonEmptyWS 1
        >>= \t -> (windows . W.view $ t)
getSortByIndexNoSP =
        fmap (.namedScratchpadFilterOutWorkspace) getSortByIndex

-- toggle any workspace but scratchpad
myToggle = windows $ W.view =<< W.tag . head . filter 
        ((\x -> x /= "NSP" && x /= "SP") . W.tag) . W.hidden

myKeys conf = let

    subKeys str ks = subtitle str : mkNamedKeymap conf ks
    screenKeys     = ["w","v","z"]
    dirKeys        = ["j","k","h","l"]
    arrowKeys        = ["<D>","<U>","<L>","<R>"]
    dirs           = [ D,  U,  L,  R ]

    --screenAction f        = screenWorkspace >=> flip whenJust (windows . f)

    zipM  m nm ks as f = zipWith (\k d -> (m ++ k, addName nm $ f d)) ks as
    zipM' m nm ks as f b = zipWith (\k d -> (m ++ k, addName nm $ f d b)) ks as

    -- from xmonad.layout.sublayouts
    focusMaster' st = let (f:fs) = W.integrate st
        in W.Stack f [] fs
    swapMaster' (W.Stack f u d) = W.Stack f [] $ reverse u ++ d

    -- try sending one message, fallback if unreceived, then refresh
    tryMsgR x y = sequence_ [(tryMessage_ x y), refresh]

    -- warpCursor = warpToWindow (9/10) (9/10)

    -- cf https://github.com/pjones/xmonadrc
    --switch :: ProjectTable -> ProjectName -> X ()
    --switch ps name = case Map.lookup name ps of
    --  Just p              -> switchProject p
    --  Nothing | null name -> return ()

    -- do something with current X selection
    unsafeWithSelection app = join $ io $ liftM unsafeSpawn $ fmap (\x -> app ++ " " ++ x) getSelection

    toggleFloat w = windows (\s -> if M.member w (W.floating s)
                    then W.sink w s
                    else (W.float w (W.RationalRect (1/3) (1/4) (1/2) (4/5)) s))

    in

    -----------------------------------------------------------------------
    -- System / Utilities
    -----------------------------------------------------------------------
    subKeys "System"
    [ ("M-q"                    , addName "Restart XMonad"                  $ spawn "xmonad --restart")
    , ("M-C-q"                  , addName "Rebuild & restart XMonad"        $ spawn "xmonad --recompile && xmonad --restart")
    , ("M-S-q"                  , addName "Quit XMonad"                     $ confirmPrompt hotPromptTheme "Quit XMonad" $ io (exitWith ExitSuccess))
    , ("M-x"                    , addName "Lock screen"                     $ spawn "xset s activate")
    , ("M-S-4"                    , addName "Print Screen"                    $ return ())
  --, ("M-F1"                   , addName "Show Keybindings"                $ return ())
    ] ^++^

    -----------------------------------------------------------------------
    -- Actions
    -----------------------------------------------------------------------
    subKeys "Actions"
    [ ("M-a"                    , addName "Notify w current X selection"    $ unsafeWithSelection "notify-send")
  --, ("M-7"                    , addName "TESTING"                         $ runInTerm "-name glances" "glances" )
    -- , ("M-u"                    , addName "Copy current browser URL"        $ spawn "with-url copy")
    -- , ("M-o"                    , addName "Display (output) launcher"       $ spawn "displayctl menu")
    , ("M-<XF86Display>"        , addName "Display - force internal"        $ spawn "displayctl internal")
    , ("S-<XF86Display>"        , addName "Display - force internal"        $ spawn "displayctl internal")
    -- , ("<XF86AudioLowerVolume>" , addName "Volume down"                     $ lowerVolume 10 >> return ())
    -- , ("<XF86AudioRaiseVolume>" , addName "Volume up"                       $ raiseVolume 10 >> return ())
    -- , ("<XF86AudioMute>"        , addName "Toggle volume"                   $ toggleMute    >> return ())
    , ("M-i"                    , addName "Network (Interface) launcher"    $ spawn myNetwork)
    , ("M-/"                    , addName "On-screen keys"                  $ spawn "killall screenkey &>/dev/null || screenkey --no-systray")
    , ("M-S-/"                  , addName "On-screen keys settings"         $ spawn "screenkey --show-settings")
    , ("M1-p"                   , addName "Capture screen"                  $ spawn "screenshot" )
    , ("M1-S-p"                 , addName "Capture screen - area select"    $ spawn "screenshot area" )
    , ("M1-r"                   , addName "Record screen"                   $ spawn "screencast" )
    , ("M1-S-r"                 , addName "Record screen - area select"     $ spawn "screencast area" )
    ] ^++^

    -----------------------------------------------------------------------
    -- Launchers
    -----------------------------------------------------------------------
    subKeys "Launchers"
    [ ("M-<Space>"              , addName "Launcher"                        $ spawn myLauncher)
    , ("M-u"                    , addName "Password Manager"                $ spawn myPasswordManager)
    , ("M-e"                    , addName "Emojis"                          $ spawn myEmoji)
    , ("M-<Return>"             , addName "Terminal"                        $ spawn myTerminal)
    , ("M-\\"                   , addName "Browser"                         $ spawn myBrowser)
    , ("M-c"                    , addName "NSP Chat"                        $ bindOn WS [(wsWRK, namedScratchpadAction scratchpads "hangoutsWork"),
                                                                              ("", namedScratchpadAction scratchpads "hangoutsPersonal")])
    , ("M-t"                    , addName "NSP Tasks"                       $ bindOn WS [(wsWRK, namedScratchpadAction scratchpads "trelloWork"),
                                                                              ("", namedScratchpadAction scratchpads "trello")])
    , ("M-m"                    , addName "NSP Music"                       $ namedScratchpadAction scratchpads "googleMusic")
    , ("M-v"                    , addName "NSP Video"                       $ namedScratchpadAction scratchpads "plex")
    , ("M1-x"                   , addName "NSP Xawtv"                       $ namedScratchpadAction scratchpads "xawtv")
    , ("M-n"                    , addName "NSP Console"                     $ namedScratchpadAction scratchpads "console")
    , ("M-s s"                  , addName "Cancel submap"                   $ return ())
    , ("M-s M-s"                , addName "Cancel submap"                   $ return ())
    ] ^++^

    -----------------------------------------------------------------------
    -- Windows
    -----------------------------------------------------------------------

    subKeys "Windows"
    (
    [ ("M-<Backspace>"          , addName "Kill"                            kill1)
    , ("M-S-<Backspace>"        , addName "Kill all"                        $ confirmPrompt hotPromptTheme "kill all" $ killAll)
    --, ("M-d"                    , addName "Duplicate w to all ws"           $ windows copyToAll)
    --, ("M-S-d"                  , addName "Kill other duplicates"           $ killAllOtherCopies)
    , ("M-d"                    , addName "Duplicate w to all ws"           $ toggleCopyToAll)
    , ("M-p"                    , addName "Hide window to stack"            $ withFocused hideWindow)
    , ("M-S-p"                  , addName "Restore hidden window (FIFO)"    $ popOldestHiddenWindow)

    , ("M-b"                    , addName "Promote"                         $ promote) 

    , ("M-g"                    , addName "Un-merge from sublayout"         $ withFocused (sendMessage . UnMerge))
    , ("M-S-g"                  , addName "Merge all into sublayout"        $ withFocused (sendMessage . MergeAll))

    , ("M-z u"                  , addName "Focus urgent"                    focusUrgent)
    , ("M-z m"                  , addName "Focus master"                    $ windows W.focusMaster)

    --, ("M-<Tab>"              	, addName "Focus down"                      $ windows W.focusDown)
    --, ("M-S-<Tab>"              , addName "Focus up"                        $ windows W.focusUp)

    , ("M-'"                    , addName "Navigate tabs D"                 $ bindOn LD [("Tabs", windows W.focusDown), ("", onGroup W.focusDown')])
    , ("M-;"                    , addName "Navigate tabs U"                 $ bindOn LD [("Tabs", windows W.focusUp), ("", onGroup W.focusUp')])
    , ("C-'"                    , addName "Swap tab D"                      $ windows W.swapDown)
    , ("C-;"                    , addName "Swap tab U"                      $ windows W.swapUp)

    -- ComboP specific (can remove after demo)
    , ("M-C-S-m"                , addName "Combo swap"                      $ sendMessage $ SwapWindow)
    ]

    ++ zipM' "M-"               "Navigate window"                           dirKeys dirs windowGo True
    -- ++ zipM' "M-S-"               "Move window"                               dirKeys dirs windowSwap True
    -- TODO: following may necessitate use of a "passthrough" binding that can send C- values to focused w
    ++ zipM' "C-"             "Move window"                               dirKeys dirs windowSwap True
    ++ zipM  "M-C-"             "Merge w/sublayout"                         dirKeys dirs (sendMessage . pullGroup)
    ++ zipM' "M-"               "Navigate screen"                           arrowKeys dirs screenGo True
    -- ++ zipM' "M-S-"             "Move window to screen"                     arrowKeys dirs windowToScreen True
    ++ zipM' "M-C-"             "Move window to screen"                     arrowKeys dirs windowToScreen True
    ++ zipM' "M-S-"             "Swap workspace to screen"                  arrowKeys dirs screenSwap True

    ) ^++^

    -----------------------------------------------------------------------
    -- Workspaces & Projects
    -----------------------------------------------------------------------

    -- original version was for dynamic workspaces
    --    subKeys "{a,o,e,u,i,d,...} focus and move window between workspaces"
    --    (  zipMod "View      ws" wsKeys [0..] "M-"      (withNthWorkspace W.greedyView)

    subKeys "Workspaces & Projects"
    (
    [ ("M-w"                    , addName "Switch to Project"           $ switchProjectPrompt warmPromptTheme)
    , ("M-S-w"                  , addName "Shift to Project"            $ shiftToProjectPrompt warmPromptTheme)
    , ("M-<Escape>"             , addName "Next non-empty workspace"    $ nextNonEmptyWS)
    , ("M-S-<Escape>"           , addName "Prev non-empty workspace"    $ prevNonEmptyWS)
    , ("M-`"                    , addName "Next non-empty workspace"    $ nextNonEmptyWS)
    , ("M-S-`"                  , addName "Prev non-empty workspace"    $ prevNonEmptyWS)
    , ("M-a"                    , addName "Toggle last workspace"       $ toggleWS' ["NSP"])
    ]
    ++ zipM "M-"                "View      ws"                          wsKeys [0..] (withNthWorkspace W.greedyView)
    -- ++ zipM "M-S-"              "Move w to ws"                          wsKeys [0..] (withNthWorkspace W.shift)
    -- TODO: following may necessitate use of a "passthrough" binding that can send C- values to focused w
    ++ zipM "C-"                "Move w to ws"                          wsKeys [0..] (withNthWorkspace W.shift)
    -- TODO: make following a submap
    ++ zipM "M-S-C-"            "Copy w to ws"                          wsKeys [0..] (withNthWorkspace copy)
    ) ^++^

    -- TODO: consider a submap for nav/move to specific workspaces based on first initial

    -----------------------------------------------------------------------
    -- Layouts & Sublayouts
    -----------------------------------------------------------------------

    subKeys "Layout Management"

    [ ("M-<Tab>"                , addName "Cycle all layouts"               $ sendMessage NextLayout)
    , ("M-C-<Tab>"              , addName "Cycle sublayout"                 $ toSubl NextLayout)
    , ("M-S-<Tab>"              , addName "Reset layout"                    $ setLayout $ XMonad.layoutHook conf)

    , ("M-y"                    , addName "Float tiled w"                   $ withFocused toggleFloat)
    , ("M-S-y"                  , addName "Tile all floating w"             $ sinkAll)

    , ("M-,"                    , addName "Decrease master windows"         $ sendMessage (IncMasterN (-1)))
    , ("M-."                    , addName "Increase master windows"         $ sendMessage (IncMasterN 1))

    , ("M-r"                    , addName "Reflect/Rotate"              $ tryMsgR (Rotate) (XMonad.Layout.MultiToggle.Toggle REFLECTX))
    , ("M-S-r"                  , addName "Force Reflect (even on BSP)" $ sendMessage (XMonad.Layout.MultiToggle.Toggle REFLECTX))


    -- If following is run on a floating window, the sequence first tiles it.
    -- Not perfect, but works.
    , ("M-f"                , addName "Fullscreen"                      $ sequence_ [ (withFocused $ windows . W.sink)
                                                                        , (sendMessage $ XMonad.Layout.MultiToggle.Toggle FULL) ])

    -- Fake fullscreen fullscreens into the window rect. The expand/shrink
    -- is a hack to make the full screen paint into the rect properly.
    -- The tryMsgR handles the BSP vs standard resizing functions.
    , ("M-S-f"                  , addName "Fake fullscreen"             $ sequence_ [ (P.sendKey P.noModMask xK_F11)
                                                                                    , (tryMsgR (ExpandTowards L) (Shrink))
                                                                                    , (tryMsgR (ExpandTowards R) (Expand)) ])
    , ("C-S-h"                  , addName "Ctrl-h passthrough"          $ P.sendKey controlMask xK_h)
    , ("C-S-j"                  , addName "Ctrl-j passthrough"          $ P.sendKey controlMask xK_j)
    , ("C-S-k"                  , addName "Ctrl-k passthrough"          $ P.sendKey controlMask xK_k)
    , ("C-S-l"                  , addName "Ctrl-l passthrough"          $ P.sendKey controlMask xK_l)
    ] ^++^

    -----------------------------------------------------------------------
    -- Reference
    -----------------------------------------------------------------------
    -- recent windows not working
    -- , ("M4-<Tab>",              , addName "Cycle recent windows"        $ (cycleRecentWindows [xK_Super_L] xK_Tab xK_Tab))
    -- either not using these much or (in case of two tab items below), they conflict with other bindings
    -- so I'm just turning off this whole section for now. retaining for refernce after a couple months
    -- of working with my bindings to see if I want them back. TODO REVIEW
    --, ("M-s m"                  , addName "Swap master"                 $ windows W.shiftMaster)
    --, ("M-s p"                  , addName "Swap next"                   $ windows W.swapUp)
    --, ("M-s n"                  , addName "Swap prev"                   $ windows W.swapDown)
    --, ("M-<Tab>"                , addName "Cycle up"                    $ windows W.swapUp)
    --, ("M-S-<Tab>"              , addName "Cycle down"                  $ windows W.swapDown)

    -- sublayout specific (unused)
    -- , ("M4-C-S-m"               , addName "onGroup focusMaster"         $ onGroup focusMaster')
    -- , ("M4-C-S-]"               , addName "toSubl IncMasterN 1"         $ toSubl $ IncMasterN 1)
    -- , ("M4-C-S-["               , addName "toSubl IncMasterN -1"        $ toSubl $ IncMasterN (-1))
    -- , ("M4-C-S-<Return>"        , addName "onGroup swapMaster"          $ onGroup swapMaster')


    -----------------------------------------------------------------------
    -- Resizing
    -----------------------------------------------------------------------

    subKeys "Resize"

    [

    -- following is a hacky hack hack
    --
    -- I want to be able to use the same resize bindings on both BinarySpacePartition and other
    -- less sophisticated layouts. BSP handles resizing in four directions (amazing!) but other
    -- layouts have less refined tastes and we're lucky if they just resize the master on a single
    -- axis.
    --
    -- To this end, I am using X.A.MessageFeedback to test for success on using the BSP resizing
    -- and, if it fails, defaulting to the standard (or the X.L.ResizableTile Mirror variants)
    -- Expand and Shrink commands.
    --
    -- The "sequence_" wrapper is needed because for some reason the windows weren't resizing till
    -- I moved to a different window or refreshed, so I added that here. Shrug.
    
    -- mnemonic: less than / greater than
    --, ("M4-<L>"       , addName "Expand (L on BSP)"     $ sequence_ [(tryMessage_ (ExpandTowards L) (Expand)), refresh])

--      ("C-<L>"                  , addName "Expand (L on BSP)"           $ tryMsgR (ExpandTowards L) (Shrink))
--    , ("C-<R>"                  , addName "Expand (R on BSP)"           $ tryMsgR (ExpandTowards R) (Expand))
--    , ("C-<U>"                  , addName "Expand (U on BSP)"           $ tryMsgR (ExpandTowards U) (MirrorShrink))
--    , ("C-<D>"                  , addName "Expand (D on BSP)"           $ tryMsgR (ExpandTowards D) (MirrorExpand))
--
--    , ("C-S-<L>"                , addName "Shrink (L on BSP)"           $ tryMsgR (ShrinkFrom R) (Shrink))
--    , ("C-S-<R>"                , addName "Shrink (R on BSP)"           $ tryMsgR (ShrinkFrom L) (Expand))
--    , ("C-S-<U>"                , addName "Shrink (U on BSP)"           $ tryMsgR (ShrinkFrom D) (MirrorShrink))
--    , ("C-S-<D>"                , addName "Shrink (D on BSP)"           $ tryMsgR (ShrinkFrom U) (MirrorExpand))

      ("M-["                    , addName "Expand (L on BSP)"           $ tryMsgR (ExpandTowards L) (Shrink))
    , ("M-]"                    , addName "Expand (R on BSP)"           $ tryMsgR (ExpandTowards R) (Expand))
    , ("M-S-["                  , addName "Expand (U on BSP)"           $ tryMsgR (ExpandTowards U) (MirrorShrink))
    , ("M-S-]"                  , addName "Expand (D on BSP)"           $ tryMsgR (ExpandTowards D) (MirrorExpand))

    , ("M-C-["                  , addName "Shrink (L on BSP)"           $ tryMsgR (ShrinkFrom R) (Shrink))
    , ("M-C-]"                  , addName "Shrink (R on BSP)"           $ tryMsgR (ShrinkFrom L) (Expand))
    , ("M-C-S-["                , addName "Shrink (U on BSP)"           $ tryMsgR (ShrinkFrom D) (MirrorShrink))
    , ("M-C-S-]"                , addName "Shrink (D on BSP)"           $ tryMsgR (ShrinkFrom U) (MirrorExpand))

  --, ("M-r"                    , addName "Mirror (BSP rotate)"         $ tryMsgR (Rotate) (XMonad.Layout.MultiToggle.Toggle MIRROR))
  --, ("M-S-C-m"                , addName "Mirror (always)"             $ sendMessage $ XMonad.Layout.MultiToggle.Toggle MIRROR)
  --, ("M4-r"                   , addName "BSP Rotate"                  $ sendMessage Rotate)

-- TODO: the following are potentially useful but I won't know till I work with BSP further
--    , ("M4-s"                   , addName "BSP Swap"                    $ sendMessage XMonad.Layout.BinarySpacePartition.Swap)
--    , ("M4-p"                   , addName "BSP Focus Parent"            $ sendMessage FocusParent)
--    , ("M4-n"                   , addName "BSP Select Node"             $ sendMessage SelectNode)
    --, ("M4-m"                   , addName "BSP Move Node"               $ sendMessage MoveNode)

    -- sublayout specific (unused)
    --  ("M4-C-S-."               , addName "toSubl Shrink"               $ toSubl Shrink)
    --, ("M4-C-S-,"               , addName "toSubl Expand"               $ toSubl Expand)
    ]
		where
			toggleCopyToAll = wsContainingCopies >>= \ws -> case ws of
							[] -> windows copyToAll
							_ -> killAllOtherCopies

    -----------------------------------------------------------------------
    -- Screens
    -----------------------------------------------------------------------
--    subKeys "Screens"
--    ([("M-C-<Right>", addName "Focus prev screen" prevScreen)
--    , ("M-C-<Left>" , addName "Focus next screen" nextScreen)
--    ]
--    ++ zipMod "Focus screen"                         screenKeys [0..] "M-"    (screenAction W.view)
--    ++ zipMod "Move client to screen"                screenKeys [0..] "M-S-"  (screenAction W.shift)
--    ++ zipMod "Swap workspace with screen"           screenKeys [0..] "M-M1-" (screenAction W.greedyView)
--    ++ zipMod "Swap workspace with and focus screen" screenKeys [0..] "M-C-"  (\s -> screenAction W.greedyView s >> screenAction W.view s)
--    ) ^++^

--    subKeys "Media Controls"
--    [
--    ("<XF86AudioMicMute>"      , addName "Mic Mute"                    $ spawn "notify-send mic mute")
--    ]
    

-- Mouse bindings: default actions bound to mouse events
-- Includes window snapping on move/resize using X.A.FloatSnap
-- Includes window w/h ratio constraint (square) using X.H.ConstrainedResize
myMouseBindings (XConfig {XMonad.modMask = myModMask}) = M.fromList $

    [ ((myModMask,               button1) ,(\w -> focus w
      >> mouseMoveWindow w
      >> ifClick (snapMagicMove (Just 50) (Just 50) w)
      >> windows W.shiftMaster))

    , ((myModMask .|. shiftMask, button1), (\w -> focus w
      >> mouseMoveWindow w
      >> ifClick (snapMagicResize [L,R,U,D] (Just 50) (Just 50) w)
      >> windows W.shiftMaster))

    , ((myModMask,               button3), (\w -> focus w
      >> mouseResizeWindow w
      >> ifClick (snapMagicResize [R,D] (Just 50) (Just 50) w)
      >> windows W.shiftMaster))

    , ((myModMask .|. shiftMask, button3), (\w -> focus w
      >> Sqr.mouseResizeWindow w True
      >> ifClick (snapMagicResize [R,D] (Just 50) (Just 50) w)
      >> windows W.shiftMaster ))

--    , ((mySecondaryModMask,      button4), (\w -> focus w
--      >> prevNonEmptyWS))
--
--    , ((mySecondaryModMask,      button5), (\w -> focus w
--      >> nextNonEmptyWS))

    ]

------------------------------------------------------------------------}}}
-- Startup                                                              {{{
---------------------------------------------------------------------------

myStartupHook = do

    -- init-tilingwm sets up all major "desktop environment" like components
    -- spawnOnce "$HOME/bin/wm/init-tilingwm"
    -- spawn "/home/peel/bin/wm/init-tilingwm"
    spawn "$HOME/bin/wm/init-wallpaper"

    -- init-tray kills and restarts stalone tray, hence just "spawn" so it
    -- runs on restart and will suffice to reposition tray on display changes
    -- TODO: evaluate moving to a "restart tray only" option on display change
    spawn     "$HOME/bin/wm/init-tray"

    spawn     "xfce4-power-manager &"
    spawn     "$HOME/.screenlayout/home.sh"

    setDefaultCursor xC_left_ptr

quitXmonad :: X ()
quitXmonad = io (exitWith ExitSuccess)

rebuildXmonad :: X ()
rebuildXmonad = do
    spawn "xmonad --recompile && xmonad --restart"

restartXmonad :: X ()
restartXmonad = do
    spawn "xmonad --restart"

------------------------------------------------------------------------}}}
-- Log                                                                  {{{
---------------------------------------------------------------------------

myLogHook h = do

    -- following block for copy windows marking
    copies <- wsContainingCopies
    let check ws | ws `elem` copies =
                   pad . xmobarColor yellow red . wrap "*" " "  $ ws
                 | otherwise = pad ws

    fadeWindowsLogHook myFadeHook
    ewmhDesktopsLogHook
    --dynamicLogWithPP $ defaultPP
    dynamicLogWithPP $ def

        { ppCurrent             = xmobarColor active "" . wrap "[" "]"
        , ppTitle               = xmobarColor active "" . shorten 50
        , ppVisible             = xmobarColor base0  "" . wrap "(" ")"
        , ppUrgent              = xmobarColor red    "" . wrap " " " "
        , ppHidden              = check
        , ppHiddenNoWindows     = const ""
        , ppSep                 = xmobarColor red blue "  :  "
        , ppWsSep               = " "
        , ppLayout              = xmobarColor yellow ""
        , ppOrder               = id
        , ppOutput              = hPutStrLn h  
        , ppSort                = fmap 
                                  (namedScratchpadFilterOutWorkspace.)
                                  (ppSort def)
                                  --(ppSort defaultPP)
        , ppExtras              = [] }

myFadeHook = composeAll
    [ opaque -- default to opaque
    , isUnfocused --> opacity 0.85
    , (className =? "Terminator") <&&> (isUnfocused) --> opacity 0.9
    , (className =? "URxvt") <&&> (isUnfocused) --> opacity 0.9
    , fmap ("Google" `isPrefixOf`) className --> opaque
    , isDialog --> opaque 
    --, isUnfocused --> opacity 0.55
    --, isFloating  --> opacity 0.75
    ]

------------------------------------------------------------------------}}}
-- Actions                                                              {{{
---------------------------------------------------------------------------


---------------------------------------------------------------------------
-- Urgency Hook                                                            
---------------------------------------------------------------------------
-- from https://pbrisbin.com/posts/using_notify_osd_for_xmonad_notifications/
data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name     <- getName w
        Just idx <- fmap (W.findTag w) $ gets windowset

        safeSpawn "notify-send" [show name, "workspace " ++ idx]
-- cf https://github.com/pjones/xmonadrc


---------------------------------------------------------------------------
-- New Window Actions
---------------------------------------------------------------------------

-- https://wiki.haskell.org/Xmonad/General_xmonad.hs_config_tips#ManageHook_examples
-- <+> manageHook defaultConfig

myManageHook :: ManageHook
myManageHook =
        manageSpecific
    <+> manageDocks
    <+> namedScratchpadManageHook scratchpads
    <+> fullscreenManageHook
    <+> manageSpawn
    where
        manageSpecific = composeOne
            [ resource =? "desktop_window" -?> doIgnore
            , resource =? "stalonetray"    -?> doIgnore
            , resource =? "vlc"    -?> doFloat
            , resource =? trelloResource -?> doFullFloat
            , resource =? trelloWorkResource -?> doFullFloat
            , resource =? googleMusicResource -?> doFullFloat
            , resource =? plexResource -?> doCenterFloat
            , resource =? hangoutsResource -?> insertPosition End Newer
            , transience
            , isBrowserDialog -?> forceCenterFloat
            --, isConsole -?> forceCenterFloat
            , isRole =? gtkFile  -?> forceCenterFloat
            , isDialog -?> doCenterFloat
            , isRole =? "pop-up" -?> doCenterFloat
            , isInProperty "_NET_WM_WINDOW_TYPE"
                           "_NET_WM_WINDOW_TYPE_SPLASH" -?> doCenterFloat
            , resource =? "console" -?> tileBelowNoFocus
            , isFullscreen -?> doFullFloat
            , pure True -?> tileBelow ]
        isBrowserDialog = isDialog <&&> className =? myBrowserClass
        gtkFile = "GtkFileChooserDialog"
        isRole = stringProperty "WM_WINDOW_ROLE"
        -- insert WHERE and focus WHAT
        tileBelow = insertPosition Below Newer
        tileBelowNoFocus = insertPosition Below Older

---------------------------------------------------------------------------
-- X Event Actions
---------------------------------------------------------------------------

-- for reference, the following line is the same as dynamicTitle myDynHook
-- <+> dynamicPropertyChange "WM_NAME" myDynHook

-- I'm not really into full screens without my say so... I often like to
-- fullscreen a window but keep it constrained to a window rect (e.g.
-- for videos, etc. without the UI chrome cluttering things up). I can
-- always do that and then full screen the subsequent window if I want.
-- THUS, to cut a long comment short, no fullscreenEventHook
-- <+> XMonad.Hooks.EwmhDesktops.fullscreenEventHook

myHandleEventHook = docksEventHook
                <+> fadeWindowsEventHook
                <+> dynamicTitle myDynHook
                <+> handleEventHook def
                <+> XMonad.Layout.Fullscreen.fullscreenEventHook
    where
        myDynHook = composeAll
            [ isPersonalHangouts --> forceCenterFloat
            , isWorkHangouts --> insertPosition End Newer
            ]

---------------------------------------------------------------------------
-- Custom hook helpers
---------------------------------------------------------------------------

-- from:
-- https://github.com/pjones/xmonadrc/blob/master/src/XMonad/Local/Action.hs
--
-- Useful when a floating window requests stupid dimensions.  There
-- was a bug in Handbrake that would pop up the file dialog with
-- almost no height due to one of my rotated monitors.

forceCenterFloat :: ManageHook
forceCenterFloat = doFloatDep move
  where
    move :: W.RationalRect -> W.RationalRect
    move _ = W.RationalRect x y w h

    w, h, x, y :: Rational
    w = 1/3
    h = 1/2
    x = (1-w)/2
    y = (1-h)/2

-- I left this here because I want to explore using tags more
-- ... did I crib this from pjones config?
--
---- | If the given condition is 'True' then add the given tag name to
---- the window being mapped.  Always returns 'Nothing' to continue
---- processing other manage hooks.
--addTagAndContinue :: Query Bool -> String -> MaybeManageHook
--addTagAndContinue p tag = do
--  x <- p
--  when x (liftX . addTag tag =<< ask)
--  return Nothing


-- vim: ft=haskell:foldmethod=marker:expandtab:ts=4:shiftwidth=4
