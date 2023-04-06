import XMonad
import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig(additionalKeys)

myStartupHook = do
  spawnOnce "emacsclient -c"

myLauncher = "rofi -combi-modi window,drun,run -show combi -modi combi"

main = xmonad $ defaultConfig
        { terminal = "emacsclient -c --eval '(vterm)'"
        , startupHook = myStartupHook
        , modMask = mod1Mask
        } `additionalKeys`
        [ ((mod1Mask, xK_p), spawn myLauncher)
        ]
