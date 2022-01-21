import XMonad
import XMonad.Util.SpawnOnce

myStartupHook = do
  spawnOnce "emacsclient -c"

main = xmonad defaultConfig
        { terminal = "emacsclient -c --eval '(vterm)'",
          startupHook = myStartupHook
        }
