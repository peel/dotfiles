import XMonad

myStartupHook = do
  spawnOnce "emacsclient -c"

main = xmonad defaultConfig
        { terminal = "emacsclient -c --eval '(vterm)'"
          startupHook = myStartupHook
        }
