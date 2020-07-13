{-# LANGUAGE OverloadedStrings #-}

import Turtle
import Prelude hiding (FilePath)
import qualified Data.Text (empty)

data App = App { name :: String, path :: String }
         | Cmd { name :: String, path :: String, args :: [String] } deriving Show

paths = fmap (\(App{name=n, path=p}) -> fromString (p <> "/" <> n <> ".app"))
names = fmap (\(App{name=n, path=_}) -> fromString n)

apps =
  [ App { name = "Dash",   path = "/Applications" }
  , App { name = "Docker", path = "/Applications" }
  , App { name = "Focus",  path = "/Applications" }
  , App { name = "Firefox\\ Developer\\ Edition", path = "/Applications" }
  ] ++ -- temporary
  [ App { name = "Canary\\ Mail", path = "/Applications" } -- notmuch/mbsync pm
  ]

clis =
  [ App { name = "focus",  path = "open focus://focus?minutes=25" }
  , App { name = "emacsclient",  path = "emacsclient -a \'\' -nc" }
  ]    

run cmd fn apps =
  (select $ fn apps) >>= exec
  where exec a = inshell (cmd <> " " <> a <> " -gj") empty
  
start = run "open" paths
kill = run "pkill" names
cli = run Data.Text.empty paths
tell str = inshell msg empty
  where msg = "osascript -e 'display notification \"" <> str <> "\" with title \"Focus Mode\"'"
  
parser :: Parser (FilePath)
parser = argPath "src" "The source file"

main = sh (do
  mState <- options "Setup work env" parser
  stdout $ case mState of
    "off" -> do
      sh $ kill (apps ++ clis)
      tell "Stopped..."
    "on"  -> do
      sh $ start apps
      sh $ cli clis
      tell "Focusing...")
