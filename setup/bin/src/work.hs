{-# LANGUAGE OverloadedStrings #-}

import Turtle
import Prelude hiding (FilePath)
import qualified Data.Text (empty)

data App = App { name :: Text, path :: Text }
         | Cmd { name :: Text, path :: Text, args :: [Text] } deriving Show

app = fmap (\(App{name=n, path=p}) -> p <> "/" <> n <> ".app")
paths apps = fmap path apps
names apps = fmap name apps

apps =
  [ App { name = "Dash",   path = "/Applications" }
  , App { name = "Docker", path = "/Applications" }
  , App { name = "Focus",  path = "/Applications" }
  , App { name = "Firefox\\ Developer\\ Edition", path = "/Applications" }
  , App { name = "Endel", path = "/Applications" }
  , App { name = "Noizio", path = "/Applications" }
  ] ++ -- temporary
  [ App { name = "Canary\\ Mail", path = "/Applications" } -- notmuch/mbsync pm
  ]

clis =
  [ App { name = "focus",  path = "open focus://focus?minutes=25" }
  , App { name = "emacsclient",  path = "emacsclient -a '' -nc" }
  ]

run cmd fn apps =
  (select $ fn apps) >>= exec
  where exec a = inshell (cmd <> " " <> a) empty
  
start = run "open -gj" app
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
      -- filter active, open -gj does not really work
      sh $ start apps
      sh $ cli clis
      tell "Focusing...")
