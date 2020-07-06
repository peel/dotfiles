{-# LANGUAGE OverloadedStrings #-}

import Turtle
import Prelude hiding (FilePath)
import qualified Data.Text (empty)

data App = App { name :: String, path :: String } deriving Show

paths :: [App] -> [Text]
paths = fmap (\(App{name=n, path=p}) -> fromString (p <> "/" <> n <> ".app"))

names :: [App] -> [Text]
names = fmap (\(App{name=n, path=_}) -> fromString n)

apps :: [App]
apps =
  [ App { name = "Slack",  path = "/Applications" } -- temp (irccloud)
  , App { name = "Dash",   path = "/Applications" }
  , App { name = "Docker", path = "/Applications" }
  , App { name = "Focus",  path = "/Applications" }
  , App { name = "Canary Mail", path = "/Applications" } -- temporary (notmuch/mbsync pm)
  , App { name = "Calendar", path = "/Applications" } -- temporary (calfw)
  ]
  
clis :: [App]
clis = [ App { name = "focus",  path = "open focus://focus?minutes=25" }
       , App { name = "emacsclient",  path = "emacsclient -a '' -nc" }
       ]    

run :: Text -> ([App] -> [Text]) -> [App] -> Shell ()
run cmd fn apps = do
  a <- select $ fn apps
  stdout $ inshell (cmd <> " " <> a) empty

start = run "open" paths
kill = run "pkill" names
cli = run Data.Text.empty paths

parser :: Parser (FilePath)
parser = argPath "src" "The source file"

main = sh (do
  mState <- options "Setup work env" parser
  case mState of
    "off" ->
      kill clis
    "on"  ->
      cli clis)
