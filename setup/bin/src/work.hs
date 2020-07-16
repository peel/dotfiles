{-# LANGUAGE OverloadedStrings #-}

import Turtle
import qualified Control.Foldl as Fold
import Prelude hiding (FilePath)
import Data.List
import qualified Data.Text as T (intercalate, empty) 

apps = [ "Dash", "Docker", "Endel", "Focus", "Noizio" ]
clis = [ "open focus://focus?minutes=25", "emacsclient -a '' -nc" ]

path app = "/Applications/" <> app <> ".app"

run cmd apps =
  (select apps) >>= exec
  where exec a = inshell (cmd <> " " <> a) empty

active = inshell "ps aux | rg '.+~?/Applications/((\\w|\\s|\\d)+)\\.app\\W?.*' -r '$1' | sort -u" empty
managedApps active = (filter (\a -> a `elem` active))

tell str apps = inshell msg empty
  where
    msg = "osascript -e 'display notification \"" <> str <> ": " <> names <> "\" with title \"Focus Mode\"'"
    names = T.intercalate (", ") apps
  
parser :: Parser (FilePath)
parser = argPath "src" "The source file"

main = sh (do
  toggle <- options "Setup work env" parser
  as <- fold active Fold.list

  let running = managedApps (lineToText <$> as) apps
  let toRun = apps \\ running

  stdout $ case toggle of
    "off" -> do
      sh $ run "pkill" running
      tell "Stopped" running
    "on" -> do
      sh $ run "open -gj" (path <$> toRun)
      sh $ run T.empty clis
      tell "Running" (toRun <> clis))
