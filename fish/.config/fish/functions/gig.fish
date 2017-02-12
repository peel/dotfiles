#!/usr/bin/env fish

# Example
# git osx,scala
# ls -l
#  .gitignore


function gig -a templates -d "Generates a gitignore file from comma-delimited keywords"
  curl -Ls "https://www.gitignore.io/api/$templates" >> .gitignore
end

gi "$argv"
