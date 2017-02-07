#!/usr/bin/env fish

# Open an issue in each project
#
# Example:
# ghi_opens "some issue"

function ghi-opens -a message -d "Open an issue in each project"
  set -l cmd (echo "ghi open -m \"[\$_1] $message\"")
  projects $cmd
end

ghi-opens $argv
