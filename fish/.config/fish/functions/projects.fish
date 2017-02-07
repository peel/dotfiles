#!/usr/bin/env fish

# Execute command for all projects in directory
#
# Example:
# projects sbt "project $1" "~test"

function projects -a lambda -d "Executes function for all projects in directory"
  # list subdirectories containing 'src' subdir, excluding start dir
  set src_dirs (find $PWD ! -path $PWD -maxdepth 1 -type d -exec test -d '{}/src' \; -print)
  echo $lambda
  if test -z $lambda
    map '$_1' $src_dirs
  else
    each $lambda $src_dirs
  end
end

# projects $argv
