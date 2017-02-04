#!/usr/bin/bash

# Mark an issue as in progress in github
#
# Example:
# ghi_now 1            # set issue #1 as in-progress
# ghi_now "some issue" # open a new issue and mark as in-progress

function ghi_now() {
    if [[ -n "$1" ]]; then
        echo "Setting $1 as in-progress" &&
        ghi label "$1" -a "in progress" &&
        echo "Assigning $1 to self" &&
        ghi assign $1 -u $(git config user.name)
    else
        echo "Setting $1 as in-progress" &&
        ghi open -m "$1" -L "in progress" | xargs -0 bash -c 'for id; do git checkout -b "f-issue-$id"' &&
        echo "Assigning $1 to self" &&
        ghi assign $1 -u $(git config --global user.name)
    fi
}

ghi_now $@
