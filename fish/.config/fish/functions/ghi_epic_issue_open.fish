#!/usr/bin/bash

# Opens an issue on github
# If a milestone exists, attach issue to to the milestone.
# Otherwise simply open an issue.
#
# Example:
# ghi_epic_issue_open "some title"

function ghi_epic_issue_open() {
    local MILESTONE=$(<.milestone)
    if [[ -n $MILESTONE ]]; then
        echo "Attaching issue to $MILESTONE" &&
        ghi open $1 -M $MILESTONE
    else
        ghi open $1
    fi
}

ghi_epic_issue_open $@
