#!/usr/bin/bash

# Closes an "epic" in github
# If a milestone exists, close it.
# Otherwise simply push changes.
#
# 1. Push changes
# 2. Close a milestone if exists
# 3. Open a pull request if not exists
# 4. Get back to master branch
# 5. Remove branch
# 6. Remove config
#
# Example:
# ghi_epic_close  # close currently open epic

function ghi_epic_close() {
    local MILESTONE=$(<.milestone)
    local PULLREQUEST=$(<.pullrequest)
    local BRANCH=$(git rev-parse --abbrev-ref HEAD)
    local TITLE=${BRANCH[2,50]//-/}
    if [[ -n $MILESTONE ]]; then
        echo "Pushing to origin/$BRANCH" &&
        git push -u origin $BRANCH &&
        if [[ ! -n $PULLREQUEST ]]; then
            echo "Creating a pull-request: $TITLE" &&
            local PULLREQUEST=$(hub pull-request -m "[wip] ${TITLE}" | awk 'match($0, /[0-9]+/){print substr($0, RSTART, RLENGTH)}') &&
            ghi edit $PULLREQUEST -M $MILESTONE &&
        fi
        echo "Closing a milestone $MILESTONE" &&
        ghi milestone -s closed $MILESTONE &&
        echo "Checking back master" &&
        git checkout master &&
        echo "Removing branch $BRANCH" &&
        git branch -d $BRANCH &&
        echo "Removing .milestone config" &&
        rm -f .milestone
    else
        git push
    fi
}

ghi_epic_close
