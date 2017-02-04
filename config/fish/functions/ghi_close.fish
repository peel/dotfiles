#!/usr/bin/bash

# Closes a github issue
# Example:
# ghi_close 1      # close issue #1
# ghi_close "aaaa" # open an issue "aaaa" and close it

function ghi_close() {
    if [[ -n "$1" ]]; then
        echo "Closing issue #${1}"
        ghi close "$1"
    else
        echo "Opening and closing issue: ${1}"
        ghi close $(ghi open -m "$1" -L "in progress" | head -n 1 | awk 'match($0, /[0-9]+/){print substr($0, RSTART, RLENGTH)}')
    fi
}

ghi_close $@
