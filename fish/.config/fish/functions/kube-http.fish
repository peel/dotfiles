#!/usr/bin/env fish

# Executes CURL GET without logging into container's bash
#
# Example:
# kube-http service-gateway/profiles/1

function kube-http -a url
  set HOST (echo $url | egrep -o '^[^/]+')
  echo "curl -sS $url 2> /dev/null" | k8E $HOST bash ^ /dev/null | jq
end
