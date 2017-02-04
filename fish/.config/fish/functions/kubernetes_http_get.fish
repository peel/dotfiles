#!/usr/bin/bash

# Executes CURL GET without logging into container's bash
#
# Example:
# kubernetes_curl_get service-gateway/profiles/1

function kubernetes_http_get(){
    local HOST=$(egrep -o '^[^/]+' <<< $1)
    echo curl -sS $1 2> /dev/null | tee >(k8E $HOST bash)
}

kubernetes_http_get $@
