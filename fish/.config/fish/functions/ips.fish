#!/usr/bin/bash

function ips
  nmap -sP (ipconfig getifaddr en0)/24
end
