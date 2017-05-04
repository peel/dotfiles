function ke -a service -a cmd -d "exec command via kubernetes"
  k exec -it (kp $service) -- $cmd
end
