function ke -a service -d "exec command via kubernetes"
  k exec -it (kp $service)
end
