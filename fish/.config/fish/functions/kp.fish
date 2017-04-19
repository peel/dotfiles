function kp -a service -d "list kuberentes pods"
  if test -n "$service"
    k get pods | awk -v service="$service-[0-9]+-.*" '$service ~ service { print $1 }' | head -1
  else
    k get pods
  end
end
