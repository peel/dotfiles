function kl -a service -d "stream kubernetes logs"
  k logs -f (kp $service)
end
