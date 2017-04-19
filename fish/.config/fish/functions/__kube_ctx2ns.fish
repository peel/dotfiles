function __kube_ctx2ns -a context
  switch $context
    case warp
      echo qa-auth
    case nwtprod
      echo auth
  end
end
