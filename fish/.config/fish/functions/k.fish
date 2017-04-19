function k -d "kubectl with namespace and context"
  kubectl -n (__kube_mapctx (kc)) $argv
end
