{ exec, ... }: {
  pass = name: exec [./pass.sh name];
}
