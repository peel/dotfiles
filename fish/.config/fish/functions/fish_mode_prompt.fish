function fish_mode_prompt --description "Display the vi mode for the prompt"
  # Do nothing if not in vi mode
  if test "$fish_key_bindings" = "fish_vi_key_bindings"
    switch $fish_bind_mode
      case default
        set_color --bold cyan
        echo '·'
      case insert
        set_color --bold cyan
        echo '›'
      case replace-one
        set_color --bold red
        echo '‹'
      case visual
        set_color --bold grey
        echo '‹'
    end
    set_color normal
    echo -n ' '
  end
end
