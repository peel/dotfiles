command: "
  if [[ -z /usr/local/bin/kwmc ]]; then
    export KWMC_PATH=/usr/local/bin/;
  else
    export KWMC_PATH=/run/current-system/sw/bin/;
  fi;
  echo $(${KWMC_PATH}kwmc query space active name)"

refreshFrequency: 1000 # ms

render: (output) ->
  """
  <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/font-awesome/4.6.1/css/font-awesome.min.css">
  <div class="ac"
    <span></span>
    <span class="icon"></span>
  </div>
  """

update: (output, el) ->
    $(".ac span:first-child", el).text("  #{output}")
    $icon = $(".ac span.icon", el)
    $icon.removeClass().addClass("icon")
    $icon.addClass("fa #{@icon(output)}")

icon: (status) =>
    return if status.substring(0, 4) == "main"
        "fa-home"
    else if status.substring(0, 3) == "web"
        "fa-safari"
    else if status.substring(0, 3) == "rnd"
        "fa-random"
    else if status.substring(0, 5) == "games"
        "fa-gamepad"
    else
        "fa-times"

style: """
  -webkit-font-smoothing: antialiased
  text-align: right
  color: #98d1ce
  font: 10px "Pragmata Pro"
  height: 16px
  overflow: hidden
  text-overflow: ellipsis
  right: 320px
  top: 6px
  width: 50%
"""
