command: "
  if [[ -z /usr/local/bin/kwmc ]]; then
    export KWMC_PATH=/usr/local/bin/;
  else
    export KWMC_PATH=/run/current-system/sw/bin/;
  fi;
  echo $(${KWMC_PATH}kwmc query space active tag)"

refreshFrequency: 1000 # ms

render: (output) ->
  """
  <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/font-awesome/4.6.1/css/font-awesome.min.css">
  <div class="foc"
    <span></span>
    <span class="icon"></span>
  </div>
  """

update: (output, el) ->
    $(".foc span:first-child", el).text("  #{output}")
    $icon = $(".foc span.icon", el)
    $icon.removeClass().addClass("icon")
    $icon.addClass("fa fa-bars")

style: """
  -webkit-font-smoothing: antialiased
  color: #98d1ce
  font: 10px "Pragmata Pro"
  height: 16px
  left: 10px
  overflow: hidden
  text-overflow: ellipsis
  top: 6px
  width: auto
"""
