

# Color #


Shell ANSI coloring for Erlang.

## Usage

Color exports two sets of functions, color names and a generic formatting
function (`p`). Both functions takes IO data and returns IO data (they never
print anything themselves). The following colors are supported as functions:

* `black`
* `red`
* `green`
* `yellow`
* `blue`
* `purple`
* `cyan`
* `white`

They can be called directly, e.g. `color:yellow(IOData)`. The generic
formatting function `p` takes two arguments, IO data and options. In addition
to the colors above, it also supports:

* `light_black`
* `light_red`
* `light_green`
* `light_yellow`
* `light_blue`
* `light_purple`
* `light_cyan`
* `light_white`

It also supports several modes:

* `normal`
* `bold`
* `underline`
* `blink`

The options list can be any combination of these colors or modes.
To assign a background color, simple include two colors. The first will be used
as the foreground color, and the second will be used as the background color
(to set only the background color, include `inherit` as the foreground color).

Examples:

* `color:p("text", [yellow])` (same as `color:yellow("text")`)
* `color:p("text", [green, red, bold])`
* `color:p("text", [inherit, blue])`
* `color:p("text", [bold, underline])`


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="https://github.com/eproxus/color/blob/master/doc/color.md" class="module">color</a></td></tr></table>

