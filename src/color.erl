-module(color).

% API
-export([p/2]).

-export([black/1]).
-export([black/2]).

-export([red/1]).
-export([red/2]).

-export([green/1]).
-export([green/2]).

-export([yellow/1]).
-export([yellow/2]).

-export([blue/1]).
-export([blue/2]).

-export([purple/1]).
-export([purple/2]).

-export([cyan/1]).
-export([cyan/2]).

-export([white/1]).
-export([white/2]).

-define(reset, <<"\e[0m">>).

-define(foreground(Color),
    Color(Text) -> [<<"\e[0;">>, foreground(Color), <<"m">>, Text, ?reset]
).

-type opt() :: black | red | green | yellow | blue | purple | cyan | white |
    light_black | light_red | light_green | light_yellow | light_blue |
    light_purple | light_cyan | light_white | normal | bold | underline |
    blink | inherit.

%--- API ----------------------------------------------------------------------

% @doc Format and color `IOData' according to `Opts'.
-spec p(iodata(), [opt()]) -> iodata().
p(IOData, Opts) ->
    {Modes, Colors} = parse(Opts),
    [<<"\e[">>, Modes, Colors, <<"m">>, IOData, ?reset].

% @doc Color the text black.
-spec black(iodata()) -> iodata().
black(IOData)  -> p(IOData, [black]).

% @doc Color the format-text black.
-spec black(string(), [term()]) -> iodata().
black(FormatString, FormatArgs)  -> black(io_lib:format(FormatString, FormatArgs)).

% @doc Color the text red.
-spec red(iodata()) -> iodata().
red(IOData)    -> p(IOData, [red]).

% @doc Color the format-text red.
-spec red(string(), [term()]) -> iodata().
red(FormatString, FormatArgs)  -> red(io_lib:format(FormatString, FormatArgs)).

% @doc Color the text green.
-spec green(iodata()) -> iodata().
green(IOData)  -> p(IOData, [green]).

% @doc Color the format-text green.
-spec green(string(), [term()]) -> iodata().
green(FormatString, FormatArgs)  -> green(io_lib:format(FormatString, FormatArgs)).

% @doc Color the text yellow.
-spec yellow(iodata()) -> iodata().
yellow(IOData) -> p(IOData, [yellow]).

% @doc Color the format-text yellow.
-spec yellow(string(), [term()]) -> iodata().
yellow(FormatString, FormatArgs)  -> yellow(io_lib:format(FormatString, FormatArgs)).

% @doc Color the text blue.
-spec blue(iodata()) -> iodata().
blue(IOData)   -> p(IOData, [blue]).

% @doc Color the format-text blue.
-spec blue(string(), [term()]) -> iodata().
blue(FormatString, FormatArgs)  -> blue(io_lib:format(FormatString, FormatArgs)).

% @doc Color the text purple.
-spec purple(iodata()) -> iodata().
purple(IOData) -> p(IOData, [purple]).

% @doc Color the format-text purple.
-spec purple(string(), [term()]) -> iodata().
purple(FormatString, FormatArgs)  -> purple(io_lib:format(FormatString, FormatArgs)).

% @doc Color the text cyan.
-spec cyan(iodata()) -> iodata().
cyan(IOData)   -> p(IOData, [cyan]).

% @doc Color the format-text cyan.
-spec cyan(string(), [term()]) -> iodata().
cyan(FormatString, FormatArgs)  -> cyan(io_lib:format(FormatString, FormatArgs)).

% @doc Color the text white.
-spec white(iodata()) -> iodata().
white(IOData)  -> p(IOData, [white]).

% @doc Color the format-text white.
-spec white(string(), [term()]) -> iodata().
white(FormatString, FormatArgs)  -> white(io_lib:format(FormatString, FormatArgs)).

%--- Internal Functions -------------------------------------------------------

parse(Opts) -> parse(Opts, {mode(normal), <<>>}).

parse([], Result) ->
    Result;
parse([Mode|Opts], {<<>>, Colors})
  when Mode == normal; Mode == bold; Mode == underline ->
    parse(Opts, {mode(Mode), Colors});
parse([Mode|Opts], {Modes, Colors})
  when Mode == normal; Mode == bold; Mode == underline ->
    Bin = mode(Mode),
    parse(Opts, {<<Modes/binary, ";", Bin/binary>>, Colors});
parse([Color|Opts], {Mode, <<>>}) ->
    Bin = foreground(Color),
    parse(Opts, {Mode, <<";", Bin/binary>>});
parse([Color|Opts], {Mode, Colors}) ->
    Bin = background(Color),
    parse(Opts, {Mode, <<Colors/binary, ";", Bin/binary>>}).

% Color_Off='\e[0m'       # Text Reset

mode(normal)    -> <<"0">>;
mode(bold)      -> <<"1">>;
mode(underline) -> <<"4">>;
mode(blink)     -> <<"5">>;
mode(_)         -> error(invalid_mode).

foreground(inherit)      -> <<"0">>;
foreground(black)        -> <<"30">>;
foreground(red)          -> <<"31">>;
foreground(green)        -> <<"32">>;
foreground(yellow)       -> <<"33">>;
foreground(blue)         -> <<"34">>;
foreground(purple)       -> <<"35">>;
foreground(cyan)         -> <<"36">>;
foreground(white)        -> <<"37">>;
foreground(light_black)  -> <<"90">>;
foreground(light_red)    -> <<"91">>;
foreground(light_green)  -> <<"92">>;
foreground(light_yellow) -> <<"93">>;
foreground(light_blue)   -> <<"94">>;
foreground(light_purple) -> <<"95">>;
foreground(light_cyan)   -> <<"96">>;
foreground(light_white)  -> <<"97">>;
foreground(_)            -> error(invalid_foreground_color).

background(black)        -> <<"40">>;
background(red)          -> <<"41">>;
background(green)        -> <<"42">>;
background(yellow)       -> <<"43">>;
background(blue)         -> <<"44">>;
background(purple)       -> <<"45">>;
background(cyan)         -> <<"46">>;
background(white)        -> <<"47">>;
background(light_black)  -> <<"100">>;
background(light_red)    -> <<"101">>;
background(light_green)  -> <<"102">>;
background(light_yellow) -> <<"103">>;
background(light_blue)   -> <<"104">>;
background(light_purple) -> <<"105">>;
background(light_cyan)   -> <<"106">>;
background(light_white)  -> <<"107">>;
background(_)            -> error(invalid_background_color).
