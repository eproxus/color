-module(color).

% API
-export([p/2]).

-export([black/1]).
-export([red/1]).
-export([green/1]).
-export([yellow/1]).
-export([blue/1]).
-export([purple/1]).
-export([cyan/1]).
-export([white/1]).

-define(reset, <<"\e[0m">>).

-define(foreground(Color),
    Color(Text) -> [<<"\e[0;">>, foreground(Color), <<"m">>, Text, ?reset]
).

%--- API ----------------------------------------------------------------------

p(IOData, Opts) ->
    {Modes, Colors} = parse(Opts),
    [<<"\e[">>, Modes, Colors, <<"m">>, IOData, ?reset].

?foreground(black).
?foreground(red).
?foreground(green).
?foreground(yellow).
?foreground(blue).
?foreground(purple).
?foreground(cyan).
?foreground(white).

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
