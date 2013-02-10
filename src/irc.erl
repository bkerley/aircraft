-module (irc).
-export ([parse/1, build/1]).
-import (irc_build, [message/1]).
-import (irc_parse, [irc_parse/1]).
-include ("irc.hrl").

parse(Mesg) -> irc_parse(Mesg).
build(Mesg) -> message(Mesg).