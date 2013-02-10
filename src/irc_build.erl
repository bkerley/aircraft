-module (irc_build).
-author ("bkerley@brycekerley.net").
-export ([irc_build/1, message/1]).
-include_lib("eunit/include/eunit.hrl").
-include ("irc.hrl").

message(Accum) when is_record(Accum, ircmesg) ->
	irc_build(Accum) ++ "\r\n".

irc_build(Accum) when is_record(Accum, ircmesg) ->
	SpacedParams = join_params(Accum),
	SpacedPrefix = join_prefix(Accum),
	Command = join_command(Accum),
	SpacedPrefix ++ Command ++ " " ++ SpacedParams.

join_prefix(Accum) when is_record(Accum, ircmesg) ->
	join_prefix(Accum#ircmesg.prefix);
join_prefix([]) -> [];
join_prefix(Prefix) when is_list(Prefix) -> ":" ++ Prefix ++ " ".

join_command(Accum) when is_record(Accum, ircmesg) ->
	join_command(Accum#ircmesg.command);
join_command(Cmd) when is_integer(Cmd) ->
	integer_to_list(Cmd);
join_command(Cmd) when is_list(Cmd) ->
	Cmd.

join_params(Accum) when is_record(Accum, ircmesg) ->
	join_params(Accum#ircmesg.params);
join_params(Params) when is_list(Params) ->
	join_spaced(lists:reverse(join_params([], Params))).

join_params(Accum, []) ->
	Accum;
join_params(Accum, [Car|Cdr]) ->
	Words = string:words(Car),
	if
		Words > 1 ->
			[[$:|join_spaced([Car|Cdr])]|Accum];
		true ->
			join_params([Car|Accum], Cdr)
	end.

join_spaced(Remains) ->
	join_spaced([], lists:reverse(Remains)).

join_spaced(Accum, []) -> Accum;
join_spaced([], [Car|Cdr]) ->
	join_spaced(Car, Cdr);
join_spaced(Accum, [Car|Cdr]) ->
	join_spaced(Car ++ " " ++ Accum, Cdr).

swingler_test() ->
	Built = irc_build(#ircmesg{
		prefix="swingler_!root@408DA930.6B34A88.81D249CA.IP",
		command="PRIVMSG",
		params=["#onebutan","Your quote has been pushed into the queue as #2507."]}),
	":swingler_!root@408DA930.6B34A88.81D249CA.IP PRIVMSG #onebutan :Your quote has been pushed into the queue as #2507." = Built.

outbound_test() ->
	Built = irc_build(#ircmesg{
		command="PRIVMSG",
		params=["#onebutan","oh","that's","in","th","ctcp","spec"]
	}),
	"PRIVMSG #onebutan oh that's in th ctcp spec" = Built.

chatbus_test() ->
	Built = irc_build(#ircmesg{
		prefix="chatbus.irc",
		command=375,
		params=["bonz2","- chatbus.irc Message of the Day - "]
	}),
	":chatbus.irc 375 bonz2 :- chatbus.irc Message of the Day - " = Built.