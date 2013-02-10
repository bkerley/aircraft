-module (irc_parse).
-author ("bkerley@brycekerley.net").
-export ([irc_parse/1]).
-include_lib("eunit/include/eunit.hrl").
-include ("irc.hrl").

irc_parse(Mesg) when is_binary(Mesg) ->
	irc_parse(binary_to_list(Mesg));
irc_parse(Mesg) when is_list(Mesg)->
	Trim = string:strip(string:strip(Mesg, right, $\n), right, $\r),
	irc_parse(start, #ircmesg{}, Trim).

irc_parse(start, Accum, Mesg) ->
	[Pcheck|Remain] = Mesg,
	if
		Pcheck == $: ->
			irc_parse(prefix, Accum, Remain);
		true ->
			irc_parse(command, Accum, Mesg)
	end;


irc_parse(prefix, _, "") ->
	%never left the prefix
	throw({badmessage, "The IRC message had no command - just a prefix."});
irc_parse(prefix, Accum, [$ |Remain]) -> % space ends prefix
	BackPfx = Accum#ircmesg.prefix,
	FwdPfx = lists:reverse(BackPfx),
	NewAccum = Accum#ircmesg{prefix=FwdPfx},
	irc_parse(space_command, NewAccum, Remain);
irc_parse(prefix, Accum, [Cur|Remain]) -> % continue eating prefix
	OldPrefix = Accum#ircmesg.prefix,
	NewPrefix = [Cur|OldPrefix],
	NewAccum = Accum#ircmesg{prefix=NewPrefix},
	irc_parse(prefix, NewAccum, Remain);

irc_parse(space_command, Accum, [$ |Remain]) -> % continue eating spaces
	irc_parse(space_command, Accum, Remain);
irc_parse(space_command, Accum, Mesg) -> % onwards to message
	irc_parse(command, Accum, Mesg);

irc_parse(command, Accum, "") ->
	reverse_cmd(Accum);
irc_parse(command, Accum, [$ |Remain]) -> % space ends command, continue to params
	irc_parse(space_params, reverse_cmd(Accum), Remain);
irc_parse(command, Accum, [Cur|Remain]) -> % continue eating command
	OldCommand = Accum#ircmesg.command,
	NewCommand = [Cur|OldCommand],
	NewAccum = Accum#ircmesg{command=NewCommand},
	irc_parse(command, NewAccum, Remain);

irc_parse(space_params, Accum, "") ->
	Accum;
irc_parse(space_params, Accum, [$ |Remain]) ->
	irc_parse(space_params, Accum, Remain);
irc_parse(space_params, Accum, Mesg) ->
	Params = params_parse(Mesg),
	Accum#ircmesg{params=Params}.

params_parse(Mesg) ->
	params_parse([], [], Mesg).

%end of string
params_parse(Accum, [], []) ->
	lists:reverse(Accum);
params_parse(Accum, Cur, []) ->
	params_parse([lists:reverse(Cur)|Accum], [], []);

%end of param
params_parse(Accum, [], [$ |Remain]) ->
	params_parse(Accum, [], Remain);
params_parse(Accum, Cur, [$ |Remain]) ->
	params_parse([lists:reverse(Cur)|Accum], [], Remain);

%remain
params_parse(Accum, [], [$:|Remain]) ->
	params_parse([Remain|Accum], [], []);

%normal
params_parse(Accum, Cur, [First|Rest]) ->
	params_parse(Accum, [First|Cur], Rest).

reverse_cmd(Rec) ->
	Back = Rec#ircmesg.command,
	Fwd = lists:reverse(Back),
	Rec#ircmesg{command=Fwd}.

% TESTING
mode_parse() -> irc_parse(":bonz2 MODE bonz2 :+x").

mode_prefix_test() ->
	Acc = mode_parse(),
	"bonz2" = Acc#ircmesg.prefix.
mode_command_test() ->
	Acc = mode_parse(),
	"MODE" = Acc#ircmesg.command.
mode_params_test() ->
	Acc = mode_parse(),
	["bonz2", "+x"] = Acc#ircmesg.params.

motd_test() ->
	Acc =  irc_parse(":chatbus.irc 372 bonz2 :- <@hello> WELL YOU'D BETTER GO CATCH IT                         |\r\n"),
	#ircmesg{
		prefix="chatbus.irc",
		command="372",
		params=[
			"bonz2",
			"- <@hello> WELL YOU'D BETTER GO CATCH IT                         |"
	]} = Acc.

swingler_test() ->
	Acc = irc_parse(":swingler_!root@408DA930.6B34A88.81D249CA.IP PRIVMSG #onebutan :ahahaha\r\n"),
	#ircmesg{
		prefix="swingler_!root@408DA930.6B34A88.81D249CA.IP",
		command="PRIVMSG",
		params=["#onebutan", "ahahaha"]
	} = Acc.

privmsg_test() ->
	Acc = irc_parse("PRIVMSG #onebutan have to scroll to that part of the rfc, hang on\r\n"),
	#ircmesg{
		command="PRIVMSG",
		params=["#onebutan", "have", "to", "scroll", "to", "that", "part", "of", "the", "rfc,", "hang", "on"]
	} = Acc.

binary_test() ->
	Acc = irc_parse(<<"QUIT l8r\r\n">>),
	#ircmesg{command="QUIT", params=["l8r"]} = Acc.