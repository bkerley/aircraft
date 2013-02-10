-module(channel).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-author("bkerley@brycekerley.net").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record (state, {name, users, topic}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
  UserDict = dict:new(),
  Topic = "",
  case normalize_channel_name(proplists:get_value(name, Args)) of
    Name when is_binary(Name) ->
      {ok, #state{name = Name, users = UserDict, topic = Topic}};
    _ ->
      {stop, "Missing or invalid channel name."}
  end.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

normalize_channel_name(OriginalName) when is_list(OriginalName) ->
  normalize_channel_name(list_to_binary(OriginalName));
normalize_channel_name(OriginalName) when is_binary(OriginalName) ->
  validate_channel_name(OriginalName);
normalize_channel_name(_) ->
  badarg.

validate_channel_name(BinaryName) ->
  % regexp from sam stephenson's hector
  % https://github.com/sstephenson/hector/blob/master/lib/hector/channel.rb

  {ok, Re} = re:compile("^[#&+!][#&+!\\-\\w\\p{L}\\p{M}\\p{N}\\p{S}\\p{P}\\p{Co}]{1,49}$", [unicode]),
  case re:run(BinaryName, Re) of
    {match, _} -> BinaryName;
    _ -> invalid
  end.

%% tests
-ifdef(TEST).

-define(should_be_badarg(Name), 
  ?_assertEqual(badarg, normalize_channel_name(Name))).
-define(should_be_valid(Name),
  ?LET(NormalizedName, normalize_channel_name(Name), 
  [
   ?_assertEqual(NormalizedName, NormalizedName),
   ?_assertNotEqual(invalid, NormalizedName),
   ?_assertNotEqual(badarg, NormalizedName)
  ])).
-define(should_be_invalid(Name),
  ?_assertEqual(invalid, normalize_channel_name(Name))).

channel_name_test_() ->
  [
   % types
   ?should_be_badarg(5),
   ?should_be_badarg(toot),
   % actual channels
   ?should_be_valid(<<"#aesthetes">>),
   ?should_be_valid("#rubygems-trust"),
   ?should_be_invalid("butt channel"),
   ?should_be_invalid(<<"butt channel">>),
   % starting characters
   ?should_be_valid("+test"),
   ?should_be_valid("&test"),
   ?should_be_valid("!test"),
   ?should_be_invalid("@test"),
   % prefix characters
   ?should_be_valid("##"),
   ?should_be_valid("#test#"),
   ?should_be_valid("#&"),
   ?should_be_valid("++&#"),
   ?should_be_valid("!te&t"),
   ?should_be_valid("#8*(&x")
  ].
-endif.
