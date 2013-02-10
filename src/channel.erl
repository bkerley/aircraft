-module(channel).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

--record (state, {name, users, topic}).

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
  end
  {ok, #state{users = UserDict, topic = Topic}}.

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

normalize_channel_name(OriginalName) ->
  case OriginalName
    when is_string(OriginalName) ->
      normalize_channel_name(list_to_binary(OriginalName));
    when is_binary(OriginalName) ->
      validate_channel_name(OriginalName);
    _ ->
      invalid.

validate_channel_name(BinaryName) ->
  % regexp from sam stephenson's hector
  % https://github.com/sstephenson/hector/blob/master/lib/hector/channel.rb

  Re = re:compile("^[#&+!][#&+!\-\w\p{L}\p{M}\p{N}\p{S}\p{P}\p{Co}]{1,49}$", [unicode]),
  case re:run(BinaryName, Re) of
    {match, _} -> BinaryName;
    _ -> invalid_name.
