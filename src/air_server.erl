-module(air_server).
-behaviour(gen_server).

%% API
-export([
         start_link/1,
         get_count/0,
         stop/0
         ]).

%% gen_server
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
         ]).

-define(SERVER, ?MODULE).

-record(state, {port = 6667, lsock, request_count = 0}).


%% API
start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

get_count() ->
    gen_server:call(?SERVER, get_count).

stop() ->
    gen_server:cast(?SERVER, stop).

%% gen_server
init([Port]) ->
    {ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
    {ok, #state{port=Port, lsock=LSock}, 0}.


handle_call(get_count, _From, State) ->
    {reply, {ok, State#state.request_count}, State}.

handle_cast(stop, State) ->
    {stop, ok, State}.

handle_info({tcp, Socket, RawData}, State) ->
    RequestCount = State#state.request_count,
    try
        Data = string:strip(
                 string:strip(RawData, right, $\n),
                 right, $\r),
        [M, F|Args] = string:tokens(Data, "|"),
        Module = list_to_atom(M),
        Function = list_to_atom(F),
        Reply =
        case Args of
            [] -> Module:Function();
            Args -> Module:Function(Args)
        end,
        case Reply of
            ok ->
                gen_tcp:send(Socket, "ok");
            {ok, Atom} when is_atom(Atom) ->
                gen_tcp:send(Socket, atom_to_list(Atom));
            {ok, Integer} when is_integer(Integer) ->
                gen_tcp:send(Socket, integer_to_list(Integer));
            {ok, [H|_] = String} when is_integer(H), H >= $\s, H<255 ->
                gen_tcp:send(Socket, String)
        end
    catch
        _C:_E ->
            gen_tcp:send(Socket, "error - call failed\n")
    end,
    {noreply, State#state{request_count = RequestCount + 1}};
handle_info(timeout, State) ->
    {ok, _Sock} = gen_tcp:accept(State#state.lsock),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
