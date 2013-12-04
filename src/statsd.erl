-module(statsd).
-author("Dominik Liebler").
-behaviour(gen_server).

%% forced by gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start/2, start/1, start/0, stop/0]).
-export([increment/1, increment/2, decrement/1, decrement/2, count/2, count/3, gauge/2, timing/2, timing/3]).

-define(STATSD_DEFAULT_PORT, 8125).
-define(STATSD_DEFAULT_HOST, "localhost").

%% holds all the relevant state that is used internally to pass the socket around
-record(state, {
	port = ?STATSD_DEFAULT_PORT,
	host = ?STATSD_DEFAULT_HOST,
	socket
}).

%% Public: opens the socket
%%
%% returns a #state record containing the socket
start(Host, Port) ->
	{ok, Socket} = gen_udp:open(0),
	State = #state{port = Port, host = Host, socket = Socket},
	gen_server:start_link({local, ?MODULE}, ?MODULE, [State], []).
start(Host) ->
	start(Host, ?STATSD_DEFAULT_PORT).
start() ->
	start(?STATSD_DEFAULT_HOST).

%% Public: stop server
%%
%% returns: {ok}
stop() ->
	gen_server:call(?MODULE, stop),
	{ok}.

%% Internal: used by gen_server and called on connection init
%%
%% returns: {ok, State}
init([State]) ->
	{ok}.

%% Internal: used by gen_server and called on connection termination
%%
%% returns: {ok}
terminate(_Reason, State) ->
	{ok, State}.
	
%% Public: increments a counter by 1
%% 
%% returns ok or {error, Reason}
increment(Key, Samplerate) ->
	count(Key, 1, Samplerate).
increment(Key) ->
	count(Key, 1).
	
%% Public: decrements a counter by 1
%% 
%% returns ok or {error, Reason}
decrement(Key, Samplerate) ->
	count(Key, -1, Samplerate).
decrement(Key) ->
	count(Key, -1).

%% Public: increments a counter by an arbitrary integer value
%%
%% returns: ok or {error, Reason}
count(Key, Value) ->
	send({message, Key, Value, c}).
count(Key, Value, Samplerate) ->
	send({message, Key, Value, c, Samplerate}, Samplerate).

%% Public: sends an arbitrary gauge value
%%
%% returns: ok or {error, Reason}
gauge(Key, Value) ->
	send({message, Key, Value, g}).

%% Public: sends a timing in ms
%%
%% returns: ok or {error, Reason}
timing(Key, Value) ->
	send({message, Key, Value, ms}).
timing(Key, Value, Samplerate) ->
	send({message, Key, Value, ms, Samplerate}, Samplerate).
	
%% Internal: prepares and sends the messages
%%
%% returns: ok or {error, Reason}
send(Message, Samplerate) when Samplerate =:= 1 ->
    send(Message);

send(Message, Samplerate) ->
    case random:uniform() =< Samplerate of
        true -> send(Message);
        _ -> ok
    end.

send(Message) ->
    gen_server:cast(?MODULE, {send_message, build_message(Message)}).

%% Internal: builds the message string to be sent
%% 
%% returns: a String
build_message({message, Key, Value, Type}) ->
	lists:concat([Key, ":", io_lib:format("~w", [Value]), "|", Type]);
build_message({message, Key, Value, Type, Samplerate}) ->
	lists:concat([build_message({message, Key, Value, Type}) | ["@", io_lib:format("~.2f", [1.0 / Samplerate])]]).

%% Internal: handles gen_server:call calls
%%
%% returns:	{reply, ok|error, State}
handle_cast({send_message, Message}, State) ->
	gen_udp:send(State#state.socket, State#state.host, State#state.port, Message),
	{ok, State}.
handle_call(stop, _From, State) ->
	gen_udp:close(State#state.socket),
	{stop, normal, stopped, State}.
	
code_change(_OldVersion, State, _Extra) -> {ok, State}.
handle_info(_Info, State) -> {ok, State}.
