-module(my_server).
-behaviour(gen_server).

%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% gen_server.

init([]) ->
	{ok, #state{}}.

handle_call(log_info, _From, State) ->
    syslog:start(),
    {ok,Log} = syslog:open("Beuha", [cons, perror, pid], local0),
    syslog:log(Log, err, "error happens"),
	syslog:log(Log, info, "process count: ~w", [length(processes())]),
	syslog:close(Log),
	{reply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
