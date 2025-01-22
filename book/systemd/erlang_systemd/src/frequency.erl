-module(frequency).
-behaviour(gen_server).

%-export([start_link/0, stop/0, allocate/0, deallocate/1]).
%-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2]).
%-export([format_status/2]).
-compile(export_all).
-compile(nowarn_export_all).

%% CLIENT FUNCTIONS

%% start() -> {ok, pid()} | {error, Reason}
%% Starts the frequency server. Called by supervisor

start_link() ->
    io:format('Starting up ...\n'),
    io:format('Startup complete before notify\n'),
    {ok, _} = application:ensure_all_started(systemd),
    timer:sleep(5000),
    systemd:notify([ready, {status, "booting"}]),
    io:format('after ready: Startup complete after notify\n'),
    timer:sleep(5000),
    systemd:notify([{status, "Processing  data"}]),
    io:format('after:Processing  data: Startup complete after notify\n'),
    timer:sleep(5000),
    systemd:notify([{status, "After Processing  data"}]),
    io:format('after:After Processing  data : Startup complete after notify\n'),
    timer:sleep(5000),
    systemd:notify([ready, {status, "booted"}]),
    io:format('after:ready booted \n'),
    gen_server:start_link({local, frequency}, frequency, [], []).

%% stop() -> ok.
%% Stops the frequency server.

stop() ->
    gen_server:cast(frequency, stop).


%% allocate() -> {ok, Frequency} | {error, no_resource}
%% If available, it returns a frequency used to make a call.
%% Frequency must be deallocated on termination.

allocate() ->
    gen_server:call(frequency, {allocate, self()}).

%% deallocate() -> ok
%% Frees a frequency so it can be used by another client.

deallocate(Frequency) ->
    gen_server:cast(frequency, {deallocate, Frequency }).


%% CALLBACK FUNCTIONS

%% init(_) -> {ok, State}
%% Initialises the generic server by getting the list of
%% available frequencies. [] is the list of allocated ones.

init(_Args) ->
    Frequencies = {get_frequencies(), []},
    {ok, Frequencies}.

%% Dummy function. To be replaced with call to BSC.

get_frequencies() -> [10,11,12,13,14, 15].


%% handle_call({allocate, Pid}, _, {Available, Allocated}) ->
%%     {reply, {ok, Frequency} | {error, no_resource}, {Available, Allocated}}
%% Callback for allocating resources. 

handle_call({allocate, Pid}, _From, Frequencies) ->
    {NewFrequencies, Reply} = allocate(Frequencies, Pid),
    {reply, Reply, NewFrequencies}.


%% handle_cast({deallocate, Freq}, Frequencies) -> {noreply, NewFrequencies}; 
%% Callback for deallocating resources

handle_cast({deallocate, Freq}, Frequencies) ->
    NewFrequencies = deallocate(Frequencies, Freq),
    {noreply, NewFrequencies}; 

%% handle_cast(stop, LoopData) -> {stop, normal, LoopData}.
%% callback to stop the gen_server.

handle_cast(stop, LoopData) ->
    {stop, normal, LoopData}.

handle_info(_Msg, LoopData) ->
    {noreply, LoopData}.

%% terminate(Reason, LoopData) -> ok.
%% Termination callback. Does nothing, but should instead kill clients.

terminate(_Reason, _LoopData) ->
    ok.

format_status(_Opt, [_ProcDict, {Available, Allocated}]) ->
    {data, [{"State", {{available, Available}, {allocated, Allocated}}}]}.

%% INTERNAL FUNCTIONS

%% Helper functions used to allocate and deallocate resources.

allocate({[], Allocated}, _Pid) ->
    freq_overload:frequency_denied(),
    {{[], Allocated}, {error, no_frequency}};
allocate({[Res|Resources], Allocated}, Pid) ->
    case Resources of
	[] -> freq_overload:no_frequency();
	_  -> ok
    end,
    {{Resources, [{Res, Pid}|Allocated]}, {ok, Res}}.

deallocate({Free, Allocated}, Res) ->
    case Free of
	[] -> freq_overload:frequency_available();
	_  -> ok
    end,
    NewAllocated = lists:keydelete(Res, 1, Allocated),
    {[Res|Free],  NewAllocated}.


start() ->
    %{ok, PF} = frequency:start_link(),
    {ok, PO} = freq_overload:start_link(),
    frequency:allocate(),
    frequency:allocate(),
    frequency:allocate(),
    frequency:allocate(),
    frequency:allocate(),
    frequency:allocate(),
    frequency:allocate(),
    frequency:allocate(),
    frequency:allocate(),
    frequency:allocate(),
    frequency:deallocate(10),
    Res2=counters_my:get_counters_my(PO),
    io:format("result:~p~n", [Res2]),
    'this is an end'.
