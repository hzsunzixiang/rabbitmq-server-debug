%% Feel free to use, reuse and abuse the code in this file.

-module(echo_protocol).
-behaviour(ranch_protocol).

-export([start_link/3]).
-export([init/3]).

start_link(Ref, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [Ref, Transport, Opts]),
	{ok, Pid}.

init(Ref, Transport, _Opts = []) ->
	{ok, Socket} = ranch:handshake(Ref),
	loop(Socket, Transport).

loop(Socket, Transport) ->
	case Transport:recv(Socket, 0, 60000) of
		{ok, Data} when Data =/= <<4>> ->
			Transport:send(Socket, Data),
			loop(Socket, Transport);
		_ ->
			ok = Transport:close(Socket)
	end.

%handshake(State=#state{ref=Ref, transport=Transport, handshake_timeout=HandshakeTimeout,
%		max_conns=MaxConns, alarms=Alarms0}, CurConns, NbChildren, Sleepers, To, Socket, SupPid, ProtocolPid) ->
%	case Transport:controlling_process(Socket, ProtocolPid) of
%		ok ->
%			ProtocolPid ! {handshake, Ref, Transport, Socket, HandshakeTimeout},
%			put(SupPid, active),
%			CurConns2 = CurConns + 1,
%			Sleepers2 = if CurConns2 < MaxConns ->
%					To ! self(),
%					Sleepers;
%				true ->
%					[To|Sleepers]
%			end,
%			Alarms1 = trigger_alarms(Ref, Alarms0, CurConns2),
%			loop(State#state{alarms=Alarms1}, CurConns2, NbChildren + 1, Sleepers2);
%		{error, _} ->
%			Transport:close(Socket),
%			%% Only kill the supervised pid, because the connection's pid,
%			%% when different, is supposed to be sitting under it and linked.
%			exit(SupPid, kill),
%			To ! self(),
%			loop(State, CurConns, NbChildren, Sleepers)
%	end.
%
%
%handshake1(Ref, Opts) ->
%	receive {handshake, Ref, Transport, CSocket, Timeout} ->
%		Handshake = handshake_transport(Transport, handshake, CSocket, Opts, Timeout),
%		handshake_result(Handshake, Ref, Transport, CSocket, Timeout)
%	end.
%
%handshake_transport(Transport, Fun, CSocket, undefined, Timeout) ->
%	Transport:Fun(CSocket, Timeout);
%handshake_transport(Transport, Fun, CSocket, {opts, Opts}, Timeout) ->
%	Transport:Fun(CSocket, Opts, Timeout).
