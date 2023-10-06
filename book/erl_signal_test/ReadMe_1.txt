

# 操作系统处理的 sigusr1 sigquit sigterm 
OS Signal Event Handler
Asynchronous OS signals may be subscribed to via the Kernel applications event manager (see OTP Design Principles and gen_event(3)) registered as erl_signal_server. A default signal handler is installed which handles the following signals:

sigusr1
The default handler will halt Erlang and produce a crashdump with slogan "Received SIGUSR1". This is equivalent to calling erlang:halt("Received SIGUSR1").

sigquit
The default handler will halt Erlang immediately. This is equivalent to calling erlang:halt().

sigterm
The default handler will terminate Erlang normally. This is equivalent to calling init:stop().



# rabbitmq处理的  SIGHUP SIGTSTP 
%% #{signal => default | ignore | stop}.
-define(SIGNALS_HANDLED_BY_US,
        #{
          %% SIGHUP is often used to reload the configuration or reopen
          %% log files after they were rotated. We don't support any
          %% of those two cases, so ignore it for now, until we can do
          %% something about it.
          sighup => ignore,

          %% SIGTSTP is triggered by Ctrl+Z to pause a program. However
          %% we can't handle SIGCONT, the signal used to resume the
          %% program. Unfortunately, it makes a SIGTSTP handler less
          %% useful here.
          sigtstp => ignore
         }).
