-module(linkmon_Hebert).
-compile(export_all).

chain(N) ->
    chain(N,self()).

chain_monitor(N) ->
    %
    % -> linkmon-chain_monitor.png
    %
    spawn_monitor(linkmon_Hebert,chain,[N,self()]).

system_chain(N) ->
    process_flag(trap_exit,true),
    chain(N).

chain(0, Shell) ->
    receive
        M -> Shell ! M
    after
        2000 ->
            % Why does this message not show
            % up in the shell?
            % Because the way chain/2 is written:
            % the shell is linked to the process
            % so it dies with them but it is restarted
            % by its supervisor - with a different pid.
            %
            % -> linkmon-chain.png
            % %
            Shell ! {viszlat,self()},
            exit(ennyi_volt)
    end;
chain(N, Shell) ->
    io:format("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa[~p~n",[Shell]),
  % Pid = spawn(fun() -> chain(N-1) end),
    Pid = spawn(?MODULE,chain,[N-1,Shell]),
    link(Pid),
    receive
        M -> Shell ! M
    end.

% c(linkmon_Hebert),
% linkmon_Hebert:chain(7).
% flush().

% c(linkmon_Hebert),
% io:format("Shell's self() -> ~p~n",[self()]),
% linkmon_Hebert:chain_monitor(7).
% io:format("Shell's self() -> ~p~n",[self()]),
% flush().

% c(linkmon_Hebert),
% io:format("Shell's self() -> ~p~n",[self()]),
% linkmon_Hebert:system_chain(7),
% io:format("Shell's self() -> ~p~n",[self()]),
% flush().

% ======================================================
% === self() contexts ==================================
% ======================================================
%
% -> linkmon-self-contexts.png
%
self_from_spawned_anon_fun(Shell) ->
    spawn(fun() -> mod_fun(Shell,self()) end).

self_from_spawned_mod_fun(Shell) ->
    spawn(?MODULE,mod_fun,[Shell,self()]).

mod_fun(Shell,Pid) ->
    Shell ! Pid.

% c(linkmon_Hebert),
% io:format("Shell's self() -> ~p~n",[self()]),
% linkmon_Hebert:self_from_spawned_anon_fun(self()),
% linkmon_Hebert:self_from_spawned_mod_fun(self()),
% flush().
