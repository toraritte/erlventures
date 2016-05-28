-module(monitors_on_exit_Armstrong).
-export([on_exit/1,on_exit_F/2,on_exit_F_with_spawnmon/1]).

% set up monitoring on an already running process
on_exit(Pid) ->
    Shell = self(),
    spawn(fun() ->
                  Ref = monitor(process,Pid),
                  receive
                      {'DOWN', Ref, process, Pid, _Reason} = M ->
                          io:format("~p~n",[M]),
                          Shell ! M
                  end
          end),
    Pid.

% get a function, spawn it and start monitoring it
on_exit_F(Fun,Implementation) ->
    Shell = self(),
    spawn(fun() ->
                  Pid = spawn(Fun),
                  Shell ! Pid,
                  Ref = monitor(process,Pid),
                  receive
                      {'DOWN', Ref, process, Pid, _Reason} = M ->
                          io:format("~p~n",[M]),
                          Shell ! M
                  end
          end),
    case Implementation of
        faulty -> 
            receive
                Pid -> Pid
            end;
        correcter->
            % i suspect that there is a dangerous assumption on my part,
            % that a very first message is going to be a pid
            % (if not, will it crash with Unbound Var error?)
            % FOLLOW UP:
            % Nope, it wouldn't even compile as it is below:)
            %%% receive
            %%%     Pid when is_pid(Pid) -> Pid;
            %%%     _ -> Pid
            %%% end
            receive
                Pid when is_pid(Pid) -> Pid
            end
            % Although not efficient as messages will be left in
            % the shell but that's not the point right now. I'll
            % flush it after.
    end.


on_exit_F_with_spawnmon(Fun) ->
    Shell = self(),
    spawn(fun() ->
                  {Pid,Ref} = spawn_monitor(Fun),
                  Shell ! Pid,
                  receive
                      {'DOWN', Ref, process, Pid, _Reason} = M ->
                          io:format("~p~n",[M]),
                          Shell ! M
                  end
          end),
    % same thing applies as wit on_exit_F/2 so no need for
    % the sallang(HUN).
    receive
        Pid when is_pid(Pid) -> Pid
    end.
