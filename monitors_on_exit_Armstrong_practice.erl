c(misc_proc),
Rounds = 27.
% First attempt, and I realized that this is wrong from the start
% because multiple monitors are set up for the same process, yet
% it is interesting there are multiple 'DOWN' messages other than
% 'noproc' show up.
%
% I think the explanation is that on_exit/1 creates multiple 
% monitors for Pid and the rest is the same as the race
% condition below (see figure).

f(F), f(Pid),
F = fun() ->
        spawn(fun() ->
            receive
                Atom -> atom_to_list(Atom)
            end
        end)
    end,
Pid = F(),
[ begin
      Pid =  misc_proc:on_exit(Pid),
      Pid ! "abc"
  end
  || _ <- lists:seq(1,Rounds)],
flush().

% Reorganizing the message sending in another comprehension
% (or using spawn_monitor/1) all the monitors build up and
% tear down ok.
% Also, one message is enough.

f(F), f(Pid),
F = fun() ->
        spawn(fun() ->
            receive
                Atom -> atom_to_list(Atom)
            end
        end)
    end,
Pid = F(),
[ begin
      Pid =  misc_proc:on_exit(Pid)
  end
  || _ <- lists:seq(1,Rounds)],
Pid ! "abc",
flush().

% Race condition when spawning monitoring process
% and sending crash-message in the same list comprehension.
%
% I think that this is what is happening:
% the code was misleading for me at first because I saw
% it as simply a sequential code whereas spawning is
% concurrent - spawn/1 shoots of creating a process and
% it immediately executes the next line (while the process
% is forming, and it executes its own instructions).
%
%              +++++++++++
%  ===   :     ++THE-BAD++                                                                           
%  =0=   :     +++++++++++                                                                      
%  ===   :                                                                                       
%  SHE   t  ===                                                                                  
%  LLL   i  =1=                                                                                  
%   |    m  ===                                                                                  
%  SHE   e spawn      ===                                                                        
%  LLL   l  F/0       =2=                      ++++++++++++                                     
%   |    i  ---       ===                      ++THE-GOOD++                                     
%  SHE   n   |       spawn                     ++++++++++++                                     
%  LLL   e   |      on_exit/1                                                                    
%   |    :   |      ---------                                                                    
%  SHE   :   |         |                                                                         
%  LLL   :   X <----------------- 1 ! "abc"    ===                                                          
%   |    :             |                       =3=                                                      
%  SHE   :           mon(1)                    ===                                                      
%  LLL   :             ?                      spawn      ===                                            
%   |  <----------{...,noproc}                 F/0       =4=                                            
%  SHE   :                                     ---       ===                                            
%  LLL   :                                      |       spawn                                           
%   |    :                                      |      on_exit/1                                        
%  SHE   :                                      |      ---------                                        
%  LLL   :                                      |         |                                             
%   |    :         ++^++^^^++^+                 |         |                                             
%  SHE   :         <+THE-UGLY+>                 m--Ref--mon(3)                                    
%  LLL   :         ++V++VVV++V+                 |         |                                
%   |    :                                      |         |                                             
%  SHE   :                                      |         |                                      
%  LLL   :                                      X <----------------- 3 ! "abc"                                      
%   |    :                                      L________ |                                      
%  SHE   :                                               \|                                      
%  LLL <----------------------------------------------{'DOWN',...,Reason}                        
%   |    :                                                                                              
%   |    V                                                                                                
% 
% Yes, the examples took about 40 minutes tops, and this little
% drawing about 3 hours. And it may not even be accurate.
% (I know, we don't start sentences with "and".) 

f(F), f(Pid),                                                                                            
F = fun() ->                                                                                     
        spawn(fun() ->                                                                           
            receive
                Atom -> atom_to_list(Atom)
            end
        end)
    end,
[ begin
      Pid = misc_proc:on_exit(F()),
      Pid ! "abc"
  end
  || _ <- lists:seq(1,Rounds)].
flush().

% No race condition here - the monitor processes
% and the crash-messages are sent in different
% comprehensions.

f(PList),
PList = [ begin
      misc_proc:on_exit(F())
  end
  || _ <- lists:seq(1,Rounds)],
[Pid ! "abc" || Pid <- PList],
flush().

% Trying a new function to spawn the to-be-monitored
% process from the monitor process itself.
% Why is this seem to run ok and then crash unexpectedly
% on the next run?
%
% Because the way on_exit_F/1 is written. There are
% 2 different types of messages sent to the Erlang
% shell, yet at the end they are treated as equals.
% When the list comprehension gets to sending "abc"
% to Pid, previously messaged processes start crashing
% and the last receive picks up the {'DOWN',...} tuples
% as well.
%
% ** exception error: bad argument
%      in operator  !/2
%              called as {'DOWN',#Ref<0.0.2.362>,process,<0.132.0>,
%                         {badarg,[{erlang,atom_to_list,["abc"],[]}]}}
%                                !  "abc"

f(F),
F = fun() ->
        receive
            Atom -> atom_to_list(Atom)
        end
    end,
[ begin
      Pid = misc_proc:on_exit_F(F,faulty),
      Pid ! "abc"
  end
  || _ <- lists:seq(1,7)], % left the 7 to get lucky with a "clean" run
flush().

% Calling the "correcter" version
% The output is neat but of course it is good-for-nothing but
% demonstration.

f(F),
F = fun() ->
        receive
            Atom -> atom_to_list(Atom)
        end
    end,
[ begin
      Pid = misc_proc:on_exit_F(F,correcter),
      Pid ! "abc"
  end
  || _ <- lists:seq(1,Rounds)],
flush().

% There should be a better way to show how code evolves for
% learning purposes...

% And now, onto glorious copy-paste again. This one
% works as expected because of the atomic spawn_monitor/1

f(F),
F = fun() ->
        receive
            Atom -> atom_to_list(Atom)
        end
    end,
[ begin
      Pid = misc_proc:on_exit_F_with_spawnmon(F),
      Pid ! "abc"
  end
  || _ <- lists:seq(1,Rounds)],
flush().
