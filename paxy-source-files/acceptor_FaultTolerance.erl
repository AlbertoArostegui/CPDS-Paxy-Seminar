-module(acceptor).
-export([start/2]).

start(Name, PanelId) ->
  spawn(fun() -> init(Name, PanelId) end).

% Added code - recover from fault
init(Name, na) ->
  io:format("[Acceptor ~w] RECOVERED ~n", [Name]),
  pers:open(Name), 
  case pers:read(Name) of 
    {Pr, Vt, Ac, Pn} ->
      Pn ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Vt]), 
              "Promised: " ++ io_lib:format("~p", [Pr]),  Ac}
  end,
  pers:close(Name),
  io:format("Recovered data Pr=~w Vt=~w Ac=~w Pn=~w ~n", [Pr, Vt, Ac, Pn]),
  acceptor(Name, Pr, Vt, Ac, Pn);
  
  
init(Name, PanelId) ->
  Promised = order:null(), 
  Voted = order:null(),
  Value = na,
  pers:open(Name),
  pers:store(Name, Promised, Voted, Value, PanelId),
  pers:close(Name),
  acceptor(Name, Promised, Voted, Value, PanelId).

acceptor(Name, Promised, Voted, Value, PanelId) ->
  receive
    {prepare, Proposer, Round} ->
      case order:gr(Round, Promised) of
        true ->
          % Added code - send promise
          pers:open(Name),
          pers:store(Name, Promised, Voted, Value, PanelId),
          pers:close(Name),

          Proposer ! {promise, Round, Voted, Value},               
          io:format("[Acceptor ~w] Phase 1: promised ~w voted ~w colour ~w~n",
                 [Name, Round, Voted, Value]),
          % Update gui
          Colour = case Value of na -> {0,0,0}; _ -> Value end,
          PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Voted]), "Promised: " ++ io_lib:format("~p", [Round]), Colour}, 
		  
          acceptor(Name, Round, Voted, Value, PanelId);
        false ->
          Proposer ! {sorry, {prepare, Round}},
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    {accept, Proposer, Round, Proposal} ->
      case order:goe(Round, Promised) of
        true ->
          % Added code - voting
          pers:open(Name),
          pers:store(Name, Promised, Round, Proposal, PanelId),
          pers:close(Name),

          Proposer ! {vote, Round},
          case order:goe(Round, Voted) of
            true ->
              io:format("[Acceptor ~w] Phase 2: promised ~w voted ~w colour ~w value ~w~n", [Name, Promised, Round, Proposal, Value]),

              % Update gui
              PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Round]), 
                         "Promised: " ++ io_lib:format("~p", [Promised]), Proposal},
						 
              acceptor(Name, Promised, Round, Proposal, PanelId);
            false ->
			  acceptor(Name, Promised, Voted, Value, PanelId)
          end;                            
        false ->
          Proposer ! {sorry, {accept, Round}},
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    stop ->
      % Added code - close and delete files
      pers:close(Name),
      pers:delete(Name),
      
      PanelId ! stop,
      ok
  end.
