-module(acceptor).
-export([start/2]).
-define(delay, 500).

start(Name, PanelId) ->
  spawn(fun() -> init(Name, PanelId) end).

  
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
          % Added code
          P = rand:uniform(10),
          if P =< ?drop ->
                io:format("message dropped~n");
            true ->
                %send message
                Proposer ! {promise, Round, Voted, Value}
          end,
           % Proposer ! {promise, Round, Voted, Value},               
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
          % Added code
          P = rand:uniform(10),
          if P =< ?drop ->
                io:format("message dropped~n");
            true ->
                %send message
                Proposer ! {vote, Round}
          end,
          % Proposer ! {vote, Round},
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
      PanelId ! stop,
      ok
  end.
