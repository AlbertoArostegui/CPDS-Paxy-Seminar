-module(paxy).
-export([start/1, stop/0, stop/1]).

-define(RED, {255,0,0}).
-define(BLUE, {0,0,255}).
-define(GREEN, {0,255,0}).

% Sleep is a list with the initial sleep time for each proposer
start(Sleep) ->
    AcceptorNames = ["Homer", "Marge", "Bart", "Lisa", "Maggie"],
    AccRegister = [homer, marge, bart, lisa, maggie],
    ProposerNames = [{"Fry", ?RED}, {"Bender", ?GREEN}, {"Leela", ?BLUE}],
    PropInfo = [{fry, ?RED}, {bender, ?GREEN}, {leela, ?BLUE}],
    
    % Start GUI on the current node
    register(gui, spawn(fun() -> gui:start(AcceptorNames, ProposerNames) end)),
    gui ! {reqState, self()},
    
    AccNode = 'paxy-acc@127.0.0.1',
    ProNode = 'paxy-pro@127.0.0.1',
    
    receive
        {reqState, State} ->
            {AccIds, PropIds} = State,
            % Start acceptors on AccNode
            start_acceptors(AccIds, AccRegister, AccNode),
            spawn(fun() -> 
                Begin = erlang:monotonic_time(),
                % Start proposers on ProNode
                start_proposers(PropIds, PropInfo, AccRegister, Sleep, self(), ProNode, AccNode),
                wait_proposers(length(PropIds)),
                End = erlang:monotonic_time(),
                Elapsed = erlang:convert_time_unit(End-Begin, native, millisecond),
                io:format("[Paxy] Total elapsed time: ~w ms~n", [Elapsed])
            end)
    end.

start_acceptors(AccIds, AccReg, Node) ->
    case AccIds of
        [] ->
            ok;
        [AccId|Rest] ->
            [RegName|RegNameRest] = AccReg,
            % Spawn acceptor on remote node
            Pid = spawn(Node, acceptor, start, [RegName, AccId]),
            rpc:call(Node, erlang, register, [RegName, Pid]),
            start_acceptors(Rest, RegNameRest, Node)
    end.

start_proposers(PropIds, PropInfo, Acceptors, Sleep, Main, ProNode, AccNode) ->
    % Convert local acceptor names to their full remote names
    RemoteAcceptors = lists:map(fun(Acc) -> {Acc, AccNode} end, Acceptors),
    case PropIds of
        [] ->
            ok;
        [PropId|Rest] ->
            [{RegName, Colour}|RestInfo] = PropInfo,
            [FirstSleep|RestSleep] = Sleep,
            % Spawn proposer on remote node
            spawn(ProNode, proposer, start, [RegName, Colour, RemoteAcceptors, FirstSleep, PropId, Main]),
            start_proposers(Rest, RestInfo, Acceptors, RestSleep, Main, ProNode, AccNode)
    end.

wait_proposers(0) ->
    ok;
wait_proposers(N) ->
    receive
        done ->
            wait_proposers(N-1)
    end.

stop() ->
    AccNode = 'paxy-acc@127.0.0.1',
    lists:foreach(fun(Name) ->
        stop({Name, AccNode})
    end, [homer, marge, bart, lisa, maggie]),
    stop(gui).

stop(Name) when is_atom(Name) ->
    case whereis(Name) of
        undefined ->
            ok;
        Pid ->
            Pid ! stop
    end;
stop({Name, Node}) ->
    case rpc:call(Node, erlang, whereis, [Name]) of
        undefined ->
            ok;
        Pid ->
            Pid ! stop
    end.

 
