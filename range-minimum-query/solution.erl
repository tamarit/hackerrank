% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/dice-path

-module(solution).
-export([main/0, prof/0]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main() ->
    Data = read_data(),
    Res = calculate(Data),
    output_data(Res),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Profiling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prof() ->
    eprof:start(),
    eprof:start_profiling([self()]),
    main2(),
    eprof:stop_profiling(),
    eprof:analyze(total). 

main2() ->
    StartInput = os:timestamp(),
    Data = read_data(),
    io:format("INPUT: total time taken ~p seconds~n", [timer:now_diff(os:timestamp(), StartInput)/1000000]),
    StartRes = os:timestamp(),
    Res = calculate(Data),
    io:format("CALCULATE: total time taken ~p seconds~n", [timer:now_diff(os:timestamp(), StartRes)/1000000]),
    StartOutput = os:timestamp(),
    output_data(Res),
    io:format("OUTPUT: total time taken ~p seconds~n", [timer:now_diff(os:timestamp(), StartOutput)/1000000]),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Calculate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% *********************************
% BEGIN https://github.com/lagodiuk/segment-tree-erl
% *********************************


-define(EMPTY_CHILD, null).
-define(START_INDEX, 1).

-record(interval, {left, right}).
-define(EMPTY_INTERVAL, null).

-record(node, {left, right, val, interval}).
-record(segment_tree, {root, func}).

new(List, F) ->
    Leafs = make_leafs(List, F),
    Tree = make_tree(Leafs, [], F),
        #segment_tree{root=Tree, func=F}.

make_leafs(List, F) ->
    make_leafs(List, F, [], ?START_INDEX).
make_leafs([], _F, Acc, _Indx) ->
    lists:reverse(Acc);
make_leafs([H | T], F, Acc, Indx) ->
    Interval = interval_new(Indx),
    Value = F(H, ?EMPTY_CHILD),
    Leaf = node(H, Value, Interval),
    make_leafs(T, F, [Leaf | Acc], Indx+1).

make_tree([], [Tree], _F) ->
        Tree;
make_tree([], Acc, F) ->
        make_tree(lists:reverse(Acc), [], F);
make_tree([A], Acc, F) ->
        make_tree([], [A | Acc], F);
make_tree([A = #node{val=ValA, interval=LInterval, _=_}, B = #node{val=ValB, interval=RInterval, _=_} | T], Acc, F) ->
    Interval = join(LInterval, RInterval),
    Value = F(ValA, ValB),
    Node = node(A, B, Value, Interval),
        make_tree(T, [Node | Acc], F).

fetch(#interval{left=L, right=R}, SegTree) when R < L ->
    ReverseInterval = interval_new(R,L),
    fetch(ReverseInterval, SegTree);
fetch(Interval, #segment_tree{root=Root, func=F}) ->
    fetch(Interval, Root, F).
fetch(#interval{left=L, right=R}, #node{interval=#interval{left=L, right=R}, val=Val, _=_}, _F) ->
    Val;
fetch(Interval, #node{left=Left, right=Right}, F) ->
    LInterval = get_interval(Left),
    RInterval = get_interval(Right),
    LI = intersect(LInterval, Interval),
    RI = intersect(Interval, RInterval),
    case {LI, RI} of
        {LI, ?EMPTY_INTERVAL} ->
            fetch(LI, Left, F);
        {?EMPTY_INTERVAL, RI} ->
            fetch(RI, Right, F);
        {LI, RI} ->
            LF = fetch(LI, Left, F),
            RF = fetch(RI, Right, F),
            F(LF, RF)
    end.

node(Left, Right, Val, Interval) ->
        #node{left=Left, right=Right, val=Val, interval=Interval}.
node(Leaf, Val, Interval) ->
        #node{left=Leaf, right=?EMPTY_CHILD, val=Val, interval=Interval}.

get_interval(#node{interval=Interval, _=_}) ->
    Interval.

%% create interval which represents single point
interval_new(X) ->
    interval_new(X, X).

%% create interval which has left point smaller than right
interval_new(L, R) when L =< R ->
    #interval{left=L, right=R};
interval_new(R, L) ->
    #interval{left=L, right=R}.

%% join "neighbour" intervals:
%% [x..(n-1)] + [n..y] -> [x..y]
join(#interval{left=L1, right=R1}, #interval{left=L2, right=R2}) when (L2-R1) == 1 ->
    interval_new(L1, R2).

%% if intervals intersect - returns intersection interval
%% otherwise - returns EMPTY_INTERVAL (which is defined in "interval.hrl")
intersect(Interval, Interval) ->
    Interval;
intersect(#interval{left=L1, right=R1}, #interval{left=L2, right=R2}) ->
    A0 = [L1, R1, L2, R2],
    A1 = lists:sort(A0),
    [_, X1, X2, _] = A1,
    case A0 == A1 of
        false ->
            interval_new(X1, X2);
        true when X1 == X2 ->
            interval_new(X1, X2);
        true ->
            ?EMPTY_INTERVAL
    end.

% *********************************
% END https://github.com/lagodiuk/segment-tree-erl
% *********************************

 calculate({A, Ts}) ->
    Min = fun
        (X, ?EMPTY_CHILD) -> X; 
        (X, Y) -> min(X, Y)
    end,
    ST = new(A, Min),
    lists:map(
        fun([L, R]) -> 
            fetch(interval_new(L + 1, R + 1), ST) 
        end, 
        Ts).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(Ds) -> 
    lists:foreach(fun(D) -> io:format("~p\n", [D]) end, Ds).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    Binary = read(),
    Res = binary:split(Binary, [<<"\n">>], [global]),
    [_, ArrStr | Ts] = [binary_to_list(R) || R <- Res],
    {
        str2intlist(ArrStr), 
        lists:filter(
            fun([]) -> false; (_) -> true end,
            lists:map(fun str2intlist/1, Ts)
        )
    }.

str2int(Str) ->
    element(1,string:to_integer(Str)).

str2intlist(S) ->
    [str2int(T) || T <- string:tokens(S, " ")].

-define(BLK_SIZE, 16384).

read() ->
    ok = io:setopts(standard_io, [binary]),
    read(<<>>).

read(Acc) ->
    case file:read(standard_io, ?BLK_SIZE) of
        {ok, Data} ->
            read(<<Acc/bytes, Data/bytes>>);
        eof ->
            Acc
    end.



