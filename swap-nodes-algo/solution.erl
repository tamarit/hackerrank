% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/swap-nodes-algo

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

-record(node, 
    {
        left = -1, 
        root = 1, 
        right = -1
    }).

calculate({Tree, Ks}) ->
    {Res, _} = lists:mapfoldl(fun swap/2, Tree, Ks),
    Res.

swap(K, Tree) ->
    Res  = swap(K, 1, K, Tree),
    {Res, Res}.

swap(_, _, _, Node = #node{root = -1}) ->
    Node;
swap(K, K, FixK, Node = #node{left = L, right = R}) ->
    NL = swap(K + FixK, K + 1, FixK, L),
    NR = swap(K + FixK, K + 1, FixK, R),
    Node#node{left = NR, right = NL};
swap(K, D, FixK, Node = #node{left = L, right = R}) ->
    NL = swap(K, D + 1, FixK, L),
    NR = swap(K, D + 1, FixK, R),
    Node#node{left = NL, right = NR}.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(Ds) -> 
    lists:map(
        fun(D) -> io:format("~s\n", [intlist2str(inorder(D))]) end, 
        Ds).

inorder(#node{root = -1}) ->
    [];
inorder(#node{left = L, root = M, right = R}) ->
    inorder(L) ++ [M | inorder(R)].

intlist2str(L) ->
    string:join(lists:map(fun integer_to_list/1, L), " ").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    Binary = read(),
    Res = binary:split(Binary, [<<"\n">>], [global]),
    [_| TreeAndK] = [binary_to_list(R) || R <- Res],
    {[Tree], [_|KsStr]}  = read_branches(TreeAndK, [#node{}]),
    {Tree, lists:map(fun str2int/1, KsStr)}.

read_branches([H | T], [Node = #node{root = -1} | Pending]) ->
    % ed({prev, [H | T], [Node | Pending]}),
    {RNodes, NotRead} = 
        read_branches([H | T], Pending),
    % ed({post, RNodes, NotRead}),
    {[Node | RNodes], NotRead};
read_branches([H | T], [Node = #node{} | Pending]) ->
    % ed({prev, [H | T], [Node | Pending]}),
    [L, R] = str2intlist(H),
    {RNodes, NotRead} = 
        read_branches(T, Pending ++ [#node{root = L}, #node{root = R}]),
    [RTree, LTree | Rest] = 
        lists:reverse(RNodes),
    % ed({post, [LTree, RTree | Rest], NotRead}),
    {[Node#node{left = LTree, right = RTree} | lists:reverse(Rest)], NotRead};
read_branches(NotRead, []) ->
    {[], NotRead}.

str2intlist(S) ->
    [str2int(T) || T <- string:tokens(S, " ")].

str2int(Str) ->
    element(1,string:to_integer(Str)).

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

ed(T) ->    
    erlang:display(T).
