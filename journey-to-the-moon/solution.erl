% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/journey-to-the-moon

-module(solution).
-export([main/0, prof/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main() ->
    Data = read_data(),
    % StartRes = os:timestamp(),
    Res = calculate(Data),
    % io:format("CALCULATE: total time taken ~p seconds~n", [timer:now_diff(os:timestamp(), StartRes)/1000000]),
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

calculate({N, Pairs}) ->
    Vertices = 
        lists:seq(0, N-1),
    G = 
        digraph:new(),
    [digraph:add_vertex(G, V) || V <- Vertices],
    [digraph:add_edge(G, V1, V2)|| {V1, V2} <- Pairs],  
    CC = digraph_utils:components(G),
    CCL = [length(C) || C <- CC],
    count_comb(CCL, N, 0).

count_comb([_], _, Acc) ->
    Acc;
count_comb([H|T], Total, Acc) ->
    NTotal = Total - H,
    count_comb(T, NTotal, (H * NTotal) + Acc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(D) -> 
    io:format("~p\n", [D]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    Binary = read(),
    Res = binary:split(Binary, [<<"\n">>], [global]),
    [N_I| PairsStr] = [binary_to_list(R) || R <- Res],
    [N|_] = str2listint(N_I),
    Pairs = [list_to_tuple(str2listint(Pair))  || Pair <- PairsStr],
    {N, Pairs}.

str2int(Str) ->
    element(1,string:to_integer(Str)).

str2listint(Str) ->
    LStr = 
        string:tokens(Str, " "),
    [str2int(S)  || S <- LStr].

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

% ed(T) ->    
%     erlang:display(T).
