% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/messy-medians

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

-record(double_heap, 
    {left = nil,
     right = nil}).

dh_insert(X, I, nil) ->
    LHeap = gb_trees:enter({X, I}, [], gb_trees:from_orddict([])),
    #double_heap{left = LHeap, right = gb_trees:from_orddict([])};
dh_insert(X, I, DH = #double_heap{left = L, right = R}) ->
    {{LL,_}, _} = gb_trees:largest(L),
    % ed({to_list(L), to_list(R)}),
    NDH = #double_heap{left = NL, right = NR} =
        case X < LL of
            true -> 
                DH#double_heap{left = gb_trees:enter({X, I}, [], L)};
            false ->
                DH#double_heap{right = gb_trees:enter({X, I}, [], R)}
        end,
    Diff = gb_trees:size(NL) - gb_trees:size(NR),
    if 
        Diff > 1 -> 
            {{Max,IMax}, _, NNL} = gb_trees:take_largest(NL),
            NDH#double_heap{
                left = NNL, 
                right = gb_trees:enter({Max,IMax}, [], NR)
            };
        Diff =< -1 -> 
            {{Min,IMin}, _, NNR} = gb_trees:take_smallest(NR),
            NDH#double_heap{
                right = NNR, 
                left = gb_trees:enter({Min,IMin}, [], NL)
            };
        true ->
            NDH
    end.

median(#double_heap{left = L}) ->
    {{Max,_}, _} = gb_trees:largest(L),
    Max.

calculate(Tests) ->
    Marked = 
        mark_to_store(lists:reverse(Tests), gb_trees:from_orddict([]), 1, []),
    % io:format("~p\n", [Marked]),
    run(Marked, nil, 1, gb_trees:from_orddict([])).
   

mark_to_store([H | T], Dict, I, Acc) when H >= 0 ->
    {NDict, NAcc} = 
        case gb_trees:is_empty(Dict) of 
            false ->
                case gb_trees:smallest(Dict) of 
                    {I, Idx} ->   
                        {_, _, NDict0} = gb_trees:take_smallest(Dict),
                        {NDict0, [{save, H, Idx} | Acc]};
                    _ ->
                        {Dict, [H | Acc]}
                end;
            true ->
                {Dict, [H | Acc]}
        end,
    mark_to_store(
        T, 
        NDict, 
        I + 1, 
        NAcc);   
mark_to_store([H | T], Dict, I, Acc) when H < 0 ->
    CurrentHI = 
        case gb_trees:is_defined(abs(H) + I, Dict) of
            true -> 
                gb_trees:get(abs(H) + I, Dict);
            false ->
                []
        end,
    NDict = 
        case gb_trees:is_empty(Dict) of 
            false ->
                case gb_trees:smallest(Dict) of 
                    {I, Idx} ->   
                        {_, _, NDict0} = gb_trees:take_smallest(Dict),
                        gb_trees:enter(abs(H) + I, [-I | CurrentHI ++ Idx], NDict0);
                    _ ->
                        gb_trees:enter(abs(H) + I, [-I | CurrentHI], Dict)
                end;
            true ->
                gb_trees:enter(abs(H) + I, [-I | CurrentHI], Dict)
        end,
    mark_to_store(
        T, 
        NDict, 
        I + 1, 
        [{load, -I} | Acc]);
mark_to_store([], {0,nil}, _, Acc) ->
    Acc.

run([{save, N, Idx} | T], CDH, I, Dict) ->
    NCDH = dh_insert(N, I, CDH),
    NDict = 
        lists:foldl(
            fun(Ix, Acc) -> 
                gb_trees:enter(Ix, NCDH, Acc) 
            end, 
            Dict, 
            Idx),
    print_median(NCDH),
    run(T, NCDH, I + 1, NDict);
run([{load, _} | T], _, I, Dict) ->
    {_, NCDH, NDict} = gb_trees:take_smallest(Dict),
    print_median(NCDH),
    run(T, NCDH, I + 1, NDict);
run([N | T], CDH, I, Dict) ->
    NCDH = dh_insert(N, I, CDH),
    print_median(NCDH),
    run(T, NCDH, I + 1, Dict);
run([], _, _, _) ->
    ok.


print_median(DH) ->
    % ed({to_list(DH#double_heap.left), to_list(DH#double_heap.right)}),
    io:format("~p\n", [median(DH)]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(_) -> 
    io:format("").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    Binary = read(),
    Res = binary:split(Binary, [<<"\n">>], [global]),
    [_| Tests] = [binary_to_list(R) || R <- Res],
    lists:map(fun str2int/1, Tests).


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
    io:format("~p\n", [T]),
    ok.
