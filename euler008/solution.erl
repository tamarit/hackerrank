% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/contests/projecteuler/challenges/euler008

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
    
calculate(Ts) ->
    lists:map(fun calculate_test/1, Ts).

calculate_test({1, Ds}) ->
    lists:max(Ds);
calculate_test({K, Ds}) ->
    case group_seq(K, Ds) of 
        [] ->
            0; 
        GSeq ->
            lists:max(
                lists:map(
                    fun(NDs) ->
                        IniSeq = string:substr(NDs, 1, K),
                        Max = prod(IniSeq),
                        % io:format("~p\n", [{NDs, IniSeq, Max}]),
                        trunc(
                            find_max_seq(
                                string:substr(NDs, K + 1), 
                                K, 
                                {Max / hd(NDs), 
                                tl(IniSeq)}, 
                                Max))
                    end,
                    GSeq
                )
            )
    end.

group_seq(K, Ds) ->
    [Seq || Seq <- group_seq(Ds, [], []), length(Seq) >= K]. 

group_seq([], CSeq, Acc) ->
    lists:reverse([lists:reverse(CSeq) | Acc]);
group_seq([0|T], CSeq, Acc) ->
    group_seq(T, [], [lists:reverse(CSeq)|Acc]);
group_seq([H|T], CSeq, Acc) ->
    group_seq(T, [H|CSeq], Acc).

find_max_seq([H|T], K, {Prod, Prev}, Max) ->
    % io:format("~p\n", [{[H|T], {Prod, Prev}, Max}]),
    ProdSeq = H * Prod,
    NMax = 
        case ProdSeq > Max of 
            true ->
                ProdSeq;
            false ->
                Max 
        end,
    find_max_seq(T, K, {ProdSeq / hd(Prev), tl(Prev) ++ [H]}, NMax);
find_max_seq([], _, _, Max) ->
    Max.

prod(L) ->
    lists:foldl(fun(X, Prod) -> X * Prod end, 1, L).
 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(Ds) -> 
    lists:map(
        fun(D) -> io:format("~p\n", [D]) end, 
        Ds).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    Binary = read(),
    Res = binary:split(Binary, [<<"\n">>], [global]),
    [_| Tests] = [binary_to_list(R) || R <- Res],
    read_tests(Tests, []).

read_tests([NKStr, DigStr | Tests], Acc) ->
    [_, KStr] = string:tokens(NKStr, " "),
    read_tests(Tests, [{str2int(KStr), [str2int([D]) || D <- DigStr]} | Acc]);
read_tests([], Acc) ->
    lists:reverse(Acc).

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

