% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/contests/projecteuler/challenges/euler011

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
    Rows = Ts,
    Cols = columns(Ts, []),
    LeftDiags = left_diagonals(Ts),
    RightDiags = right_diagonals(Ts),
    Seqs = lists:concat([Rows, Cols, LeftDiags, RightDiags]),
    % io:format("~p\n", [Seqs]),
    lists:max(lists:map(fun calculate_test/1, Seqs)).

columns(Ls, Acc) ->
    Col = one_column(Ls, []),
    NLs = [tl(L) || L <- Ls],
    case hd(NLs) of 
        [] ->
            [Col | Acc];
        _ ->
            columns(NLs, [Col | Acc])
    end.

one_column([[H|_] | TL], Acc) ->
    one_column(TL, [H | Acc]);
one_column([], Acc) ->
    Acc.

left_diagonals(Ls) ->
    left_diagonals(Ls, 20, 1, []).    

left_diagonals(Ls, I, J, Acc) ->
    Seq = one_diagonal(Ls, I, J, fun(X, Y) -> X + Y end, []),
    NAcc = [Seq | Acc],
    case {I,J} of 
        {1, 20} ->
            NAcc;
        {1, _} ->
            left_diagonals(Ls, 1, J + 1, NAcc);
        {_, 1} ->
            left_diagonals(Ls, I - 1, 1, NAcc)
    end.

right_diagonals(Ls) ->
    right_diagonals(Ls, 20, 20, []).  

right_diagonals(Ls, I, J, Acc) ->
    Seq = one_diagonal(Ls, I, J, fun(X, Y) -> X - Y end, []),
    NAcc = [Seq | Acc],
    case {I,J} of 
        {1, 1} ->
            NAcc;
        {1, _} ->
            right_diagonals(Ls, 1, J - 1, NAcc);
        {_, 20} ->
            right_diagonals(Ls, I - 1, 20, NAcc)
    end.

one_diagonal(Ls, I, J, Desp, Seq) when (I =< 20) and (J =< 20) and (J >= 1) ->
    one_diagonal(Ls, I + 1, Desp(J, 1), Desp, [get_element(Ls, I, J) | Seq]);
one_diagonal(_, _, _, _, Seq) ->
    Seq.

get_element(Ls, I, J) ->
    lists:nth(J, lists:nth(I, Ls)).

calculate_test(Ds) ->
    K = 4,
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

output_data(D) -> 
    io:format("~p\n", [D]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    Binary = read(),
    Res = binary:split(Binary, [<<"\n">>], [global]),
    GridStr = [binary_to_list(R) || R <- Res],
    lists:map(fun str2intlist/1, GridStr).

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

