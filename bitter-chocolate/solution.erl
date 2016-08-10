% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/bitter-chocolate

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
    ets:new(dict, [named_table]),
    ets:insert(dict, [{{1, 0, 0}, false}, {{1, 1, 0}, true}, {{2, 0, 0}, true}]),
    lists:map(fun can_win/1, Ts).

can_win(K = {R1, R2, R3}) ->
    % io:format("CW: ~p\n", [{Player, {R1, R2, R3}}]),
    case ets:lookup(dict, K) of 
        [{K, CW}] ->
            CW;
        [] ->
            CW = 
                lists:any(fun(T) -> T end,
                    [ not(can_win(substract({R1, R2, R3}, {S1, S1, S1})))
                     || S1 <- seq(R1 - 1)] ++ 
                    [ not(can_win(substract({R1, R2, R3}, {0, S2, S2})))
                     || S2 <- seq(R2)] ++
                    [ not(can_win(substract({R1, R2, R3}, {0, 0, S3})))
                     || S3 <- seq(R3)]),
            ets:insert(dict, {K, CW}),
            CW
    end.

substract({R1, R2, R3}, {0, 0, S3}) ->
    {R1, R2, sub(R3, S3)};
substract({R1, R2, R3}, {0, S2, S2}) ->
    Sub = sub(R2, S2),
    {R1, Sub, min(Sub, R3)};
substract({R1, R2, R3}, {S1, S1, S1}) ->
    Sub = sub(R1, S1),
    {Sub, min(Sub, R2), min(Sub, R3)}.

sub(A, B) when A >= B ->
    A - B;
sub(_, _)  ->
    0.

seq(R) ->
    case R > 0 of 
        true ->
            lists:seq(1, R);
        false ->
            []
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(Ds) -> 
    lists:map(
        fun(D) -> io:format("~s\n", [if D -> "WIN"; true -> "LOSE" end]) end, 
        Ds).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    Binary = read(),
    Res = binary:split(Binary, [<<"\n">>], [global]),
    [_| Tests] = [binary_to_list(R) || R <- Res],
    lists:map(fun(T) -> list_to_tuple(str2intlist(T)) end, Tests).

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
