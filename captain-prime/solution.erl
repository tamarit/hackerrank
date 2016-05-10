% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/captain-prime

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
    io:format("INPUT: total time taken ~p seconds~n", [timer:now_diff(os:timestamp(), StartInput)]),
    StartRes = os:timestamp(),
    Res = calculate(Data),
    io:format("CALCULATE: total time taken ~p seconds~n", [timer:now_diff(os:timestamp(), StartRes)]),
    StartOutput = os:timestamp(),
    output_data(Res),
    io:format("OUTPUT: total time taken ~p seconds~n", [timer:now_diff(os:timestamp(), StartOutput)]),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Calculate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calculate(Ts) ->
    lists:map(fun calculate_test/1, Ts).

calculate_test(T) ->
    case {is_prime(T), has_zeros(T)} of 
        {true, false} -> 
            case {seqPrimes(T, fun leftDigits/3), seqPrimes(T, fun rightDigits/3)} of 
                {true, true} ->
                    "CENTRAL";
                {false, true} ->
                    "RIGHT";
                {true, false} ->
                    "LEFT";
                {false, false} ->
                    "DEAD"
            end;
        _ ->
            "DEAD"
    end.

is_prime(T) ->
    factors(T) == [T].

has_zeros(N) ->
    StrN = 
        lists:flatten(io_lib:format("~p", [N])),
    lists:member($0, StrN).

seqPrimes(N,FunSeq) ->
    lists:all(
        fun is_prime/1,
        FunSeq(N, 10, [])
        ).

leftDigits(N, Div, Acc) when (N rem Div) < N ->
    leftDigits(N, Div * 10, [N rem Div|Acc]);
leftDigits(_, _, Acc) ->
    Acc.

rightDigits(N, Div, Acc) when (N div Div) > 0 ->
    rightDigits(N, Div * 10, [N div Div|Acc]);
rightDigits(_, _, Acc) ->
    Acc.


% **********************
% From http://rosettacode.org/wiki/Prime_decomposition#Erlang
% **********************
factors(N) ->
     factors(N, 2, []).
 
factors(1, _, Acc) -> 
    Acc;
factors(N, K, Acc) when N rem K == 0 ->
    factors(N div K, K, [K|Acc]);
factors(N, K, Acc) ->
    factors(N, K + 1, Acc).
% **********************

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(Data) -> 
    lists:foreach(
        fun(D) -> io:format("~s\n", [D]) end, 
        Data).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    Binary = read(),
    Res = binary:split(Binary, [<<"\n">>], [global]),
    [_ | Tests0] = [binary_to_list(R) || R <- Res],
    [str2int(T)  || T <- Tests0].

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

str2int(Str) ->
    element(1,string:to_integer(Str)).

