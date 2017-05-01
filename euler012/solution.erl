% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/contests/projecteuler/challenges/euler012

-module(solution).
-export([main/0, prof/0, total_factors/1]).

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

calculate_test(T) ->
    first_with_n(T, 1, 1, 1, 1).

first_with_n(N, I, TN, DO, DE) ->
    {NDO, NDE} =
        case ((I rem 2) == 0) of 
            true ->
                {DO, total_factors(I + 1)};
            false ->
                {total_factors((I + 1) div 2), DE}
        end,
    % io:format("~p\n", [{I, TN, Cn}]),
    case (NDO * NDE) > N of 
        true ->
            TN;
        false ->
            NI = I + 1,
            first_with_n(N, I + 1, TN + NI, NDO, NDE)
    end.

total_factors(1) ->
    1;
total_factors(N) ->
    Root = trunc(math:sqrt(N)),
    Total = total_factors(N, Root, 2, 2),
    case trunc(Root * Root) of
        N ->
            Total - 1;
        _ ->
            Total
    end.

total_factors(_, Root, I, Acc) when I > Root ->
    Acc;
total_factors(N, Root, I, Acc) when (N rem I) == 0 ->
    total_factors(N, Root, I + 1, Acc + 2);
total_factors(N, Root, I, Acc)  ->
    total_factors(N, Root, I + 1, Acc).


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

