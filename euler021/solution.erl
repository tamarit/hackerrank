% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/contests/projecteuler/challenges/euler021

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
    % [io:format("f(N) when N > ~p -> ~p + f(~p);\n", [N, N,N])|| N <- lists:reverse(g())].
    lists:map(fun f/1, Ts).

f(N) when N > 100485 -> 100485 + f(100485);
f(N) when N > 88730 -> 88730 + f(88730);
f(N) when N > 87633 -> 87633 + f(87633);
f(N) when N > 79750 -> 79750 + f(79750);
f(N) when N > 76084 -> 76084 + f(76084);
f(N) when N > 71145 -> 71145 + f(71145);
f(N) when N > 69615 -> 69615 + f(69615);
f(N) when N > 67095 -> 67095 + f(67095);
f(N) when N > 66992 -> 66992 + f(66992);
f(N) when N > 66928 -> 66928 + f(66928);
f(N) when N > 63020 -> 63020 + f(63020);
f(N) when N > 18416 -> 18416 + f(18416);
f(N) when N > 17296 -> 17296 + f(17296);
f(N) when N > 14595 -> 14595 + f(14595);
f(N) when N > 12285 -> 12285 + f(12285);
f(N) when N > 10856 -> 10856 + f(10856);
f(N) when N > 10744 -> 10744 + f(10744);
f(N) when N > 6368 -> 6368 + f(6368);
f(N) when N > 6232 -> 6232 + f(6232);
f(N) when N > 5564 -> 5564 + f(5564);
f(N) when N > 5020 -> 5020 + f(5020);
f(N) when N > 2924 -> 2924 + f(2924);
f(N) when N > 2620 -> 2620 + f(2620);
f(N) when N > 1210 -> 1210 + f(1210);
f(N) when N > 1184 -> 1184 + f(1184);
f(N) when N > 284 -> 284 + f(284);
f(N) when N > 220 -> 220;
f(_) -> 0.

g() ->
[220,
284,
1184,
1210,
2620,
2924,
5020,
5564,
6232,
6368,
10744,
10856,
12285,
14595,
17296,
18416,
63020,
66928,
66992,
67095,
69615,
71145,
76084,
79750,
87633,
88730,
100485].

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

