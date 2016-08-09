% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/contests/projecteuler/challenges/euler014

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
    lists:map(fun steps_to_one/1, Ts).

steps_to_one(1) ->
    1;
steps_to_one(2) ->
    2;
steps_to_one(N) ->
    % io:format("\n\nsteps_to_one: ~p\n", [N]),
    f(N).


f(N) when N >= 3732423 -> 3732423;
f(N) when N >= 3542887 -> 3542887;
f(N) when N >= 3064033 -> 3064033;
f(N) when N >= 2298025 -> 2298025;
f(N) when N >= 1723519 -> 1723519;
f(N) when N >= 1564063 -> 1564063;
f(N) when N >= 1501353 -> 1501353;
f(N) when N >= 1126015 -> 1126015;
f(N) when N >= 1117065 -> 1117065;
f(N) when N >= 837799 -> 837799;
f(N) when N >= 626331 -> 626331;
f(N) when N >= 511935 -> 511935;
f(N) when N >= 410011 -> 410011;
f(N) when N >= 230631 -> 230631;
f(N) when N >= 216367 -> 216367;
f(N) when N >= 156159 -> 156159;
f(N) when N >= 142587 -> 142587;
f(N) when N >= 106239 -> 106239;
f(N) when N >= 77031 -> 77031;
f(N) when N >= 52527 -> 52527;
f(N) when N >= 35655 -> 35655;
f(N) when N >= 35497 -> 35497;
f(N) when N >= 34239 -> 34239;
f(N) when N >= 26623 -> 26623;
f(N) when N >= 23529 -> 23529;
f(N) when N >= 17673 -> 17673;
f(N) when N >= 17647 -> 17647;
f(N) when N >= 13255 -> 13255;
f(N) when N >= 10971 -> 10971;
f(N) when N >= 6171 -> 6171;
f(N) when N >= 3711 -> 3711;
f(N) when N >= 2919 -> 2919;
f(N) when N >= 2463 -> 2463;
f(N) when N >= 2323 -> 2323;
f(N) when N >= 2322 -> 2322;
f(N) when N >= 2223 -> 2223;
f(N) when N >= 1161 -> 1161;
f(N) when N >= 871 -> 871;
f(N) when N >= 703 -> 703;
f(N) when N >= 667 -> 667;
f(N) when N >= 655 -> 655;
f(N) when N >= 654 -> 654;
f(N) when N >= 649 -> 649;
f(N) when N >= 327 -> 327;
f(N) when N >= 313 -> 313;
f(N) when N >= 235 -> 235;
f(N) when N >= 231 -> 231;
f(N) when N >= 171 -> 171;
f(N) when N >= 129 -> 129;
f(N) when N >= 97 -> 97;
f(N) when N >= 73 -> 73;
f(N) when N >= 55 -> 55;
f(N) when N >= 54 -> 54;
f(N) when N >= 27 -> 27;
f(N) when N >= 25 -> 25;
f(N) when N >= 19 -> 19;
f(N) when N >= 18 -> 18;
f(N) when N >= 9 -> 9;
f(N) when N >= 7 -> 7;
f(N) when N >= 6 -> 6;
f(N) when N >= 3 -> 3;
f(N) when N >= 2 -> 2;
f(N) when N >= 1 -> 1.


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

