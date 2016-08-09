% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/contests/projecteuler/challenges/euler015

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

calculate_test({N, M}) ->
    choose(N + M, min(N, M)) rem trunc(math:pow(10,9)+7).

% *********************************
% BEGIN Copied from https://rosettacode.org/wiki/Evaluate_binomial_coefficients#Erlang
% *********************************

choose(N, K) when is_integer(N), is_integer(K), (N >= 0), (K >= 0), (N >= K) ->
  choose(N, K, 1, 1).
 
choose(N, K, K, Acc) ->
  (Acc * (N - K + 1)) div K;
choose(N, K, I, Acc) ->
  choose(N, K, I+1, (Acc * (N-I+1)) div I).

% *********************************
% END Copied from https://rosettacode.org/wiki/Evaluate_binomial_coefficients#Erlang
% *********************************
 

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
    [_|Ts] = [binary_to_list(R) || R <- Res],
    [Item 
      || Item = {_,_} <- lists:map(
            fun list_to_tuple/1, 
            lists:map(fun str2intlist/1, Ts)
        )].

str2int(Str) ->
    element(1, string:to_integer(Str)).  

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

