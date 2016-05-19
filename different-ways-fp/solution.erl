% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/different-ways-fp

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
    {Res,_} = 
        lists:mapfoldl(
            fun count/2 ,
            dict:new(),
            Ts),
    Res.

%               1                                   , K = 0
% count(N, K) = 1                                   , K = N
%               count(N-1, K-1) + count(N-1, K),    , 0 < K < N
count({_, 0}, Dict) ->
    {1, Dict};
count({K, K}, Dict) ->
    {1, Dict};
count({N, K}, Dict) ->
    case dict:find({N, K}, Dict) of 
        {ok, Value} ->
            {Value, Dict};
        error -> 
            {Value1, NDict0} = 
                count({N - 1, K - 1}, Dict),
            {Value2, NDict} = 
                count({N - 1, K}, NDict0),
            NValue = 
                Value1 + Value2,
            {NValue , dict:store({N, K}, NValue, NDict)}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(Ds) -> 
    lists:foreach(
        fun(D) -> io:format("~p\n", [D rem trunc(math:pow(10, 8) + 7)]) end, 
        Ds).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    Binary = read(),
    Res = binary:split(Binary, [<<"\n">>], [global]),
    [_| Tests] = [binary_to_list(R) || R <- Res],
    lists:map(fun str2tupleint/1, Tests).


str2tupleint(Str) ->
    [A, B] = string:tokens(Str, " "),
    {str2int(A), str2int(B)}.

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

