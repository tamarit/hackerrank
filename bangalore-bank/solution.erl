% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/bangalore-bank

-module(solution).
-export([main/0, prof/0]).

-include_lib("stdlib/include/ms_transform.hrl").

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
    NTs  =
        lists:map(fun corr/1, Ts),
    {V, _} = time_type(NTs, {none, none, dict:new()}),
    V.
    
time_type(List = [N | T], {L, R, Dict}) ->
    case dict:find({length(List), R, L}, Dict) of 
        {ok, Value} ->
            {Value, Dict};
        error -> 
            {Value, FDict} = 
                case {L,R} of 
                    {N,_} -> 
                        time_type(T, {N, R, Dict});
                    {_,N} -> 
                        time_type(T, {L, N, Dict});
                    _ ->
                        {LTime, NDict} = 
                            if 
                                L == none -> 
                                    time_type(T, {N, R, Dict});
                                true ->
                                    {LTime0, NDict0} = 
                                        time_type(T, {N , R, Dict}),
                                    {LTime0  + abs(L-N), NDict0}
                            end,
                        {RTime, NNDict} =
                            if 
                                R == none ->
                                    time_type(T, {L, N, NDict});
                                true  -> 
                                    {RTime0, NNDict0} =
                                        time_type(T, {L, N, NDict}),
                                    {RTime0 + abs(R-N), NNDict0}
                            end,
                        {min(LTime, RTime),NNDict}
                end,
            NValue = Value + 1,
            {NValue, dict:store({length(List), L, R}, NValue, FDict)}
    end;
time_type([], {_, _, Dict}) ->
    {0, Dict}.

corr(0) ->
    10;
corr(N) ->
    N.

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
    [_| [Test]] = [binary_to_list(R) || R <- Res],
    lists:map(fun str2int/1, string:tokens(Test, " ")).

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

