% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/super-queens-on-a-chessboard

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

calculate(N) ->
    length(generate(N, N)).

generate(0, _) ->
    [[]];
generate(K, N) ->
    [[Q | Qs] || Qs <- generate(K - 1, N), Q <- lists:seq(1, N), is_safe(Q, Qs)].

is_safe(Try, Qs) ->
    not(
        (lists:member(Try, Qs))
        orelse
        (same_diag(Try, Qs))
        orelse
        (hourse_movement(Try, Qs, 2))
    ).

same_diag(Try, Qs) ->
    lists:any(
        fun({ColDist, Q}) ->
            abs(Try - Q) == ColDist
        end,
        lists:zip(lists:seq(1, length(Qs)), Qs)
    ).

hourse_movement(_, [], _) ->
    false;
hourse_movement(Try, [Q | _], 1) ->
    ((Try + 1) == Q) orelse ((Try - 1) == Q);
hourse_movement(Try, [Q | Qs], 2) ->
    ((Try + 2) == Q) orelse ((Try - 2) == Q) orelse hourse_movement(Try, Qs, 1).
    

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
    [NStr] = [binary_to_list(R) || R <- Res],
    N = str2int(NStr),
    N.


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
