% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/reverse-factorization

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

calculate({N, L}) ->
    search_sol(lists:reverse(lists:sort(L)), N, []).

search_sol(_, 1, Acc) ->
    Acc;
search_sol([], _, _) ->
    -1;
search_sol([H|T], N, Acc) ->
    % io:format("~p\n", [{N, [H|T], Acc}]),
    case (N rem H) of 
        0 ->
            search_sol([H|T], N div H, [H | Acc]);
        _ ->
            search_sol(T, N, Acc) 
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(-1) ->
    io:format("-1\n");
output_data(D) -> 
    L = 
        [1 |  
         element(
            1, 
            lists:mapfoldl(
                fun(X, Acc) -> 
                    {X * Acc, X * Acc} 
                end, 
                1, 
                D)
            )
        ],
    io:format(
        "~s\n", 
        [string:join(
            [io_lib:format("~p", [S]) || S <- L],
            " ")
        ]
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    Binary = read(),
    Res = binary:split(Binary, [<<"\n">>], [global]),
    [NStr, Ts] = [binary_to_list(R) || R <- Res],
    N = hd(strJoint2intList(NStr)),
    {N, strJoint2intList(Ts)}.

str2int(Str) ->
    element(1,string:to_integer(Str)).


strJoint2intList(Str) ->
    [str2int(T) || T <- string:tokens(Str, " ")].

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

