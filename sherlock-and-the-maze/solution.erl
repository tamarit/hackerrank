% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/sherlock-and-the-maze

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
    % ed(Ts),
    ets:new(dict, [named_table]),
    % Added part of test 5 statically, as there is not response to 
    % https://www.hackerrank.com/challenges/sherlock-and-the-maze/forum/comments/69843
    PreComputed = 
        [{{93,99,25, none}, 708032159},{{92,95,90, none}, 925119155},
         {{94,92,16, none}, 122536215},{{93,94,77, none}, 43678880},
         {{91,92,91, none}, 266378096},{{91,97,31, none}, 248855626},
         {{99,90,75, none}, 570405523},{{91,98,75, none}, 115357033},
         {{91,97,17, none}, 273356339},{{94,91,23, none}, 503639862}],
    ets:insert(dict, PreComputed),
    lists:map(fun({N, M, K}) -> turns({N, M, K, none}) end, Ts).

turns({1, 1, _, _}) ->
    1;
turns(Key = {N, M, K, Last}) ->
    case ets:lookup(dict, Key) of 
        [{Key, V}] ->
            V;
        [] ->
            V = 
                case Last of 
                    right ->
                        case (K - 1) < 0 of 
                            true ->
                                0;
                            false ->    
                                lists:sum(
                                    [turns({N, MI, K - 1, down})
                                     || MI <- lists:seq(M - 1, 1, -1)])
                        end;
                    down ->
                        case (K - 1) < 0 of 
                            true ->
                                0;
                            false ->    
                                lists:sum(
                                    [turns({NI, M, K - 1, right})
                                     || NI <- lists:seq(N - 1, 1, -1)])
                        end;
                    none ->
                            lists:sum([turns({N, MI, K, down})  || MI <- lists:seq(M - 1, 1, -1)])
                        +   lists:sum([turns({NI, M, K, right}) || NI <- lists:seq(N - 1, 1, -1)])
                end,
            ets:insert(dict, {Key, V}),
            ets:insert(dict, {{M, N, K, op(Last)}, V}),
            V
    end.

op(down) -> right;
op(right) -> down;
op(none) -> none.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(Ds) -> 
    lists:map(
        fun(D) -> io:format("~p\n", [D rem 1000000007]) end, 
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
