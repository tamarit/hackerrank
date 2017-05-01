% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/expressions

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

calculate(L) ->
    LengthL = length(L),
    L100s = length([100 || 100 <- L]),
    case (length([1 || 1 <- L]) + L100s) of 
        LengthL when L100s == 1 -> 
            special_solution(L, []);
        _ ->
            search_solution(L, [])
    end.

search_solution([H], Acc0) ->
    % io:format("Bas: ~p\n", [[H]]),
    Acc = 
        lists:reverse([H|Acc0]),
    case (eval(Acc, 0) rem 101) of 
        0 -> 
            Acc;
        _ ->
            none
    end;
search_solution([H|T], Acc) ->
    % io:format("Gen: ~p\n", [[H|T]]),
    case search_solution(T, [add, H| Acc]) of 
        none -> 
            case search_solution(T, [sub, H | Acc]) of 
                none -> 
                    search_solution(T, [mul , H| Acc]);
                Res -> 
                    Res
            end;
        Res ->
            Res
    end.

eval([add, H | T], Acc) ->
    eval(T, Acc + H);
eval([sub, H | T], Acc) ->
    eval(T, Acc - H);
eval([mul, H | T], Acc) ->
    eval(T, Acc * H);
eval([H | T], _) ->
    eval(T, H);
eval([], Acc) ->
    Acc.

special_solution([1 | T], Acc) ->
    case T of 
        [100] ->
            lists:reverse([100, add, 1 | Acc]);
        [1] ->
            lists:reverse([1, mul, 1 | Acc]);
        _ ->
            special_solution(T, [mul, 1 |Acc])
    end;
special_solution([100 | T], Acc) ->
    case T of 
        [1] ->
            lists:reverse([1, add, 100 | Acc]);
        _ ->
            special_solution(T, [add, 100 |Acc])
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(D) -> 
    print(D).

print([add | T]) ->
    io:format("+"),
    print(T);
print([sub | T]) ->
    io:format("-"),
    print(T);
print([mul | T]) ->
    io:format("*"),
    print(T);
print([H | T]) ->
    io:format("~p", [H]),
    print(T);
print([]) ->
    io:format("\n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    Binary = read(),
    Res = binary:split(Binary, [<<"\n">>], [global]),
    [_, Line] = [binary_to_list(R) || R <- Res],
    [str2int(T) || T <- string:tokens(Line, " ")].

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

