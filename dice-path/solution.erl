% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/dice-path

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

-record(dice, 
        {
            top = 1,
            bottom = 6,
            front = 2,
            back = 5,
            left = 3,
            right = 4
        }).


calculate(Tests) ->
    ets:new(t, [set, named_table]),
    IdedTests = lists:zip(lists:seq(1,length(Tests)), Tests),
    IdedTestsSortedByTest = 
        lists:sort(
            fun({_,[M1,N1]}, {_,[M2,N2]}) -> 
                ((M1 > M2) and (N1 > N2)) or ((M1 == M2) and (N1 > N2)) or  ((M1 > M2) and (N1 == N2))
            end, 
            IdedTests),
    Calulated = 
        [{Id, calculate_test(Test)} || {Id, Test} <- IdedTestsSortedByTest],
    CalulatedSorted = 
        lists:sort(
            fun({I1,_}, {I2,_}) -> 
                I1 < I2
            end, 
            Calulated),
    [Res || {_, Res} <- CalulatedSorted].

calculate_test([M,N]) ->
    case ets:lookup(t, {M, N}) of
        [] -> 
            ets:new(p, [set, named_table]),
            move(#dice{}, {1, 1}, {M, N}, 0, []);
        [{_,Value}] ->
            % io:format("Found: ~p\n", [{{M, N}, Value}]),
            Value
    end.

move(Dice, {CM, CN}, {CM, CN}, Acc, _) ->
    insert_new_value({CM, CN}, Acc + Dice#dice.top),
    Res = Acc + Dice#dice.top,
    % io:format("~p\n", [{Dice, {CM, CN}, Res, lists:reverse(Stack)}]),
    Res;
move(Dice, {CM, CN}, {M, CN}, Acc, Stack) ->
    Key = {Dice, {CM, CN}},
    Res = 
        case ets:lookup(p, Key) of 
            [{{Dice, {CM, CN}}, CValue}] ->
                CValue;
            [] ->
                Res0 = move(down(Dice),  {CM + 1, CN}, {M, CN}, Acc + Dice#dice.top, [down | Stack]),
                ets:insert(p,{Key, Res0}),
                insert_new_value({CM, CN}, Acc + Dice#dice.top),
                Res0
        end,
    % io:format("~p\n", [{Dice, {CM, CN}, Res, lists:reverse(Stack)}]),
    Res;
move(Dice, {CM, CN}, {CM, N}, Acc, Stack) ->
    Key = {Dice, {CM, CN}},
    Res = 
        case ets:lookup(p, Key) of 
            [{{Dice, {CM, CN}}, CValue}] ->
                CValue;
            [] ->
                Res0 = move(right(Dice), {CM, CN + 1}, {CM, N}, Acc + Dice#dice.top, [right | Stack]),
                ets:insert(p,{Key, Res0}),
                insert_new_value({CM, CN}, Acc + Dice#dice.top),
                Res0
        end,
    % io:format("~p\n", [{Dice, {CM, CN}, Res, lists:reverse(Stack)}]),
    Res;
move(Dice, {CM, CN}, {M, N}, Acc, Stack) ->
    Key = {Dice, {CM, CN}},
    Res = 
        case ets:lookup(p, Key) of 
            [{{Dice, {CM, CN}}, CValue}] ->
                % io:format("FOUND: ~p\n", [{Dice, {CM, CN}, CValue, lists:reverse(Stack)}]),
                CValue;
            [] ->
                Res0 = max(
                    move(down(Dice),  {CM + 1, CN}, {M, N}, Acc + Dice#dice.top, [down | Stack]),
                    move(right(Dice), {CM, CN + 1}, {M, N}, Acc + Dice#dice.top, [right | Stack])
                ),
                ets:insert(p,{Key, Res0}),
                insert_new_value({CM, CN}, Acc + Dice#dice.top),
                Res0
        end,
    % io:format("~p\n", [{Dice, {CM, CN}, Res, lists:reverse(Stack)}]),
    Res.

insert_new_value(Key, NewValue0) ->
    NewValue = 
        case ets:lookup(t, Key) of 
            [{Key, CValue}] ->
                max(NewValue0, CValue);
            [] ->
                NewValue0
        end,
    ets:insert(t,{Key, NewValue}).


right(Dice = #dice{top = Top, bottom = Bot, left = Lef, right = Rig}) ->
    Dice#dice{top = Lef, bottom = Rig, left = Bot, right = Top}.

down(Dice = #dice{top = Top, bottom = Bot, front = Fro, back = Bac}) ->
    Dice#dice{top = Bac, bottom = Fro, front = Top, back = Bot}. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(Ds) -> 
    lists:foreach(fun(D) -> io:format("~p\n", [D]) end, Ds).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    Binary = read(),
    Res = binary:split(Binary, [<<"\n">>], [global]),
    [_|Ts] = [binary_to_list(R) || R <- Res],
    lists:map(fun(S) -> [str2int(T) || T <- string:tokens(S, " ")] end, Ts).

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

