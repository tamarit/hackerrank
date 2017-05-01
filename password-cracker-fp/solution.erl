% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/password-cracker-fp

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
    lists:map(fun try_login/1, Ts).

-define (WRONG, ["WRONG", "PASSWORD"]).

try_login({Pass, LA}) ->
    try_login(
        {lists:map(fun lists:reverse/1, Pass), LA}, 
        [], 
        [],
        dict:new()).

try_login({Pass, AL =[H|T]}, Curr, Acc, DictPrev) ->
    % io:format("~p\n", [{{Pass, [H|T]}, Curr, Acc, dict:to_list(DictPrev)}]),
    case dict:find(AL, DictPrev) of
        {ok, Value} -> 
            Value;
        error ->
            NCurr = [H|Curr],
            case lists:member(NCurr, Pass) of 
                true -> 
                    Taking = try_login({Pass, T}, [], [NCurr | Acc], DictPrev),
                    case Taking of 
                        ?WRONG ->
                            try_login({Pass, T}, NCurr, Acc, dict:store(T, ?WRONG, DictPrev));
                        _ ->
                            Taking
                    end;
                false ->
                    try_login({Pass, T}, NCurr, Acc, DictPrev)  
            end
    end;
try_login({_, []}, [], Acc, _) ->
    lists:map(
        fun lists:reverse/1, 
        lists:reverse(Acc)
    );
try_login({_, []}, [_|_], _, _) ->
    ?WRONG.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(Ds) -> 
    Lines = 
        lists:map(
            fun(D) -> 
                lists:flatten(
                    io_lib:format(
                        "~s", 
                        [string:join(D, " ")]
                    )
                )
            end, 
            Ds),
    io:format("~s", [string:join(Lines, "\n")]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    Binary = read(),
    Res = binary:split(Binary, [<<"\n">>], [global]),
    [_| Tests] = [binary_to_list(R) || R <- Res],
    read_tests(Tests, []).

read_tests([_, PassStr, LA |Tail], Acc) ->
    read_tests(
        Tail, 
        [{string:tokens(PassStr, " "),  LA} | Acc]);
read_tests([], Acc) ->
    lists:reverse(Acc).

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

