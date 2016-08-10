% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/contests/projecteuler/challenges/euler017

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

calculate_test(N) ->
    Digits = int2digits(N),
    to_letter(lists:reverse(Digits), ["", "Thousand", "Million", "Billion", "Trillion"], []).

char2int(X) ->
    X - 48.

int2digits(N) ->
    lists:map(fun char2int/1, integer_to_list(N)).

to_letter([0, 0, 0 | T], Sep, Acc) ->
    to_letter(T, tl(Sep), Acc);
to_letter([D1, D10, D100 | T], Sep, Acc0) ->
    % erlang:display({D1, D10, D100, Sep, Acc0}),
    Acc = [hd(Sep) | Acc0],
    NAcc = 
        case D10 of 
            1 ->
                ToAdd = 
                    case D1 of 
                        0 ->
                            "Ten";
                        1 ->
                            "Eleven";
                        2 ->
                            "Twelve";
                        3 ->
                            "Thirteen";
                        4 ->
                            "Fourteen";
                        5 ->
                            "Fifteen";
                        6 ->
                            "Sixteen";
                        7 ->
                            "Seventeen";
                        8 ->
                            "Eighteen";
                        9 ->
                            "Nineteen"
                    end,
                [ToAdd | Acc];
            _ ->
                StrD10 = 
                    case D10 of 
                        0 ->
                            "";
                        2 ->
                            "Twenty";
                        3 ->
                            "Thirty";
                        4 ->
                            "Forty";
                        5 ->
                            "Fifty";
                        6 ->
                            "Sixty";
                        7 ->
                            "Seventy";
                        8 ->
                            "Eighty";
                        9 ->
                            "Ninety"
                    end,
                [StrD10, digit_to_str(D1) | Acc]
        end,
    NNAcc = 
        case D100 of 
            0 ->
                NAcc;
            _ ->
               [digit_to_str(D100), "Hundred" | NAcc] 
        end,
    to_letter(T, tl(Sep), NNAcc);
to_letter([D1, D10], Sep, Acc) ->
    to_letter([D1, D10, 0], Sep, Acc);
to_letter([D1], Sep, Acc) ->
    to_letter([D1, 0, 0], Sep, Acc);
to_letter([], _, Acc) ->
    % erlang:display(Acc),
    string:join([L || L <- Acc, L /= []], " ").


digit_to_str(0) ->
    "";
digit_to_str(1) ->
    "One";
digit_to_str(2) ->
    "Two";
digit_to_str(3) ->
    "Three";
digit_to_str(4) ->
    "Four";
digit_to_str(5) ->
    "Five";
digit_to_str(6) ->
    "Six";
digit_to_str(7) ->
    "Seven";
digit_to_str(8) ->
    "Eight";
digit_to_str(9) ->
    "Nine".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(Ds) -> 
    lists:map(
        fun(D) -> io:format("~s\n", [D]) end, 
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

