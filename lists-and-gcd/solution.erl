% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/lists-and-gcd

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


calculate(Fs) ->
    gcd(tl(Fs), hd(Fs)).


gcd([H|T], Acc) ->
    gcd(T, gcd(H, Acc, []));
gcd([], Acc) ->
    Acc.

gcd([N, MA|TA], [N, MB|TB], Acc) ->
    gcd(TA, TB, [min(MA, MB), N| Acc]);
gcd([NA, _|TA], LB = [NB, _|_], Acc) when NA < NB ->
    gcd(TA, LB, Acc);
gcd(LA = [NA, _|_], [NB, _|TB], Acc) when NA >= NB ->
    gcd(LA, TB, Acc);
gcd([], _, Acc) ->
    lists:reverse(Acc);
gcd(_, [], Acc) ->
    lists:reverse(Acc).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(Ds) ->
    StrDs = string:join([integer_to_list(D) || D <- Ds], " "),
    io:format("~s\n", [StrDs]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    Binary = read(),
    Res = binary:split(Binary, [<<"\n">>], [global]),
    [_ | FsStr] = [binary_to_list(R) || R <- Res],
    lists:map(fun str2intlist/1, FsStr).

str2int(Str) ->
    element(1,string:to_integer(Str)).

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



