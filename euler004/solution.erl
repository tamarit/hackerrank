% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/contests/projecteuler/challenges/euler004

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

calculate_test(T) ->
    largest_palindrome(T, 100, 100, -1).


largest_palindrome(_, _, J, Max) when (J >= 1000) ->
    Max;
largest_palindrome(T, I, J, Max) when (I >= 1000) ->
    largest_palindrome(T, 100, J + 1, Max) ;
largest_palindrome(T, I, J, Max) when (I < 1000) and (J < 1000)->
    IJ = I * J,
    case IJ < T of 
        true ->
            case IJ > Max of 
                true ->
                    case is_palindrome(IJ) of
                        true ->
                           largest_palindrome(T, I + 1, J, IJ);
                        false ->
                           largest_palindrome(T, I + 1, J, Max) 
                    end;  
                false ->
                    largest_palindrome(T, I + 1, J, Max)   
            end;
        false ->
            largest_palindrome(T, 100, J + 1, Max)
    end.

% is_palindrome(N) ->
%     NStr = integer_to_list(N),
%     Length = length(NStr),
%     Half = Length div 2,
%     Rem = Length rem 2,
%     lists:sublist(NStr, Half) == lists:reverse(lists:sublist(NStr, Half + 1 + Rem, Length)).

is_palindrome(N) ->
    NStr = integer_to_list(N),
    is_palindrome(NStr, lists:reverse(NStr)).


is_palindrome([H|T1], [H|T2]) ->
    is_palindrome(T1, T2);
is_palindrome([_|_], [_|_]) ->
    false;
is_palindrome(_, _) ->
    true.

 
% ********************** 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(Ds) -> 
    lists:map(
        fun(D) -> io:format("~p\n", [D]) end, 
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

