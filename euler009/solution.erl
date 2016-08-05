% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/contests/projecteuler/challenges/euler009

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
    pythagorean_triplet([{2, 1}, {3, 1}], T, -1).

pythagorean_triplet([{M, N} | T], Limit, Max) ->
    M2 = M * M,
    N2 = N * N,
    A = M2 - N2,
    B = 2 * M * N,
    C = M2 + N2,
    Sum = A + B + C,
    % io:format("~p\n", [{Sum, Limit, M, N, ((M - N) rem 2)}]),
    if 
        Sum > Limit ->
            pythagorean_triplet(T, Limit, Max);
        (Sum == Limit) and (((M - N) rem 2) == 1) ->
            pythagorean_triplet(T, Limit, max(A * B * C, Max));
        (Sum == Limit) and (((M - N) rem 2) == 0) ->
            pythagorean_triplet(T, Limit, Max);
        (Sum < Limit) and (((M - N) rem 2) == 1) ->
            NMax = max_in_multiples(2, A, B, C, Limit, Max),
            Next = next_coprimes(M, N),
            pythagorean_triplet(Next ++ T, Limit, NMax);
        (Sum < Limit) and (((M - N) rem 2) == 0) ->
            Next = next_coprimes(M, N),
            pythagorean_triplet(Next ++ T, Limit, Max)
        % ;true ->
        %     io:format("ERROR: ~p\n", [{Sum, Limit, M, N, ((M - N) rem 2)}])
    end;
pythagorean_triplet([], _, Max) ->
    Max.

max_in_multiples(K, A, B, C, Limit, Max) ->
    Sum = K * (A + B + C),
    if 
        Sum > Limit ->
            Max;
        Sum == Limit ->
            max(A * B * C * K * K * K, Max);
        Sum < Limit ->
            max_in_multiples(K + 1, A, B, C, Limit, Max)
    end.

next_coprimes(M, N) ->
    [{(2 * M) - N, M}, {(2 * M) + N, M}, {M + (2 * N), N}].

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

