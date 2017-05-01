% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/matrix-rotation

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


calculate({M, N, R, Array}) ->
    TupledArray = 
        list_to_tuple([list_to_tuple(L) || L <- Array]),
    % io:format("~p\n", [TupledArray]),
    Qs = create_queues(M, N, TupledArray, 0, []),
    % io:format("~p\n", [[queue:to_list(Q) || Q <- Qs]]),
    NQs = rotate(Qs, R, []),
    % io:format("~p\n", [[queue:to_list(Q) || Q <- NQs]]),
    NTupledArray = 
        uncreate_queues(M, N, TupledArray, 0, NQs),
    % io:format("~p\n", [NTupledArray]),
    NArray = 
        [tuple_to_list(T) || T <- tuple_to_list(NTupledArray)],
    NArray.


create_queues(M, N, TupledArray, Inc, Acc) when ((M > 0) and (N > 0)) ->
    Q = create_queue(1, 1, M , N, Inc, TupledArray, queue:new(), false),
    create_queues(M - 2, N - 2, TupledArray, Inc + 1, [Q | Acc]);
create_queues(_, _, _,_, Acc) ->
    lists:reverse(Acc).

uncreate_queues(M, N, TupledArray, Inc, [Q | Qs]) when ((M > 0) and (N > 0)) ->
    NTupledArray = uncreate_queue(1, 1, M , N, Inc, TupledArray, queue:to_list(Q)),
    uncreate_queues(M - 2, N - 2, NTupledArray, Inc + 1, Qs);
uncreate_queues(_, _, TupledArray,_, _) ->
    TupledArray.
    

create_queue(1, 1, _, _, _, _, Q, true) ->
    Q;
create_queue(I, J, M, N, Inc, Ts, Q, _) when ((I < M) and (J == 1)) ->
    E = element(J + Inc, element(I + Inc, Ts)),
    % io:format("~p\n", [{I,J, E}]),
    create_queue(I + 1, J, M, N, Inc, Ts, queue:in(E, Q), true);
create_queue(I, J, M, N, Inc, Ts, Q, _) when ((I == M) and (J < N)) ->
    E = element(J + Inc, element(I + Inc, Ts)),
    % io:format("~p\n", [{I,J, E}]),
    create_queue(I, J + 1, M, N, Inc, Ts, queue:in(E, Q), true); 
create_queue(I, J, M, N, Inc, Ts, Q, _) when ((I > 1) and (J == N)) ->
    E = element(J + Inc, element(I + Inc, Ts)),
    % io:format("~p\n", [{I,J,E}]),
    create_queue(I - 1, J, M, N, Inc, Ts, queue:in(E, Q), true); 
create_queue(I, J, M, N, Inc, Ts, Q, _) when ((I == 1) and (J > 1)) ->
    E = element(J + Inc, element(I + Inc, Ts)),
    % io:format("~p\n", [{I,J, E}]),
    create_queue(I, J - 1, M, N, Inc, Ts, queue:in(E, Q), true).

uncreate_queue(1, 1, _, _, _, TupledArray, []) ->
    TupledArray;
uncreate_queue(I, J, M, N, Inc, Ts, [H|T]) when ((I < M) and (J == 1)) ->
    NTJ = setelement(J + Inc, element(I + Inc, Ts), H),
    NTs = setelement(I + Inc, Ts, NTJ),
    % io:format("~p\n", [{I,J, E}]),
    uncreate_queue(I + 1, J, M, N, Inc, NTs, T);
uncreate_queue(I, J, M, N, Inc, Ts, [H|T]) when ((I == M) and (J < N)) ->
    NTJ = setelement(J + Inc, element(I + Inc, Ts), H),
    NTs = setelement(I + Inc, Ts, NTJ),
    % io:format("~p\n", [{I,J, E}]),
    uncreate_queue(I, J + 1, M, N, Inc, NTs, T); 
uncreate_queue(I, J, M, N, Inc, Ts, [H|T]) when ((I > 1) and (J == N)) ->
    NTJ = setelement(J + Inc, element(I + Inc, Ts), H),
    NTs = setelement(I + Inc, Ts, NTJ),
    % io:format("~p\n", [{I,J,E}]),
    uncreate_queue(I - 1, J, M, N, Inc, NTs, T); 
uncreate_queue(I, J, M, N, Inc, Ts, [H|T]) when ((I == 1) and (J > 1)) ->
    NTJ = setelement(J + Inc, element(I + Inc, Ts), H),
    NTs = setelement(I + Inc, Ts, NTJ),
    % io:format("~p\n", [{I,J, E}]),
    uncreate_queue(I, J - 1, M, N, Inc, NTs, T).

rotate([L|Ls], R0, Acc) ->
    R = R0 rem queue:len(L),
    NL = lists:foldl(fun rotate_one/2, L, lists:seq(1, R)),
    rotate(Ls, R0, [NL|Acc]);
rotate([], _, Acc) ->
    lists:reverse(Acc).

rotate_one(_,Q) ->
    {{value, V}, NQ} = queue:out_r(Q),
    queue:in_r(V, NQ).

    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(Dss) ->
    StrDss =  
        string:join(
            [string:join([integer_to_list(D) || D <- Ds], " ") 
            || Ds <- Dss],
            "\n"),
    io:format("~s", [StrDss]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    Binary = read(),
    Res = binary:split(Binary, [<<"\n">>], [global]),
    [MNRStr | ArraysStr] = [binary_to_list(R) || R <- Res],
    [M,N,R] = str2intlist(MNRStr),
    {M, N, R, lists:map(fun str2intlist/1, ArraysStr)}.

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



