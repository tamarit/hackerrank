% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/simplify-the-algebraic-expressions

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

% -record(node, 
%     {
%         left = none, 
%         op = , 
%         right = noe
%     }).

calculate(Ts) ->
    Parsed = lists:map(fun(T) -> parse_string(prepare_parsing(T, [])) end, Ts),
    lists:map(fun simplify/1, Parsed).


prepare_parsing([N,$x|T], Acc) when (((N =< $9) and (N >= $0))) ->
    prepare_parsing([$x | T], [$*,N | Acc]);
prepare_parsing([$x,$^,N|T], Acc) ->
    prepare_parsing(T, [$},N,$,,$X,${ | Acc]);
prepare_parsing([$x, $(|T], Acc) ->
    prepare_parsing(T, [$(,$*,$},$1,$,,$X,${ | Acc]);
prepare_parsing([$x|T], Acc) ->
    prepare_parsing(T, [$},$1,$,,$X,${ | Acc]);
prepare_parsing([N|T], Acc) ->
    prepare_parsing(T, [N | Acc]);
prepare_parsing([], Acc) ->
    lists:reverse([$.|Acc]).

parse_string(String) ->
    % io:format("~s\n", [String]),
    {ok, Ts, _} = erl_scan:string(String),
    {ok, [ListAST]} = erl_parse:parse_exprs(Ts),
    ListAST.

simplify({integer,_,A}) ->
    {var, A, 0};
simplify({op,_,'*', {integer,_,A}, {tuple,_,[_,{integer,_,N}]}}) ->
    % A*X^N
    {var, A, N};
simplify({tuple,_,[_,{integer,_,N}]}) ->
    % A*X^N
    {var, 1, N};
simplify({op,_,'+', T1, T2}) ->
    ST1 = simplify(T1),
    ST2 = simplify(T2),
    % io:format("ADD1: ~p\n", [{T1, T2}]),
    add(ST1, ST2);
simplify({op,_,'-', T1, T2}) ->
    ST1 = simplify(T1),
    ST2 = simplify(T2),
    sub(ST1, ST2);
simplify({op,_,'*', T1, T2}) ->
    ST1 = simplify(T1),
    ST2 = simplify(T2),
    mul(ST1, ST2);
simplify({op,_,'/', T1, T2}) ->
    ST1 = simplify(T1),
    ST2 = simplify(T2),
    coc(ST1, ST2);
simplify({op,_,'-', T1}) ->
    ST1 = simplify(T1),
    mul({var, -1, 0}, ST1).


add(V1 = {var, A, N}, V2 = {var, B, M}) ->
    if 
        N == M -> 
            {var, A + B, N};
        N < M ->
            {op, 1, '+', V2, V1};
        N > M ->
            {op, 1, '+', V1, V2}
    end;
add(V = {var, _, N}, Op = {op, _ , '+', V1 = {var, _, M1}, V2}) -> 
    % io:format("ADD2: ~p\n", [{V, Op}]),
    if
        N > M1 ->
            {op, 1, '+', V, Op};
        N == M1 ->
            {op, 1, '+', add(V, V1), V2};
        N < M1 ->
            {op, 1, '+', V1, add(V, V2)}
    end;
add({op, _ , '+', V1, V2}, Op = {op, _ , '+', _, _}) -> 
    % io:format("ADD3: ~p\n", [{{op, 1, '+', V1, V2}, Op}]),
    add(V2, add(V1, Op));
add(O1, O2) ->
    % io:format("ADD: ~p\n", [{O1, O2}]),
    add(O2, O1).

sub(V1, V2) ->
    add(V1, mul({var, -1, 0}, V2)).


mul({var, A, N}, {var, B, M}) ->
    {var, A * B, N + M};
mul(V = {var, _, _}, {op, _ , '+', V1, V2}) -> 
    add(mul(V, V1), mul(V, V2));
mul({op, _, '+', V1, V2}, Op = {op, _ , '+', _, _}) -> 
    % io:format("MUL2: ~p\n", [{{op, 1, '+', V1, V2}, Op}]),
    % io:format("MUL2: ~p\n", [{mul(V1, Op), mul(V2, Op)}]),
    add(mul(V1, Op), mul(V2, Op));
mul(O1, O2) ->
    % io:format("MUL: ~p\n", [{O1, O2}]),
    mul(O2, O1).

coc({var, A, N}, {var, B, M}) ->
    {var, A div B, N - M};
coc({op, _ , '+', V1, V2}, V = {var, _, _}) -> 
    add(coc(V1, V), coc(V2, V)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(Ds) -> 
    lists:map(
        % fun(D) -> io:format("~s\n", [to_str(D)]) end, 
        fun(D) -> io:format("~s\n", [to_str(D)]) end, 
        Ds).

to_str(E) -> 
   case to_str_aux(E) of 
        [$ , $+, $  | T] ->
            T;
        [$ , $-, $  | T] ->
            [$- | T]
    end.

to_str_aux({op, _, '+', Op1, Op2}) ->
    to_str_aux(Op1) ++ to_str_aux(Op2);
to_str_aux({var, A, N}) ->
    AStr = 
        if 
            ((A == 1) and (N /= 0) )->
                [$ ,$+, $ ];
            ((A == -1) and (N /= 0) )->
                [$ ,$-, $ ];
            ((A == 0) and (N /= 0) ) ->
                [];
            A < 0 ->
                [$ ,$-, $  | integer_to_list(abs(A))];
            A >= 0 ->
                [$ ,$+, $  | integer_to_list(A)]
        end,
    NStr = 
        if 
            ((N == 0) or (A == 0)) ->
                "";
            N == 1 ->
                "x";
            N > 1 ->
                [$x, $^ | integer_to_list(N)]
        end,
    AStr ++ NStr.



% 11x - 2
% 36x + 31
% -x + 18
% 12x^2 + 47x + 20
% 2x^3 + 23x^2 + 61x + 45
% 2x^5 + 5x^4 + 18x^2 + 61x + 45 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    Binary = read(),
    Res = binary:split(Binary, [<<"\n">>], [global]),
    [_| Tests] = [binary_to_list(R) || R <- Res],
    Tests.

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
