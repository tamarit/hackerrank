% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/intuitive-language

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
    Parsed = lists:map(
        fun(T) -> 
            parse_line(
                prepare_parsing(T)
            ) 
        end, 
        Ts),
    lists:foldl(
        fun eval_line/2,
        dict:new(),
        Parsed).

% Parsing
%------------------------------

prepare_parsing(Line) ->
    [   string:strip(T) 
     || T <- string:tokens(
                    string:to_lower(Line), " ")].

parse_line(["what", "is" | Rest]) ->
    {what, parse_what_content(Rest)};
parse_line(["assign" | Rest]) ->
    {assign, parse_assign_content(Rest)};
parse_line(["do" | Rest]) ->
    {do, parse_do_content(Rest)};
parse_line([Fun, "is", "function", "of" | Rest]) ->
    {fun_decl, Fun, parse_fun_content(Rest)};
parse_line([Var, "is" | Rest]) ->
    {var_decl, Var, parse_var_content(Rest)}.


parse_what_content(List) ->
    [parse_expr(E) || [E] <- separate_ands(List, [], [])].
parse_assign_content(List) ->
    Res = 
        [begin
            [Var, "to" | ExprR] = lists:reverse(Assign),
            {parse_expr(string:join(lists:reverse(ExprR), "")), parse_expr(Var)} 
         end
        || Assign <- separate_ands(List, [], [])],
    Res.
parse_do_content([[${ | Iter0] | List]) ->
    Iter = lists:droplast(Iter0),
    {parse_expr(Iter), parse_line(List)}.
parse_fun_content([Arity0 | Coeffs0]) ->
    Arity = lists:droplast(Arity0),
    Coeffs = 
        [lists:droplast(C) ||  C <- Coeffs0],
    {parse_expr(Arity), lists:map(fun parse_expr/1, Coeffs)}.
parse_var_content([Expr0]) ->
    parse_expr(lists:droplast(Expr0)).

separate_ands(["and" | List], CAcc, TAcc) ->
    separate_ands(List, [], [lists:reverse(CAcc) | TAcc]);
separate_ands([Other | List], CAcc, TAcc) ->
    separate_ands(List, [Other | CAcc], TAcc);
separate_ands([], [HCAcc0 | TCAcc], TAcc) ->
    HCAcc = lists:droplast(HCAcc0),
    CAcc = lists:reverse([HCAcc | TCAcc]),
    lists:reverse([CAcc | TAcc]).

separate_by_operators([$+ | Rest], 0, true, CAcc, TAcc) ->
    separate_by_operators(Rest, 0, false, [], [add, lists:reverse(CAcc) | TAcc]);
separate_by_operators([$- | Rest], 0, true, CAcc, TAcc) ->
    separate_by_operators(Rest, 0, false, [], [sub, lists:reverse(CAcc) | TAcc]);
separate_by_operators([$* | Rest], 0, true, CAcc, TAcc) ->
    separate_by_operators(Rest, 0, false, [], [mul, lists:reverse(CAcc) | TAcc]);
separate_by_operators([$/ | Rest], 0, true, CAcc, TAcc) ->
    separate_by_operators(Rest, 0, false, [], [coc, lists:reverse(CAcc) | TAcc]);
separate_by_operators([$( | Rest], N, _, CAcc, TAcc) ->
    separate_by_operators(Rest, N + 1, true, [$( | CAcc], TAcc);
separate_by_operators([$) | Rest], N, _, CAcc, TAcc) ->
    separate_by_operators(Rest, N - 1, true, [$) | CAcc], TAcc);
separate_by_operators([$[ | Rest], N, _, CAcc, TAcc) ->
    separate_by_operators(Rest, N + 1, true, [$[ | CAcc], TAcc);
separate_by_operators([$] | Rest], N, _, CAcc, TAcc) ->
    separate_by_operators(Rest, N - 1, true, [$] | CAcc], TAcc);
separate_by_operators([Other | Rest], N, _, CAcc, TAcc) ->
    separate_by_operators(Rest, N, true, [Other | CAcc], TAcc);
separate_by_operators([], 0, _, CAcc, TAcc) ->
    lists:reverse([lists:reverse(CAcc) | TAcc]).

process_mulcoc_tokens([T1, mul, T2 | Rest]) ->
    process_mulcoc_tokens([{mul, parse_token(T1), parse_token(T2)} | Rest]);
process_mulcoc_tokens([T1, coc, T2 | Rest]) ->
    process_mulcoc_tokens([{coc, parse_token(T1), parse_token(T2)} | Rest]);
process_mulcoc_tokens([T1, Op, T2 | Rest]) ->
    [parse_token(T1), Op | process_mulcoc_tokens([T2 | Rest])];
process_mulcoc_tokens([T]) ->
    [parse_token(T)];
process_mulcoc_tokens([]) ->
    [].

process_addsub_tokens([T1, add, T2 | Rest]) ->
    process_addsub_tokens([{add, parse_token(T1), parse_token(T2)} | Rest]);
process_addsub_tokens([T1, sub, T2 | Rest]) ->
    process_addsub_tokens([{sub, parse_token(T1), parse_token(T2)} | Rest]);
process_addsub_tokens([T]) ->
    [parse_token(T)];
process_addsub_tokens([]) ->
    [].

parse_expr(String) ->
    TokOpr = separate_by_operators(String, 0, false, [], []),
    hd(process_addsub_tokens(process_mulcoc_tokens(TokOpr))).


parse_token([First | Rest]) ->
    if 
        First >= $0 andalso First =< $9 ->
            % Positive number
            {Number0, []} = 
                lists:splitwith(
                    fun(A) -> 
                        A >= $0 andalso A =< $9 
                    end, 
                    Rest),
            Number = list_to_integer([First | Number0]),
            {number, Number};
        First == $- ->
            % Negative number
            {Number0, []} = 
                lists:splitwith(
                    fun(A) -> 
                        A >= $0 andalso A =< $9 
                    end, 
                    Rest),
            Number = list_to_integer(Number0),
            {number, -Number};
        First == $( ->
            % Bracketed expression
            {ExprStr, []} = find_end_bracket($(, $), Rest, 0, []),
            parse_expr(ExprStr);
        First >= $a andalso First =< $z ->
            % Variable or function
            {Name0, NotName} = 
                lists:splitwith(
                    fun(A) -> 
                        (A >= $a andalso A =< $z) orelse (A >= $0 andalso A =< $9)
                    end, 
                    Rest),
            Name = [First | Name0],
            {Term, []} = 
                case NotName of 
                    [$[ | _] ->
                        {Indices, NotIndices} = read_indices(NotName, []),
                        {{call, Name, Indices}, NotIndices};
                    _ ->
                        {{var, Name}, NotName}
                end,
            Term
    end;
parse_token(Other) ->
    Other.

read_indices([$[ | Rest], Acc) ->
    {Index, RestIndices} =     
        find_end_bracket($[, $], Rest, 0, []),
    read_indices(RestIndices, [parse_expr(Index) | Acc]);
read_indices(List, Acc) ->
    {lists:reverse(Acc), List}.

find_end_bracket(_, Closer, [Closer | Rest], 0, Acc) ->
    {lists:reverse(Acc), Rest};
find_end_bracket(Opener, Closer, [Closer | Rest], N, Acc) ->
    find_end_bracket(Opener, Closer, Rest, N - 1, [Closer | Acc]);
find_end_bracket(Opener, Closer, [Opener | Rest], N, Acc) ->
    find_end_bracket(Opener, Closer, Rest, N + 1, [Opener | Acc]);
find_end_bracket(Opener, Closer, [Other | Rest], N, Acc) ->
    find_end_bracket(Opener, Closer, Rest, N, [Other | Acc]);
find_end_bracket(_, _, [], _, Acc) ->
    {lists:reverse(Acc), []}.


% Evaluation
%------------------------------

eval_line({var_decl, V, E}, Env) ->
    dict:store(V, eval_expr(E, Env), Env);
eval_line({fun_decl, V, {Arity, Coeffs}}, Env) ->
    EArity = 
        eval_expr(Arity, Env),
    Val = 
        case EArity of 
            0 ->
                eval_expr(hd(Coeffs), Env);
            _ -> 
                {fun_decl, 
                    EArity, 
                    lists:map(
                        fun(C) -> 
                            eval_expr(C, Env) 
                        end, 
                        Coeffs)}
        end,
    dict:store(V, Val, Env);
eval_line({what, Es}, Env) ->
    [print(eval_expr(E, Env)) || E <- Es],
    Env;
eval_line({assign, Assigns}, Env) ->
    lists:foldl(
        fun({E, {var, V}}, CEnv) ->
            dict:store(V, eval_expr(E, CEnv), CEnv)
        end,
        Env,
        Assigns);
eval_line({do, {E, Stmt}}, Env) ->
    lists:foldl(
        fun(_, CEnv) ->
            eval_line(Stmt, CEnv)
        end,
        Env,
        lists:seq(1, eval_expr(E, Env)));
eval_line(_, Env) ->
    Env.

eval_expr({number, N}, _) ->
    N;
eval_expr({var, V}, Env) ->
    dict:fetch(V, Env);
eval_expr({add, E1, E2}, Env) ->
    eval_op(fun(A, B) -> A + B end, add, E1, E2, Env);
eval_expr({mul, E1, E2}, Env) ->
    eval_op(fun(A, B) -> A * B end, mul, E1, E2, Env);
eval_expr({sub, E1, E2}, Env) ->
    eval_op(fun(A, B) -> A - B end, sub, E1, E2, Env);
eval_expr({coc, E1, E2}, Env) ->
    eval_op(
        fun
            (A, 1) ->
                A;
            (A, B) -> 
                case (A rem B) of 
                    0 ->
                        A div B;
                    _ ->
                        GCD = gcd(A, B),
                        {coc, A div GCD , B div GCD}
                end
        end, 
        coc, E1, E2, Env);
eval_expr({call, F, As}, Env) ->
    EAs = [eval_expr(A, Env) || A <- As], 
    {fun_decl, Arity, Coeffs} = dict:fetch(F, Env),
    LAs = length(As),
    case Arity - LAs of 
        0 -> 
            [LastC | CoeffsR] = lists:reverse(Coeffs),
            eval_call(lists:reverse(CoeffsR), EAs, Env, LastC);
        Pend -> 
            EvalCoeff = 
                lists:sublist(Coeffs, 1, LAs),
            PendCoeffs = 
                lists:sublist(Coeffs, LAs + 1, Arity + 1),
            [LastC | PendCoeffsR] = lists:reverse(PendCoeffs),
            NLast = 
                eval_call(EvalCoeff, EAs, Env, LastC),
            {fun_decl, Pend, lists:reverse([NLast | PendCoeffsR])}
    end;
eval_expr(Other, _) ->
    Other.

eval_call(Coeffs, EAs, Env, Last) ->
    MCoefs = 
        lists:zipwith(
            fun(A, B) ->
                {mul, A, B}
            end,
            Coeffs,
            EAs),
    ToEval = 
        lists:foldl(
            fun(X, Acc) ->
                {add, X, Acc}
            end,
            Last,
            MCoefs),
    eval_expr(ToEval, Env).


eval_op(Fun, Op, E1, E2, Env) ->
    EE1 = eval_expr(E1, Env),
    EE2 = eval_expr(E2, Env),
    case (is_number(EE1) and is_number(EE2)) of 
        true ->
            Fun(EE1, EE2);
        false ->
            case {EE1, EE2} of 
                {{coc, _, _}, {coc, _, _}} ->
                    eval_cocs(Op, EE1, EE2, Env);
                {{coc, _, _}, _}  ->
                    eval_cocs(Op, EE1, {coc, EE2, 1}, Env);
                {_, {coc, _, _}}  ->
                    eval_cocs(Op, {coc, EE1, 1}, EE2, Env);
                {_,_} ->
                    {Op, EE1, EE2}
            end
    end.

eval_cocs(Op, {coc, CA1, CA2}, {coc, CB1, CB2}, Env) ->
    FunAddSub = 
        fun() ->
            {coc, 
                {Op, 
                    {mul, CA1 , CB2}, 
                    {mul, CB1 , CA2}
                }, 
                {mul, CA2 , CB2}
            }
        end,
    FunMulCoc = 
        fun(NCB1, NCB2) ->
            {coc,
                {mul, CA1, NCB1}, 
                {mul, CA2, NCB2}
            }
        end,
    case Op of 
        add ->
            eval_expr(FunAddSub(), Env);
        sub ->
            eval_expr(FunAddSub(), Env);
        mul ->
            eval_expr(FunMulCoc(CB1, CB2), Env);
        coc ->
            eval_expr(FunMulCoc(CB2, CB1), Env)
    end.

% Printing
%------------------------------

print(E) ->
    io:format("~s\n", [format(E)]).

format({coc, A, B}) when B > 0 ->
    lists:flatten(io_lib:format("~p/~p", [A, B]));
format({coc, A, B}) when B < 0 ->
    lists:flatten(io_lib:format("~p/~p", [-A, -B]));
format({fun_decl, _, Coeffs}) ->
    string:join([format(C) || C <- Coeffs], ", ");
format(Other) ->
    lists:flatten(io_lib:format("~p", [Other])).

% Support
%------------------------------

gcd(A, 0) -> 
    A;
gcd(A, B) -> 
    gcd(B, A rem B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(_) -> 
    % io:format("~p\n", [D]).
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    Binary = read(),
    Res = binary:split(Binary, [<<"\n">>], [global]),
    [binary_to_list(R) || R <- Res, R /= <<>>].

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

