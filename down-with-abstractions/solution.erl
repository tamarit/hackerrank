% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/down-with-abstractions

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
            {PT, []} = 
                parse_expression(
                    prepare_parsing(T)
                ),
            PT
        end, 
        Ts),
    % io:format("~p\n", [Parsed]),
    TParsed = 
        lists:map(fun transform/1, Parsed),
    % io:format("~p\n", [TParsed]),
    TParsed.
    % lists:foldl(
    %     fun eval_line/2,
    %     dict:new(),
    %     Parsed).

% Parsing
%------------------------------

prepare_parsing(Line) ->
    string:strip(string:to_lower(Line)).


% EXP = VAR
% EXP = '(' EXP ' ' EXP ')'
% EXP = '(' '\' VARLIST '.' EXP ')'
% VARLIST = VAR VARLIST0
% VARLIST0 = eps
% VARLIST0 = ' ' VAR VARLIST0

parse_expression([$( | Rest]) ->
    case string:strip(Rest) of 
        % Is a Lambda Expression
        [$\\ | RestLambda] ->
            {VarList0, [$. | Body0]} = 
                lists:splitwith(
                    fun(A) -> A /= $. end,
                    RestLambda),
            VarList = 
                [   {var, string:strip(T)}
                 || T <- string:tokens(VarList0, " "), not(white_spaces(T))],
            {Body, AfterExp} = 
                find_end_bracket($(, $), Body0, 0, []),
            {PBody, []} = 
                parse_expression(string:strip(Body)),
            LExpr = 
                build_lambda(VarList, PBody),
            {LExpr, AfterExp};
        % Is a Call ->
        RestStrp ->
            {E1, RestE1} = parse_expression(RestStrp),
            {E2Str, AfterExp} =
                find_end_bracket($(, $), string:strip(RestE1), 0, []),
            {E2, []} = parse_expression(E2Str),
            {{call, E1, E2}, AfterExp}
    end;
% Is a var
parse_expression(Other) ->
    {VarStr, AfterExp} = 
        lists:splitwith(
            fun(A) -> (A >= $a andalso A =< $z) orelse (A >= $0 andalso A =< $9) orelse (A == $_) end,
            Other),
    {{var, VarStr}, AfterExp}.

build_lambda([V], PBody) ->
    {lambda, V, PBody};
build_lambda([V | Vs], PBody) ->
    {lambda, V, build_lambda(Vs, PBody)}.

white_spaces([$ | L]) ->
    white_spaces(L);
white_spaces([_| _]) ->
    false;
white_spaces([]) ->
    true.

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


% Transformation
%------------------------------

transform(E) ->
    fix_point(fun t/1, E, 0).

% T[x] => x
t(X = {var, _}) ->
    % io:format("~p\n", [X]),
    X;
% T[(E₁ E₂)] => (T[E₁] T[E₂])
t({call, E1, E2}) ->
    % io:format("(~p ~p)\n", [E1, E2]),
    {call, t(E1), t(E2)};
% T[λx.x] => I
t({lambda, X, X}) ->
    % io:format("\\ ~p  . ~p\n", [X, X]),
    i;
t(Lmd = {lambda, X, E}) ->
    % io:format("\\ ~p  . ~p\n", [X, E]),
    case E of 
        {call, E1, X} ->
            case not(is_free(X, E1)) of 
% Eta-reduction
% T[λx.(E x)] = T[E] (if x is not free in E)
                true ->
                    t(E1);
                false -> 
                    t_lambda(Lmd)
            end;
        _ -> 
            t_lambda(Lmd)
    end;
t(Other) ->
    Other.
% t({k, E}) ->
%     {k, t(E)};
% t({Op, E1, E2}) ->
%     {Op, t(E1), t(E2)}.

t_lambda({lambda, X, E}) ->
    case not(is_free(X, E)) of 
% T[λx.E] => (K T[E]) (if x is not free in E)
        true ->
            {call, k, t(E)};
        false ->
            case E of 
% T[λx.λy.E] => T[λx.T[λy.E]] (if x is free in E)
                {lambda, Y, E2} ->
                    t({lambda, X, t({lambda, Y, E2})});
                {call, E1, E2} ->
                    case {is_free(X, E1), is_free(X, E2)} of
% T[λx.(E₁ E₂)] => (S T[λx.E₁] T[λx.E₂]) (if x is free in both E₁ and E₂)
                        {true, true} ->
                            {call, {call, s, t({lambda, X, E1})}, t({lambda, X, E2})};
% T[λx.(E₁ E₂)] => (C T[λx.E₁] T[E₂]) (if x is free in E₁ but not E₂)
                        {true, false} ->
                            {call, {call, c, t({lambda, X, E1})}, t(E2)};
% T[λx.(E₁ E₂)] => (B T[E₁] T[λx.E₂]) (if x is free in E₂ but not E₁)
                        {false, true} ->
                            {call, {call, b, t(E1)}, t({lambda, X, E2})}
                    end;
                Other ->
                    Other
                % {k, EK} ->
                %     {k, t(EK)};
                % {Op, E1, E2} ->
                %     {Op, t(E1), t(E2)}
            end 
    end.

is_free(V, E) ->
    lists:member(V, free_vars(E, [])).

free_vars(V = {var, _}, Env) -> 
    case lists:member(V, Env) of 
        true ->
            [];
        false ->
            [V]
    end;
free_vars({call, E1, E2}, Env) -> 
    free_vars(E1, Env) ++ free_vars(E2, Env);
free_vars({lambda, V, E}, Env) ->
    free_vars(E, [V | Env]);
free_vars(_, _) ->
    [].
% free_vars({k, E}, Env) ->
%     free_vars(E, Env);
% free_vars({_, E1, E2}, Env) -> 
%     free_vars(E1, Env) ++ free_vars(E2, Env).

% vars(V = {var, _}) -> 
%     [V];
% vars({call, E1, E2}) -> 
%     vars(E1) ++ vars(E2);
% vars({lambda, V, E}) ->
%     [V | vars(E)];
% vars(_) ->
%     [].


% Support
%------------------------------

% fix_point(_, Current, Current) ->
%     Current;
% fix_point(F, Current, _) ->
%     fix_point(F, F(Current), Current).

fix_point(F, Current, _) ->
    F(Current).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(Ds) -> 
    lists:foreach(
        fun(D) ->
            io:format("~s\n", [cl_string(D, false)])
        end,
        Ds).

cl_string(i, _) ->
    "I";
cl_string(k, _) ->
    "K";
cl_string(s, _) ->
    "S";
cl_string(b, _) ->
    "B";
cl_string(c, _) ->
    "C";
cl_string({call, E1 = {call, _, _}, E2 = {call, _, _}}, false) ->
   % [$( | cl_string(E1, false)] ++ ")" ++ [$( | cl_string(E2, false)] ++ ")";
   cl_string(E1, false) ++ [$( | cl_string(E2, false)] ++ ")";
% cl_string({call, E1 = {call, _, _}, E2}, false) ->
%    [$( | cl_string(E1, false)] ++ ")" ++ cl_string(E2, false);
cl_string({call, E1, E2 = {call, _, _}}, false) ->
   cl_string(E1, false) ++ [$( | cl_string(E2, false)] ++ ")";
cl_string({call, E1, E2}, _) ->
   cl_string(E1, false) ++ cl_string(E2, false).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    Binary = read(),
    Res = binary:split(Binary, [<<"\n">>], [global]),
    tl([binary_to_list(R) || R <- Res, R /= <<>>]).

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

