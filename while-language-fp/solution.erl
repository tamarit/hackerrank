% https://www.hackerrank.com/challenges/while-language-fp

-module(solution).
-export([main/0]).

main() ->
    Data = read_data(),
    Res = calculate(Data),
    output_data(Res),
    ok.

calculate(Program) ->
    {ok, Stmts} = 
        from_string(Program),
    eval_stmts(Stmts, dict:new()).

eval_stmts(Stmts, Dict) ->
    lists:foldl(
        fun eval_stmt/2,
        Dict,
        Stmts).    

eval_stmt({op, '=', {var, _, V}, AExp}, Dict) ->
    dict:store(V, eval_aexp(AExp, Dict), Dict);
eval_stmt({'if', B, S1, S2}, Dict) ->
    case eval_bexp(B, Dict) of 
        true ->
            eval_stmts(S1, Dict);
        false ->
            eval_stmts(S2, Dict)
    end;
eval_stmt({'while', B, S}, Dict) ->
    case eval_bexp(B, Dict) of 
        true ->
            NDict = 
                eval_stmts(S, Dict),
            eval_stmt({'while', B, S}, NDict);
        false ->
            Dict
    end.

eval_aexp({var, _, V}, Dict) ->
    dict:fetch(V, Dict);
eval_aexp({int, _, I}, _) ->
    I;
eval_aexp({op, Op, AExp1, AExp2}, Dict) ->
    case Op of 
        '+' ->
            eval_aexp(AExp1, Dict) + eval_aexp(AExp2, Dict);
        '-' ->
            eval_aexp(AExp1, Dict) - eval_aexp(AExp2, Dict);
        '*' ->
            eval_aexp(AExp1, Dict) * eval_aexp(AExp2, Dict);
        '/' ->
            eval_aexp(AExp1, Dict) div eval_aexp(AExp2, Dict)
    end.

eval_bexp({op, Op, Exp1, Exp2}, Dict) ->
    case Op of 
        'and' ->
            eval_bexp(Exp1, Dict) andalso eval_bexp(Exp2, Dict);
        'or' ->
            eval_bexp(Exp1, Dict) orelse eval_bexp(Exp2, Dict);
        '<' ->
            eval_aexp(Exp1, Dict) < eval_aexp(Exp2, Dict);
        '>' ->
            eval_aexp(Exp1, Dict) > eval_aexp(Exp2, Dict)
    end;
eval_bexp(BExp, _) ->
    BExp.

output_data(Dict) -> 
    LDict = dict:to_list(Dict),
    [io:format("~p ~p\n", [K, V]) || {K, V} <- lists:sort(LDict)].

read_data() ->
    Binary = read(),
    Res = 
        binary:split(Binary, [<<"\n">>], [global]),
    string:join([binary_to_list(R) || R <- Res], "\n").

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

from_string(S) ->
    {ok,Ts, _} = string(S),
    parse(Ts).

string(String) -> string(String, 1).

string(String, Line) -> string(String, Line, String, []).

string([], L, [], Ts) ->                     % No partial tokens!
    {ok,yyrev(Ts),L};
string(Ics0, L0, Tcs, Ts) ->
    case yystate(yystate(), Ics0, L0, 0, reject, 0) of
        {A,Alen,Ics1,L1} ->                  % Accepting end state
            string_cont(Ics1, L1, yyaction(A, Alen, Tcs, L0), Ts);
        {A,Alen,Ics1,L1,_S1} ->              % Accepting transistion state
            string_cont(Ics1, L1, yyaction(A, Alen, Tcs, L0), Ts);
        {reject,_Alen,Tlen,_Ics1,L1,_S1} ->  % After a non-accepting state
            {error,{L0,?MODULE,{illegal,yypre(Tcs, Tlen+1)}},L1};
        {A,Alen,Tlen,_Ics1,L1,_S1} ->
            Tcs1 = yysuf(Tcs, Alen),
            L2 = adjust_line(Tlen, Alen, Tcs1, L1),
            string_cont(Tcs1, L2, yyaction(A, Alen, Tcs, L0), Ts)
    end.

string_cont(Rest, Line, {token,T}, Ts) ->
    string(Rest, Line, Rest, [T|Ts]);
string_cont(Rest, Line, {token,T,Push}, Ts) ->
    NewRest = Push ++ Rest,
    string(NewRest, Line, NewRest, [T|Ts]);
string_cont(Rest, Line, {end_token,T}, Ts) ->
    string(Rest, Line, Rest, [T|Ts]);
string_cont(Rest, Line, {end_token,T,Push}, Ts) ->
    NewRest = Push ++ Rest,
    string(NewRest, Line, NewRest, [T|Ts]);
string_cont(Rest, Line, skip_token, Ts) ->
    string(Rest, Line, Rest, Ts);
string_cont(Rest, Line, {skip_token,Push}, Ts) ->
    NewRest = Push ++ Rest,
    string(NewRest, Line, NewRest, Ts);
string_cont(_Rest, Line, {error,S}, _Ts) ->
    {error,{Line,?MODULE,{user,S}},Line}.

token(Cont, Chars) -> token(Cont, Chars, 1).

token([], Chars, Line) ->
    token(yystate(), Chars, Line, Chars, 0, Line, reject, 0);
token({token,State,Line,Tcs,Tlen,Tline,Action,Alen}, Chars, _) ->
    token(State, Chars, Line, Tcs ++ Chars, Tlen, Tline, Action, Alen).

token(S0, Ics0, L0, Tcs, Tlen0, Tline, A0, Alen0) ->
    case yystate(S0, Ics0, L0, Tlen0, A0, Alen0) of
        %% Accepting end state, we have a token.
        {A1,Alen1,Ics1,L1} ->
            token_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, Tline));
        %% Accepting transition state, can take more chars.
        {A1,Alen1,[],L1,S1} ->                  % Need more chars to check
            {more,{token,S1,L1,Tcs,Alen1,Tline,A1,Alen1}};
        {A1,Alen1,Ics1,L1,_S1} ->               % Take what we got
            token_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, Tline));
        %% After a non-accepting state, maybe reach accept state later.
        {A1,Alen1,Tlen1,[],L1,S1} ->            % Need more chars to check
            {more,{token,S1,L1,Tcs,Tlen1,Tline,A1,Alen1}};
        {reject,_Alen1,Tlen1,eof,L1,_S1} ->     % No token match
            %% Check for partial token which is error.
            Ret = if Tlen1 > 0 -> {error,{Tline,?MODULE,
                                          %% Skip eof tail in Tcs.
                                          {illegal,yypre(Tcs, Tlen1)}},L1};
                     true -> {eof,L1}
                  end,
            {done,Ret,eof};
        {reject,_Alen1,Tlen1,Ics1,L1,_S1} ->    % No token match
            Error = {Tline,?MODULE,{illegal,yypre(Tcs, Tlen1+1)}},
            {done,{error,Error,L1},Ics1};
        {A1,Alen1,Tlen1,_Ics1,L1,_S1} ->       % Use last accept match
            Tcs1 = yysuf(Tcs, Alen1),
            L2 = adjust_line(Tlen1, Alen1, Tcs1, L1),
            token_cont(Tcs1, L2, yyaction(A1, Alen1, Tcs, Tline))
    end.

token_cont(Rest, Line, {token,T}) ->
    {done,{ok,T,Line},Rest};
token_cont(Rest, Line, {token,T,Push}) ->
    NewRest = Push ++ Rest,
    {done,{ok,T,Line},NewRest};
token_cont(Rest, Line, {end_token,T}) ->
    {done,{ok,T,Line},Rest};
token_cont(Rest, Line, {end_token,T,Push}) ->
    NewRest = Push ++ Rest,
    {done,{ok,T,Line},NewRest};
token_cont(Rest, Line, skip_token) ->
    token(yystate(), Rest, Line, Rest, 0, Line, reject, 0);
token_cont(Rest, Line, {skip_token,Push}) ->
    NewRest = Push ++ Rest,
    token(yystate(), NewRest, Line, NewRest, 0, Line, reject, 0);
token_cont(Rest, Line, {error,S}) ->
    {done,{error,{Line,?MODULE,{user,S}},Line},Rest}.

tokens(Cont, Chars) -> tokens(Cont, Chars, 1).

tokens([], Chars, Line) ->
    tokens(yystate(), Chars, Line, Chars, 0, Line, [], reject, 0);
tokens({tokens,State,Line,Tcs,Tlen,Tline,Ts,Action,Alen}, Chars, _) ->
    tokens(State, Chars, Line, Tcs ++ Chars, Tlen, Tline, Ts, Action, Alen);
tokens({skip_tokens,State,Line,Tcs,Tlen,Tline,Error,Action,Alen}, Chars, _) ->
    skip_tokens(State, Chars, Line, Tcs ++ Chars, Tlen, Tline, Error, Action, Alen).

tokens(S0, Ics0, L0, Tcs, Tlen0, Tline, Ts, A0, Alen0) ->
    case yystate(S0, Ics0, L0, Tlen0, A0, Alen0) of
        %% Accepting end state, we have a token.
        {A1,Alen1,Ics1,L1} ->
            tokens_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, Tline), Ts);
        %% Accepting transition state, can take more chars.
        {A1,Alen1,[],L1,S1} ->                  % Need more chars to check
            {more,{tokens,S1,L1,Tcs,Alen1,Tline,Ts,A1,Alen1}};
        {A1,Alen1,Ics1,L1,_S1} ->               % Take what we got
            tokens_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, Tline), Ts);
        %% After a non-accepting state, maybe reach accept state later.
        {A1,Alen1,Tlen1,[],L1,S1} ->            % Need more chars to check
            {more,{tokens,S1,L1,Tcs,Tlen1,Tline,Ts,A1,Alen1}};
        {reject,_Alen1,Tlen1,eof,L1,_S1} ->     % No token match
            %% Check for partial token which is error, no need to skip here.
            Ret = if Tlen1 > 0 -> {error,{Tline,?MODULE,
                                          %% Skip eof tail in Tcs.
                                          {illegal,yypre(Tcs, Tlen1)}},L1};
                     Ts == [] -> {eof,L1};
                     true -> {ok,yyrev(Ts),L1}
                  end,
            {done,Ret,eof};
        {reject,_Alen1,Tlen1,_Ics1,L1,_S1} ->
            %% Skip rest of tokens.
            Error = {L1,?MODULE,{illegal,yypre(Tcs, Tlen1+1)}},
            skip_tokens(yysuf(Tcs, Tlen1+1), L1, Error);
        {A1,Alen1,Tlen1,_Ics1,L1,_S1} ->
            Token = yyaction(A1, Alen1, Tcs, Tline),
            Tcs1 = yysuf(Tcs, Alen1),
            L2 = adjust_line(Tlen1, Alen1, Tcs1, L1),
            tokens_cont(Tcs1, L2, Token, Ts)
    end.

tokens_cont(Rest, Line, {token,T}, Ts) ->
    tokens(yystate(), Rest, Line, Rest, 0, Line, [T|Ts], reject, 0);
tokens_cont(Rest, Line, {token,T,Push}, Ts) ->
    NewRest = Push ++ Rest,
    tokens(yystate(), NewRest, Line, NewRest, 0, Line, [T|Ts], reject, 0);
tokens_cont(Rest, Line, {end_token,T}, Ts) ->
    {done,{ok,yyrev(Ts, [T]),Line},Rest};
tokens_cont(Rest, Line, {end_token,T,Push}, Ts) ->
    NewRest = Push ++ Rest,
    {done,{ok,yyrev(Ts, [T]),Line},NewRest};
tokens_cont(Rest, Line, skip_token, Ts) ->
    tokens(yystate(), Rest, Line, Rest, 0, Line, Ts, reject, 0);
tokens_cont(Rest, Line, {skip_token,Push}, Ts) ->
    NewRest = Push ++ Rest,
    tokens(yystate(), NewRest, Line, NewRest, 0, Line, Ts, reject, 0);
tokens_cont(Rest, Line, {error,S}, _Ts) ->
    skip_tokens(Rest, Line, {Line,?MODULE,{user,S}}).

skip_tokens(Ics, Line, Error) ->
    skip_tokens(yystate(), Ics, Line, Ics, 0, Line, Error, reject, 0).

skip_tokens(S0, Ics0, L0, Tcs, Tlen0, Tline, Error, A0, Alen0) ->
    case yystate(S0, Ics0, L0, Tlen0, A0, Alen0) of
        {A1,Alen1,Ics1,L1} ->                  % Accepting end state
            skip_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, Tline), Error);
        {A1,Alen1,[],L1,S1} ->                 % After an accepting state
            {more,{skip_tokens,S1,L1,Tcs,Alen1,Tline,Error,A1,Alen1}};
        {A1,Alen1,Ics1,L1,_S1} ->
            skip_cont(Ics1, L1, yyaction(A1, Alen1, Tcs, Tline), Error);
        {A1,Alen1,Tlen1,[],L1,S1} ->           % After a non-accepting state
            {more,{skip_tokens,S1,L1,Tcs,Tlen1,Tline,Error,A1,Alen1}};
        {reject,_Alen1,_Tlen1,eof,L1,_S1} ->
            {done,{error,Error,L1},eof};
        {reject,_Alen1,Tlen1,_Ics1,L1,_S1} ->
            skip_tokens(yysuf(Tcs, Tlen1+1), L1, Error);
        {A1,Alen1,Tlen1,_Ics1,L1,_S1} ->
            Token = yyaction(A1, Alen1, Tcs, Tline),
            Tcs1 = yysuf(Tcs, Alen1),
            L2 = adjust_line(Tlen1, Alen1, Tcs1, L1),
            skip_cont(Tcs1, L2, Token, Error)
    end.

skip_cont(Rest, Line, {token,_T}, Error) ->
    skip_tokens(yystate(), Rest, Line, Rest, 0, Line, Error, reject, 0);
skip_cont(Rest, Line, {token,_T,Push}, Error) ->
    NewRest = Push ++ Rest,
    skip_tokens(yystate(), NewRest, Line, NewRest, 0, Line, Error, reject, 0);
skip_cont(Rest, Line, {end_token,_T}, Error) ->
    {done,{error,Error,Line},Rest};
skip_cont(Rest, Line, {end_token,_T,Push}, Error) ->
    NewRest = Push ++ Rest,
    {done,{error,Error,Line},NewRest};
skip_cont(Rest, Line, skip_token, Error) ->
    skip_tokens(yystate(), Rest, Line, Rest, 0, Line, Error, reject, 0);
skip_cont(Rest, Line, {skip_token,Push}, Error) ->
    NewRest = Push ++ Rest,
    skip_tokens(yystate(), NewRest, Line, NewRest, 0, Line, Error, reject, 0);
skip_cont(Rest, Line, {error,_S}, Error) ->
    skip_tokens(yystate(), Rest, Line, Rest, 0, Line, Error, reject, 0).

yyrev(List) -> lists:reverse(List).
yyrev(List, Tail) -> lists:reverse(List, Tail).
yypre(List, N) -> lists:sublist(List, N).
yysuf(List, N) -> lists:nthtail(N, List).

adjust_line(N, N, _Cs, L) -> L;
adjust_line(T, A, [$\n|Cs], L) ->
    adjust_line(T-1, A, Cs, L-1);
adjust_line(T, A, [_|Cs], L) ->
    adjust_line(T-1, A, Cs, L).

yystate() -> 9.

yystate(21, [101|Ics], Line, Tlen, _, _) ->
    yystate(0, Ics, Line, Tlen+1, 2, Tlen);
yystate(21, [C|Ics], Line, Tlen, _, _) when C >= 97, C =< 100 ->
    yystate(18, Ics, Line, Tlen+1, 2, Tlen);
yystate(21, [C|Ics], Line, Tlen, _, _) when C >= 102, C =< 122 ->
    yystate(18, Ics, Line, Tlen+1, 2, Tlen);
yystate(21, Ics, Line, Tlen, _, _) ->
    {2,Tlen,Ics,Line,21};
yystate(20, [105|Ics], Line, Tlen, _, _) ->
    yystate(7, Ics, Line, Tlen+1, 2, Tlen);
yystate(20, [C|Ics], Line, Tlen, _, _) when C >= 97, C =< 104 ->
    yystate(18, Ics, Line, Tlen+1, 2, Tlen);
yystate(20, [C|Ics], Line, Tlen, _, _) when C >= 106, C =< 122 ->
    yystate(18, Ics, Line, Tlen+1, 2, Tlen);
yystate(20, Ics, Line, Tlen, _, _) ->
    {2,Tlen,Ics,Line,20};
yystate(19, Ics, Line, Tlen, _, _) ->
    {1,Tlen,Ics,Line};
yystate(18, [C|Ics], Line, Tlen, _, _) when C >= 97, C =< 122 ->
    yystate(18, Ics, Line, Tlen+1, 2, Tlen);
yystate(18, Ics, Line, Tlen, _, _) ->
    {2,Tlen,Ics,Line,18};
yystate(17, [115|Ics], Line, Tlen, _, _) ->
    yystate(4, Ics, Line, Tlen+1, 2, Tlen);
yystate(17, [C|Ics], Line, Tlen, _, _) when C >= 97, C =< 114 ->
    yystate(18, Ics, Line, Tlen+1, 2, Tlen);
yystate(17, [C|Ics], Line, Tlen, _, _) when C >= 116, C =< 122 ->
    yystate(18, Ics, Line, Tlen+1, 2, Tlen);
yystate(17, Ics, Line, Tlen, _, _) ->
    {2,Tlen,Ics,Line,17};
yystate(16, [114|Ics], Line, Tlen, _, _) ->
    yystate(8, Ics, Line, Tlen+1, 2, Tlen);
yystate(16, [C|Ics], Line, Tlen, _, _) when C >= 97, C =< 113 ->
    yystate(18, Ics, Line, Tlen+1, 2, Tlen);
yystate(16, [C|Ics], Line, Tlen, _, _) when C >= 115, C =< 122 ->
    yystate(18, Ics, Line, Tlen+1, 2, Tlen);
yystate(16, Ics, Line, Tlen, _, _) ->
    {2,Tlen,Ics,Line,16};
yystate(15, [100|Ics], Line, Tlen, _, _) ->
    yystate(8, Ics, Line, Tlen+1, 2, Tlen);
yystate(15, [C|Ics], Line, Tlen, _, _) when C >= 97, C =< 99 ->
    yystate(18, Ics, Line, Tlen+1, 2, Tlen);
yystate(15, [C|Ics], Line, Tlen, _, _) when C >= 101, C =< 122 ->
    yystate(18, Ics, Line, Tlen+1, 2, Tlen);
yystate(15, Ics, Line, Tlen, _, _) ->
    {2,Tlen,Ics,Line,15};
yystate(14, [108|Ics], Line, Tlen, _, _) ->
    yystate(17, Ics, Line, Tlen+1, 2, Tlen);
yystate(14, [C|Ics], Line, Tlen, _, _) when C >= 97, C =< 107 ->
    yystate(18, Ics, Line, Tlen+1, 2, Tlen);
yystate(14, [C|Ics], Line, Tlen, _, _) when C >= 109, C =< 122 ->
    yystate(18, Ics, Line, Tlen+1, 2, Tlen);
yystate(14, Ics, Line, Tlen, _, _) ->
    {2,Tlen,Ics,Line,14};
yystate(13, [110|Ics], Line, Tlen, _, _) ->
    yystate(15, Ics, Line, Tlen+1, 2, Tlen);
yystate(13, [C|Ics], Line, Tlen, _, _) when C >= 97, C =< 109 ->
    yystate(18, Ics, Line, Tlen+1, 2, Tlen);
yystate(13, [C|Ics], Line, Tlen, _, _) when C >= 111, C =< 122 ->
    yystate(18, Ics, Line, Tlen+1, 2, Tlen);
yystate(13, Ics, Line, Tlen, _, _) ->
    {2,Tlen,Ics,Line,13};
yystate(12, [C|Ics], Line, Tlen, _, _) when C >= 48, C =< 57 ->
    yystate(12, Ics, Line, Tlen+1, 3, Tlen);
yystate(12, Ics, Line, Tlen, _, _) ->
    {3,Tlen,Ics,Line,12};
yystate(11, [102|Ics], Line, Tlen, _, _) ->
    yystate(8, Ics, Line, Tlen+1, 2, Tlen);
yystate(11, [C|Ics], Line, Tlen, _, _) when C >= 97, C =< 101 ->
    yystate(18, Ics, Line, Tlen+1, 2, Tlen);
yystate(11, [C|Ics], Line, Tlen, _, _) when C >= 103, C =< 122 ->
    yystate(18, Ics, Line, Tlen+1, 2, Tlen);
yystate(11, Ics, Line, Tlen, _, _) ->
    {2,Tlen,Ics,Line,11};
yystate(10, [117|Ics], Line, Tlen, _, _) ->
    yystate(4, Ics, Line, Tlen+1, 2, Tlen);
yystate(10, [C|Ics], Line, Tlen, _, _) when C >= 97, C =< 116 ->
    yystate(18, Ics, Line, Tlen+1, 2, Tlen);
yystate(10, [C|Ics], Line, Tlen, _, _) when C >= 118, C =< 122 ->
    yystate(18, Ics, Line, Tlen+1, 2, Tlen);
yystate(10, Ics, Line, Tlen, _, _) ->
    {2,Tlen,Ics,Line,10};
yystate(9, [125|Ics], Line, Tlen, Action, Alen) ->
    yystate(19, Ics, Line, Tlen+1, Action, Alen);
yystate(9, [123|Ics], Line, Tlen, Action, Alen) ->
    yystate(19, Ics, Line, Tlen+1, Action, Alen);
yystate(9, [119|Ics], Line, Tlen, Action, Alen) ->
    yystate(3, Ics, Line, Tlen+1, Action, Alen);
yystate(9, [117|Ics], Line, Tlen, Action, Alen) ->
    yystate(18, Ics, Line, Tlen+1, Action, Alen);
yystate(9, [118|Ics], Line, Tlen, Action, Alen) ->
    yystate(18, Ics, Line, Tlen+1, Action, Alen);
yystate(9, [116|Ics], Line, Tlen, Action, Alen) ->
    yystate(2, Ics, Line, Tlen+1, Action, Alen);
yystate(9, [111|Ics], Line, Tlen, Action, Alen) ->
    yystate(16, Ics, Line, Tlen+1, Action, Alen);
yystate(9, [105|Ics], Line, Tlen, Action, Alen) ->
    yystate(11, Ics, Line, Tlen+1, Action, Alen);
yystate(9, [103|Ics], Line, Tlen, Action, Alen) ->
    yystate(18, Ics, Line, Tlen+1, Action, Alen);
yystate(9, [104|Ics], Line, Tlen, Action, Alen) ->
    yystate(18, Ics, Line, Tlen+1, Action, Alen);
yystate(9, [102|Ics], Line, Tlen, Action, Alen) ->
    yystate(1, Ics, Line, Tlen+1, Action, Alen);
yystate(9, [101|Ics], Line, Tlen, Action, Alen) ->
    yystate(14, Ics, Line, Tlen+1, Action, Alen);
yystate(9, [100|Ics], Line, Tlen, Action, Alen) ->
    yystate(5, Ics, Line, Tlen+1, Action, Alen);
yystate(9, [98|Ics], Line, Tlen, Action, Alen) ->
    yystate(18, Ics, Line, Tlen+1, Action, Alen);
yystate(9, [99|Ics], Line, Tlen, Action, Alen) ->
    yystate(18, Ics, Line, Tlen+1, Action, Alen);
yystate(9, [97|Ics], Line, Tlen, Action, Alen) ->
    yystate(13, Ics, Line, Tlen+1, Action, Alen);
yystate(9, [32|Ics], Line, Tlen, Action, Alen) ->
    yystate(6, Ics, Line, Tlen+1, Action, Alen);
yystate(9, [13|Ics], Line, Tlen, Action, Alen) ->
    yystate(6, Ics, Line, Tlen+1, Action, Alen);
yystate(9, [9|Ics], Line, Tlen, Action, Alen) ->
    yystate(6, Ics, Line, Tlen+1, Action, Alen);
yystate(9, [10|Ics], Line, Tlen, Action, Alen) ->
    yystate(6, Ics, Line+1, Tlen+1, Action, Alen);
yystate(9, [C|Ics], Line, Tlen, Action, Alen) when C >= 40, C =< 47 ->
    yystate(19, Ics, Line, Tlen+1, Action, Alen);
yystate(9, [C|Ics], Line, Tlen, Action, Alen) when C >= 48, C =< 57 ->
    yystate(12, Ics, Line, Tlen+1, Action, Alen);
yystate(9, [C|Ics], Line, Tlen, Action, Alen) when C >= 58, C =< 62 ->
    yystate(19, Ics, Line, Tlen+1, Action, Alen);
yystate(9, [C|Ics], Line, Tlen, Action, Alen) when C >= 106, C =< 110 ->
    yystate(18, Ics, Line, Tlen+1, Action, Alen);
yystate(9, [C|Ics], Line, Tlen, Action, Alen) when C >= 112, C =< 115 ->
    yystate(18, Ics, Line, Tlen+1, Action, Alen);
yystate(9, [C|Ics], Line, Tlen, Action, Alen) when C >= 120, C =< 122 ->
    yystate(18, Ics, Line, Tlen+1, Action, Alen);
yystate(9, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,9};
yystate(8, [C|Ics], Line, Tlen, _, _) when C >= 97, C =< 122 ->
    yystate(18, Ics, Line, Tlen+1, 0, Tlen);
yystate(8, Ics, Line, Tlen, _, _) ->
    {0,Tlen,Ics,Line,8};
yystate(7, [108|Ics], Line, Tlen, _, _) ->
    yystate(4, Ics, Line, Tlen+1, 2, Tlen);
yystate(7, [C|Ics], Line, Tlen, _, _) when C >= 97, C =< 107 ->
    yystate(18, Ics, Line, Tlen+1, 2, Tlen);
yystate(7, [C|Ics], Line, Tlen, _, _) when C >= 109, C =< 122 ->
    yystate(18, Ics, Line, Tlen+1, 2, Tlen);
yystate(7, Ics, Line, Tlen, _, _) ->
    {2,Tlen,Ics,Line,7};
yystate(6, [32|Ics], Line, Tlen, _, _) ->
    yystate(6, Ics, Line, Tlen+1, 4, Tlen);
yystate(6, [13|Ics], Line, Tlen, _, _) ->
    yystate(6, Ics, Line, Tlen+1, 4, Tlen);
yystate(6, [9|Ics], Line, Tlen, _, _) ->
    yystate(6, Ics, Line, Tlen+1, 4, Tlen);
yystate(6, [10|Ics], Line, Tlen, _, _) ->
    yystate(6, Ics, Line+1, Tlen+1, 4, Tlen);
yystate(6, Ics, Line, Tlen, _, _) ->
    {4,Tlen,Ics,Line,6};
yystate(5, [111|Ics], Line, Tlen, _, _) ->
    yystate(8, Ics, Line, Tlen+1, 2, Tlen);
yystate(5, [C|Ics], Line, Tlen, _, _) when C >= 97, C =< 110 ->
    yystate(18, Ics, Line, Tlen+1, 2, Tlen);
yystate(5, [C|Ics], Line, Tlen, _, _) when C >= 112, C =< 122 ->
    yystate(18, Ics, Line, Tlen+1, 2, Tlen);
yystate(5, Ics, Line, Tlen, _, _) ->
    {2,Tlen,Ics,Line,5};
yystate(4, [101|Ics], Line, Tlen, _, _) ->
    yystate(8, Ics, Line, Tlen+1, 2, Tlen);
yystate(4, [C|Ics], Line, Tlen, _, _) when C >= 97, C =< 100 ->
    yystate(18, Ics, Line, Tlen+1, 2, Tlen);
yystate(4, [C|Ics], Line, Tlen, _, _) when C >= 102, C =< 122 ->
    yystate(18, Ics, Line, Tlen+1, 2, Tlen);
yystate(4, Ics, Line, Tlen, _, _) ->
    {2,Tlen,Ics,Line,4};
yystate(3, [104|Ics], Line, Tlen, _, _) ->
    yystate(20, Ics, Line, Tlen+1, 2, Tlen);
yystate(3, [C|Ics], Line, Tlen, _, _) when C >= 97, C =< 103 ->
    yystate(18, Ics, Line, Tlen+1, 2, Tlen);
yystate(3, [C|Ics], Line, Tlen, _, _) when C >= 105, C =< 122 ->
    yystate(18, Ics, Line, Tlen+1, 2, Tlen);
yystate(3, Ics, Line, Tlen, _, _) ->
    {2,Tlen,Ics,Line,3};
yystate(2, [114|Ics], Line, Tlen, _, _) ->
    yystate(10, Ics, Line, Tlen+1, 2, Tlen);
yystate(2, [104|Ics], Line, Tlen, _, _) ->
    yystate(21, Ics, Line, Tlen+1, 2, Tlen);
yystate(2, [C|Ics], Line, Tlen, _, _) when C >= 97, C =< 103 ->
    yystate(18, Ics, Line, Tlen+1, 2, Tlen);
yystate(2, [C|Ics], Line, Tlen, _, _) when C >= 105, C =< 113 ->
    yystate(18, Ics, Line, Tlen+1, 2, Tlen);
yystate(2, [C|Ics], Line, Tlen, _, _) when C >= 115, C =< 122 ->
    yystate(18, Ics, Line, Tlen+1, 2, Tlen);
yystate(2, Ics, Line, Tlen, _, _) ->
    {2,Tlen,Ics,Line,2};
yystate(1, [97|Ics], Line, Tlen, _, _) ->
    yystate(14, Ics, Line, Tlen+1, 2, Tlen);
yystate(1, [C|Ics], Line, Tlen, _, _) when C >= 98, C =< 122 ->
    yystate(18, Ics, Line, Tlen+1, 2, Tlen);
yystate(1, Ics, Line, Tlen, _, _) ->
    {2,Tlen,Ics,Line,1};
yystate(0, [110|Ics], Line, Tlen, _, _) ->
    yystate(8, Ics, Line, Tlen+1, 2, Tlen);
yystate(0, [C|Ics], Line, Tlen, _, _) when C >= 97, C =< 109 ->
    yystate(18, Ics, Line, Tlen+1, 2, Tlen);
yystate(0, [C|Ics], Line, Tlen, _, _) when C >= 111, C =< 122 ->
    yystate(18, Ics, Line, Tlen+1, 2, Tlen);
yystate(0, Ics, Line, Tlen, _, _) ->
    {2,Tlen,Ics,Line,0};
yystate(S, Ics, Line, Tlen, Action, Alen) ->
    {Action,Alen,Tlen,Ics,Line,S}.

yyaction(0, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    yyaction_0(TokenChars, TokenLine);
yyaction(1, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    yyaction_1(TokenChars, TokenLine);
yyaction(2, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    yyaction_2(TokenChars, TokenLine);
yyaction(3, TokenLen, YYtcs, TokenLine) ->
    TokenChars = yypre(YYtcs, TokenLen),
    yyaction_3(TokenChars, TokenLine);
yyaction(4, _, _, _) ->
    yyaction_4();
yyaction(_, _, _, _) -> error.

-compile({inline,yyaction_0/2}).
-file("./while_t.xrl", 9).
yyaction_0(TokenChars, TokenLine) ->
     { token, { list_to_atom (TokenChars), TokenLine } } .

-compile({inline,yyaction_1/2}).
-file("./while_t.xrl", 10).
yyaction_1(TokenChars, TokenLine) ->
     { token, { list_to_atom (TokenChars), TokenLine } } .

-compile({inline,yyaction_2/2}).
-file("./while_t.xrl", 11).
yyaction_2(TokenChars, TokenLine) ->
     { token, { var, TokenLine, list_to_atom (TokenChars) } } .

-compile({inline,yyaction_3/2}).
-file("./while_t.xrl", 12).
yyaction_3(TokenChars, TokenLine) ->
     { token, { int, TokenLine, list_to_integer (TokenChars) } } .

-compile({inline,yyaction_4/0}).
-file("./while_t.xrl", 13).
yyaction_4() ->
     skip_token .


op(T) ->
    element(1, T).

-type yecc_ret() :: {'error', _} | {'ok', _}.

-spec parse(Tokens :: list()) -> yecc_ret().
parse(Tokens) ->
    yeccpars0(Tokens, {no_func, no_line}, 0, [], []).

-spec parse_and_scan({function() | {atom(), atom()}, [_]}
                     | {atom(), atom(), [_]}) -> yecc_ret().
parse_and_scan({F, A}) ->
    yeccpars0([], {{F, A}, no_line}, 0, [], []);
parse_and_scan({M, F, A}) ->
    Arity = length(A),
    yeccpars0([], {{fun M:F/Arity, A}, no_line}, 0, [], []).

-spec format_error(any()) -> [char() | list()].
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true ->
            Message;
        _ ->
            io_lib:write(Message)
    end.

-compile({nowarn_unused_function, return_error/2}).
-spec return_error(integer(), any()) -> no_return().
return_error(Line, Message) ->
    throw({error, {Line, ?MODULE, Message}}).

-define(CODE_VERSION, "1.4").

yeccpars0(Tokens, Tzr, State, States, Vstack) ->
    try yeccpars1(Tokens, Tzr, State, States, Vstack)
    catch 
        error: Error ->
            Stacktrace = erlang:get_stacktrace(),
            try yecc_error_type(Error, Stacktrace) of
                Desc ->
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                 Stacktrace)
            catch _:_ -> erlang:raise(error, Error, Stacktrace)
            end;
        %% Probably thrown from return_error/2:
        throw: {error, {_Line, ?MODULE, _M}} = Error ->
            Error
    end.

yecc_error_type(function_clause, [{?MODULE,F,ArityOrArgs,_} | _]) ->
    case atom_to_list(F) of
        "yeccgoto_" ++ SymbolL ->
            {ok,[{atom,_,Symbol}],_} = erl_scan:string(SymbolL),
            State = case ArityOrArgs of
                        [S,_,_,_,_,_,_] -> S;
                        _ -> state_is_unknown
                    end,
            {Symbol, State, missing_in_goto_table}
    end.

yeccpars1([Token | Tokens], Tzr, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens, Tzr);
yeccpars1([], {{F, A},_Line}, State, States, Vstack) ->
    case apply(F, A) of
        {ok, Tokens, Endline} ->
            yeccpars1(Tokens, {{F, A}, Endline}, State, States, Vstack);
        {eof, Endline} ->
            yeccpars1([], {no_func, Endline}, State, States, Vstack);
        {error, Descriptor, _Endline} ->
            {error, Descriptor}
    end;
yeccpars1([], {no_func, no_line}, State, States, Vstack) ->
    Line = 999999,
    yeccpars2(State, '$end', States, Vstack, yecc_end(Line), [],
              {no_func, Line});
yeccpars1([], {no_func, Endline}, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, yecc_end(Endline), [],
              {no_func, Endline}).

yeccpars1(State1, State, States, Vstack, Token0, [Token | Tokens], Tzr) ->
    yeccpars2(State, element(1, Token), [State1 | States],
              [Token0 | Vstack], Token, Tokens, Tzr);
yeccpars1(State1, State, States, Vstack, Token0, [], {{_F,_A}, _Line}=Tzr) ->
    yeccpars1([], Tzr, State, [State1 | States], [Token0 | Vstack]);
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, no_line}) ->
    Line = yecctoken_end_location(Token0),
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line});
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, Line}) ->
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line}).

yecc_end({Line,_Column}) ->
    {'$end', Line};
yecc_end(Line) ->
    {'$end', Line}.

yecctoken_end_location(Token) ->
    try erl_anno:end_location(element(2, Token)) of
        undefined -> yecctoken_location(Token);
        Loc -> Loc
    catch _:_ -> yecctoken_location(Token)
    end.

-compile({nowarn_unused_function, yeccerror/1}).
yeccerror(Token) ->
    Text = yecctoken_to_string(Token),
    Location = yecctoken_location(Token),
    {error, {Location, ?MODULE, ["syntax error before: ", Text]}}.

-compile({nowarn_unused_function, yecctoken_to_string/1}).
yecctoken_to_string(Token) ->
    try erl_scan:text(Token) of
        undefined -> yecctoken2string(Token);
        Txt -> Txt
    catch _:_ -> yecctoken2string(Token)
    end.

yecctoken_location(Token) ->
    try erl_scan:location(Token)
    catch _:_ -> element(2, Token)
    end.

-compile({nowarn_unused_function, yecctoken2string/1}).
yecctoken2string({atom, _, A}) -> io_lib:write_atom(A);
yecctoken2string({integer,_,N}) -> io_lib:write(N);
yecctoken2string({float,_,F}) -> io_lib:write(F);
yecctoken2string({char,_,C}) -> io_lib:write_char(C);
yecctoken2string({var,_,V}) -> io_lib:format("~s", [V]);
yecctoken2string({string,_,S}) -> io_lib:write_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:write(A);
yecctoken2string({_Cat, _, Val}) -> io_lib:format("~tp", [Val]);
yecctoken2string({dot, _}) -> "'.'";
yecctoken2string({'$end', _}) -> [];
yecctoken2string({Other, _}) when is_atom(Other) ->
    io_lib:write_atom(Other);
yecctoken2string(Other) ->
    io_lib:format("~tp", [Other]).



-file("while_p.erl", 179).

-dialyzer({nowarn_function, yeccpars2/7}).
yeccpars2(0=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(2=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(3=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(4=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(7=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(9=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(10=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(11=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(16=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(17=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(18=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(19=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(20=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(22=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(29=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(30=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(37=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(38=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(40=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(41=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(44=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(45=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(47=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(48=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(49=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(51=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.4",{missing_state_in_action_table, Other}}).

-dialyzer({nowarn_function, yeccpars2_0/7}).
yeccpars2_0(S, 'if', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 2, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, while, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 4, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_1/7}).
yeccpars2_1(_S, '$end', _Ss, Stack, _T, _Ts, _Tzr) ->
 {ok, hd(Stack)};
yeccpars2_1(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_2/7}).
yeccpars2_2(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 7, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, false, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, int, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, true, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_3/7}).
yeccpars2_3(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_3(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_4: see yeccpars2_2

yeccpars2_5(S, do, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_5(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_5/7}).
yeccpars2_cont_5(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_5(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_5(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_6(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_6(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_6/7}).
yeccpars2_cont_6(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_6(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_6(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_6(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_6(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_7: see yeccpars2_2

yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_8_(Stack),
 yeccgoto_bexp(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_aexp(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_10_(Stack),
 yeccgoto_bexp(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_aexp(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_12(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_5(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_13(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_6(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_14_(Stack),
 yeccgoto_aexp(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_15/7}).
yeccpars2_15(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(S, int, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(_, _, _, _, T, _, _) ->
 yeccerror(T).


yeccpars2_21(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_21_(Stack),
 yeccgoto_bexp(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).


yeccpars2_23(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_6(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_24(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_24(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_24(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_24(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_24_(Stack),
 yeccgoto_bexp(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_25_(Stack),
 yeccgoto_aexp(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_26(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_26(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_26_(Stack),
 yeccgoto_aexp(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_27(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_27(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_27_(Stack),
 yeccgoto_aexp(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_28_(Stack),
 yeccgoto_aexp(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_29_(Stack),
 yeccgoto_bexp(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).


yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_32_(Stack),
 yeccgoto_bexp(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_33_(Stack),
 yeccgoto_bexp(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_34/7}).
yeccpars2_34(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(_, _, _, _, T, _, _) ->
 yeccerror(T).



-dialyzer({nowarn_function, yeccpars2_36/7}).
yeccpars2_36(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(_, _, _, _, T, _, _) ->
 yeccerror(T).


yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_38_(Stack),
 yeccgoto_stmt(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_39(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_39_(Stack),
 yeccgoto_stmt(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_40/7}).
yeccpars2_40(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_40(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_41: see yeccpars2_15

yeccpars2_42(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_42_(Stack),
 yeccgoto_stmt(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_43(S, then, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_5(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_44/7}).
yeccpars2_44(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_45: see yeccpars2_0

-dialyzer({nowarn_function, yeccpars2_46/7}).
yeccpars2_46(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_47/7}).
yeccpars2_47(S, else, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_48/7}).
yeccpars2_48(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_49: see yeccpars2_0

-dialyzer({nowarn_function, yeccpars2_50/7}).
yeccpars2_50(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_51_(Stack),
 yeccgoto_stmt(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_aexp/7}).
yeccgoto_aexp(2, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(6, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_aexp(4, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(6, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_aexp(7, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(13, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_aexp(15=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_aexp(16, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(27, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_aexp(17, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(26, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_aexp(18=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_aexp(19, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(24, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_aexp(20, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(21, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_aexp(22, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(23, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_aexp(30, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(6, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_aexp(31, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(6, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_aexp(41, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_bexp/7}).
yeccgoto_bexp(2, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(43, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_bexp(4, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(5, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_bexp(7, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_bexp(30=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_bexp(31=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_stmt/7}).
yeccgoto_stmt(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_stmt(35, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_stmt(37, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_stmt(45, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_stmt(49, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(50, Cat, Ss, Stack, T, Ts, Tzr).

-compile({inline,yeccpars2_8_/1}).
-file("while_p.yrl", 48).
yeccpars2_8_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   false
  end | __Stack].

-compile({inline,yeccpars2_10_/1}).
-file("while_p.yrl", 45).
yeccpars2_10_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   true
  end | __Stack].

-compile({inline,yeccpars2_14_/1}).
-file("while_p.yrl", 38).
yeccpars2_14_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_21_/1}).
-file("while_p.yrl", 60).
yeccpars2_21_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , op ( __2 ) , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_24_/1}).
-file("while_p.yrl", 57).
yeccpars2_24_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , op ( __2 ) , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_25_/1}).
-file("while_p.yrl", 35).
yeccpars2_25_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , op ( __2 ) , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_26_/1}).
-file("while_p.yrl", 29).
yeccpars2_26_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , op ( __2 ) , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_27_/1}).
-file("while_p.yrl", 26).
yeccpars2_27_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , op ( __2 ) , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_28_/1}).
-file("while_p.yrl", 32).
yeccpars2_28_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , op ( __2 ) , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_29_/1}).
-file("while_p.yrl", 63).
yeccpars2_29_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_32_/1}).
-file("while_p.yrl", 54).
yeccpars2_32_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , op ( __2 ) , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_33_/1}).
-file("while_p.yrl", 51).
yeccpars2_33_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { op , op ( __2 ) , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_38_/1}).
-file("while_p.yrl", 79).
yeccpars2_38_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ { while , __2 , __5 } ]
  end | __Stack].

-compile({inline,yeccpars2_39_/1}).
-file("while_p.yrl", 73).
yeccpars2_39_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ hd ( __1 ) | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_42_/1}).
-file("while_p.yrl", 70).
yeccpars2_42_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ { op , op ( __3 ) , __1 , __4 } ]
  end | __Stack].

-compile({inline,yeccpars2_51_/1}).
-file("while_p.yrl", 76).
yeccpars2_51_(__Stack0) ->
 [__10,__9,__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ { 'if' , __2 , __5 , __9 } ]
  end | __Stack].