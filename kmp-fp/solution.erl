% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/kmp-fp

-module(solution).
-export([main/0, prof/0, kmp_table/1]).


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

calculate(Tests) ->
    lists:map(
        fun({S, W}) -> 
            kmp(list_to_tuple(W), list_to_tuple(S), kmp_table(S), 0, 1) 
        end, 
        Tests).


 % algorithm kmp_search:
 %    input:
 %        an array of characters, S (the text to be searched)
 %        an array of characters, W (the word sought)
 %    output:
 %        an integer (the zero-based position in S at which W is found)

 %    define variables:
 %        an integer, m ← 0 (the beginning of the current match in S)
 %        an integer, i ← 0 (the position of the current character in W)
 %        an array of integers, T (the table, computed elsewhere)

 %    while m + i < length(S) do
 %        if W[i] = S[m + i] then
 %            if i = length(W) - 1 then
 %                return m
 %            let i ← i + 1
 %        else
 %            if T[i] > -1 then
 %                let m ← m + i - T[i], i ← T[i]
 %            else
 %                let m ← m + 1, i ← 0
            
 %    (if we reach here, we have searched all of S unsuccessfully)
 %    return the length of S

kmp(W, S, T, M, I) when M + I =< tuple_size(S) ->
    % io:format("~p\n", [{W, S, T, M, I}]),
    case element(I, W) == element(M + I, S) of 
        true -> 
            case I == tuple_size(W) of 
                true ->
                    true;
                false ->
                    kmp(W, S, T, M, I + 1)
            end;
        false ->
            TI = element(I, T),
            case TI of 
                0 ->
                    kmp(W, S, T, M + 1, 1);
                _ ->
                    kmp(W, S, T, M + I - TI, TI)
            end
    end;
kmp(_, _, _, _, _) ->
% kmp(W, S, T, M, I) ->
    % io:format("~p\n", [{W, S, T, M, I}]),
    false.


%  algorithm kmp_table:
%     input:
%         an array of characters, W (the word to be analyzed)
%         an array of integers, T (the table to be filled)
%     output:
%         nothing (but during operation, it populates the table)

%     define variables:
%         an integer, pos ← 2 (the current position we are computing in T)
%         an integer, cnd ← 0 (the zero-based index in W of the next 
% character of the current candidate substring)

%     (the first few values are fixed but different from what the algorithm 
% might suggest)
%     let T[0] ← -1, T[1] ← 0

%     while pos < length(W) do
%         (first case: the substring continues)
%         if W[pos-1] = W[cnd] then
%             let T[pos] ← cnd + 1, cnd ← cnd + 1, pos ← pos + 1

%         (second case: it doesn't, but we can fall back)
%         else if cnd > 0 then
%             let cnd ← T[cnd]

%         (third case: we have run out of candidates.  Note cnd = 0)
%         else
%             let T[pos] ← 0, pos ← pos + 1


kmp_table([]) ->
    {};
kmp_table([_]) ->
    {0};
kmp_table(W) ->
    IniDict = 
        dict:store(2, 1, dict:store(1, 0, dict:new())),
    Dict = 
        kmp_table_gen(list_to_tuple(W), 3, 1, IniDict),
    list_to_tuple([T || {_, T} <- lists:sort(dict:to_list(Dict))]).

kmp_table_gen(W, Pos, Cnd, Dict) when Pos =< tuple_size(W) ->
    case element(Pos - 1, W) == element(Cnd, W) of 
        true ->
            kmp_table_gen(W, Pos + 1, Cnd + 1, dict:store(Pos, Cnd + 1, Dict));
        false ->
            case Cnd > 1 of 
                true ->
                    kmp_table_gen(W, Pos, dict:fetch(Cnd, Dict), Dict);
                false ->
                    kmp_table_gen(W, Pos + 1, Cnd, dict:store(Pos, 1, Dict))
            end
    end;
kmp_table_gen(_, _, _, Dict) ->
    Dict.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(Ds) ->
    [case D of 
        true -> 
            io:format("YES\n");
        false ->
            io:format("NO\n")
    end || D <- Ds].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    Binary = read(),
    Res = binary:split(Binary, [<<"\n">>], [global]),
    [_ | TestsTogether] = [binary_to_list(R) || R <- Res],
    separate_test(TestsTogether, []).

separate_test([L1, L2 | Tests], Acc) ->
    separate_test(Tests, [{L1,L2} | Acc]);
separate_test([], Acc) ->
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

