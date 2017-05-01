% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/filter-elements

-module(solution).
-export([main/0]).

main() ->
    Data = read_data(),
    output_data(calculate(Data)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Calculate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calculate(Tests) ->
    lists:map(fun calculate_test/1, Tests).

calculate_test({K, List}) ->
    {_, _, RepeatedDict} = 
        lists:foldl(
            fun(E, Acc)-> 
                get_repeated(E, Acc, K)
            end,
            {#{}, 1, orddict:new()},
            List),
    case orddict:to_list(RepeatedDict) of 
        [] ->
            [-1];
        Res ->
            [N || {_, N} <- Res]
    end.

get_repeated(E, {Map, N, LRep}, K) ->
    case maps:is_key(E, Map) of 
        false ->
            case K of 
                1 -> 
                    {maps:put(E, added, Map), N + 1, orddict:store(N, E, LRep)};
                _ -> 
                    {maps:put(E, {1, N}, Map), N + 1, LRep}
            end;
        true ->  
            case maps:get(E, Map) of 
                added -> 
                    {Map, N + 1, LRep};
                {CK, FirstTime} when CK == (K - 1) ->
                    {maps:put(E, added, Map), N + 1, orddict:store(FirstTime, E, LRep)};
                {CK, FirstTime}  ->
                    {maps:put(E, {CK + 1, FirstTime}, Map), N + 1, LRep}
            end
    end.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(Data) -> 
    TestStr = 
        lists:map(fun ints_to_str/1, Data),
    io:format(
        "~s\n", 
        [string:join(TestStr, "\n")]).

ints_to_str(Ns) -> 
    string:join(list_int_to_list_str(Ns), " ").

list_int_to_list_str(Ns) ->
    [lists:flatten(io_lib:format("~p", [N])) || N <- Ns].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    read_tests(tl(read_input([])), []).

read_tests([NK0, Nums0|Rest], Acc) ->
    NKStr = 
        string:tokens(NK0, " "),
    NumsStr = 
        string:tokens(Nums0, " "),
    [_, K | Nums] = 
        [str2int(ElemStr) || ElemStr <- NKStr ++ NumsStr],
    read_tests(Rest, [{K, Nums} | Acc]);
read_tests([], Acc) ->
    lists:reverse(Acc).

str2int(Str) ->
    element(1,string:to_integer(Str)).

read_input(L) ->
    case io:get_line("") of
        eof ->
            lists:reverse(L);
        E0 ->
            E = 
                case lists:last(E0) of 
                    $\n ->
                        lists:droplast(E0);
                    _ ->
                        E0 
                end,
            read_input([E|L])
    end.