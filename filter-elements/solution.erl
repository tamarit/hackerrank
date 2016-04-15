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
    InitialMap = 
        lists:foldl(
            fun(E, CMap) -> maps:put(E, 0, CMap) end,
            #{},
            lists:usort(List)
        ),
    FinalMap = 
        calculate_test(List, K, InitialMap),
    get_k_repeated_numbers(List, K, FinalMap, []).

calculate_test([H|T], K, Map) ->
    PrevValue  = maps:get(H,Map),
    NT = 
        case PrevValue >= K of 
            true -> 
                lists:filter(fun(E) -> E /= H end, T);
            false -> 
                T
        end,
    calculate_test(NT, K, Map#{H => PrevValue + 1});
calculate_test([], _, Map) ->
    Map.


get_k_repeated_numbers([H|T], K, Map, Acc) ->
    Total = maps:get(H,Map),
    NT = lists:filter(fun(E) -> E /= H end, T),
    NAcc = 
        case Total >= K of 
            true -> 
                [H|Acc];
            false ->
                Acc
        end,
    get_k_repeated_numbers(NT, K, Map, NAcc);
get_k_repeated_numbers([], _, _, Acc) ->
    case Acc of 
        [] ->
            [-1];
        _ ->
            lists:reverse(Acc)
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