% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/sequence-full-of-colors

-module(solution).
-export([main/0]).

main() ->
    Data = read_data(),
    output_data(calculate(Data)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Calculate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calculate(List) ->
    lists:map(fun calculate_one/1, List).

calculate_one(L) ->
    Dict = 
        lists:foldl(
            fun(C, CDict) -> dict:store(C,0,CDict) end,
            dict:new(),
            [$R,$G,$B,$Y]
        ),
    calculate_one(L, Dict).

% There are as many red balls as green balls.
% There are as many yellow balls as blue balls.
% Difference between the number of red balls and green balls in every prefix of the sequence is at most 1.
% Difference between the number of yellow balls and blue balls in every prefix of the sequence is at most 1.

calculate_one([H|T],Dict) ->
    NDict = dict:store(H, dict:fetch(H, Dict) + 1, Dict),
    DiffAtMost1 = 
        fun(C1, C2) -> 
            abs(dict:fetch(C1, NDict) - dict:fetch(C2, NDict)) =< 1
        end,
    case DiffAtMost1($R,$G) and DiffAtMost1($Y,$B) of
        true ->
            calculate_one(T, NDict);
        false -> 
            false
    end;
calculate_one([],Dict) ->
    Equal = 
        fun(C1, C2) -> 
            dict:fetch(C1, Dict) == dict:fetch(C2, Dict)
        end,
    Equal($R,$G) and Equal($Y,$B) .
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(Data) -> 
    lists:foreach(fun show_str_bool/1, Data). 

show_str_bool(Bool) ->
    io:format(
        "~s\n", 
        [str_bool(Bool)]).

str_bool(true) -> "True";
str_bool(false) -> "False".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    tl(read_input([])).

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