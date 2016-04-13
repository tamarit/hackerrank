% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/rotate-string

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
    calculate_one(L, 0, []).

calculate_one(L, R, Acc) when R < length(L) ->
    calculate_one(L, R + 1, [rotate(L, R) |Acc]);
calculate_one(_, _, Acc) ->
    Acc.

rotate(L, R) when R > 0 ->
    rotate([lists:last(L) | lists:droplast(L)], R - 1);
rotate(L, R) when R =:= 0 ->
    L.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(Data) -> 
    lists:foreach(fun show_str_list/1, Data). 

show_str_list(StrList) ->
    io:format(
        "~s\n", 
        [string:join(StrList, " ")]).

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