% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/string-o-permute

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
    calculate_one(L, []).

calculate_one([E1, E2| T], Acc) ->
    calculate_one(T, [E1, E2 |Acc]);
calculate_one([], Acc) ->
    lists:reverse(Acc).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(Data) -> 
    lists:foreach(fun show_str/1, Data). 

show_str(Str) ->
    io:format("~s\n", [Str]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    tl(read_input([])).

% str2int(Str) ->
%     element(1,string:to_integer(Str)).

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