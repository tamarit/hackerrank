% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/remove-duplicates

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
    calculate(List, []).

calculate([H|T], Acc) ->
    NT = filter(T, H, []),
    calculate(NT, [H|Acc]); 
calculate([], Acc) ->
    lists:reverse(Acc).
    
filter([H|T], H, Acc) ->
    filter(T, H, Acc);
filter([Other|T], H, Acc) ->
    filter(T, H, [Other|Acc]);
filter([], _, Acc) ->
    lists:reverse(Acc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(Data) -> 
    show_str(Data). 

show_str(Str) ->
    io:format("~s\n", [Str]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    hd(read_input([])).

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