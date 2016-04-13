% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/string-compression

-module(solution).
-export([main/0]).

main() ->
    Data = read_data(),
    output_data(calculate(Data)),
    ok.

 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Calculate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calculate(L) ->
    calculate(tl(L), hd(L), 1, []). 
    
calculate([H|T], H, Count, Acc) ->
    calculate(T, H, Count + 1, Acc);
calculate([H|T], Prev, Count, Acc) ->
    calculate(T, H, 1, append_prev(Prev, Count, Acc));
calculate([], Prev, Count, Acc) ->
    lists:reverse(append_prev(Prev, Count, Acc)).

append_prev(Prev, 1, Acc) ->
    [Prev|Acc];
append_prev(Prev, N, Acc) ->
    lists:reverse(integer_to_list(N)) ++  [Prev|Acc].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(Data) -> 
    io:format("~s\n", [Data]). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    [Line|_] = read_input([]),
    Line.

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
