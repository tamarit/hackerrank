% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/prefix-compression

-module(solution).
-export([main/0]).

main() ->
    output_data(calculate(read_data())),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Calculate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calculate({L1,L2}) ->
    calculate(L1, L2, []).

calculate([C|T1], [C|T2], Acc) ->
    calculate(T1, T2, [C|Acc]);
calculate(L1, L2, Acc) ->
    [lists:reverse(Acc), L1, L2].
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(Data) -> 
    lists:foreach(fun show_leng_str/1, Data). 

show_leng_str(Str) ->
    io:format(
        "~p ~s\n", 
        [length(Str), Str]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    [S1,S2|_] = read_input([]),
    {S1,S2}.

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