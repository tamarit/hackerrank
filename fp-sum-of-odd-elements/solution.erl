% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/fp-sum-of-odd-elements

-module(solution).
-export([main/0]).

main() ->
    read_data(),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input/Calculate/Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    read_input(0).

read_input(Acc) ->
    case io:get_line("") of
        eof ->
            io:format("~p\n", [Acc]);
        E0 ->
            E = 
                case lists:last(E0) of 
                    $\n ->
                        lists:droplast(E0);
                    _ ->
                        E0 
                end,
            IE = str2int(E),
            NAcc = 
                case abs(IE rem 2) of 
                    1 -> 
                        Acc + IE;
                    0 -> 
                        Acc 
                end,
            read_input(NAcc)
    end.

str2int(Str) ->
    element(1,string:to_integer(Str)).