% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/fp-update-list

-module(solution).
-export([main/0]).

main() ->
    read_data(),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input/Calculate/Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    read_input().

read_input() ->
    case io:get_line("") of
        eof ->
            ok;
        E0 ->
            E = 
                case lists:last(E0) of 
                    $\n ->
                        lists:droplast(E0);
                    _ ->
                        E0 
                end,
            io:format("~p\n", [abs(str2int(E))]),
            read_input()
    end.

str2int(Str) ->
    element(1,string:to_integer(Str)).