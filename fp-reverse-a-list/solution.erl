% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/fp-reverse-a-list

-module(solution).
-export([main/0]).

main() ->
    read_data(),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input/Calculate/Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    read_input([]).

read_input(L) ->
    case io:get_line("") of
        eof ->
            lists:foreach(fun(S) -> io:format("~s\n", [S]) end, L);
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