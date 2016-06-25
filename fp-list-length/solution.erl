% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/fp-list-length

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

read_input(N) ->
    case io:get_line("") of
        eof ->
            io:format("~p\n", [N]);
        _ ->
            read_input(N+1)
    end.