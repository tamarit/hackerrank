% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/fp-filter-array

-module(solution).
-export([main/0]).

main() ->
    read_data(),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input/Calculate/Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    read_input(true, 0).

read_input(First, N) ->
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
            case First of 
                true ->
                    read_input(false, str2int(E));
                false ->
                    EI = str2int(E),
                    case EI < N of 
                        true ->
                            io:format("~p\n", [EI]);
                        false ->
                            ok
                    end,
                    read_input(false, N)
            end
    end.

str2int(Str) ->
    element(1,string:to_integer(Str)).