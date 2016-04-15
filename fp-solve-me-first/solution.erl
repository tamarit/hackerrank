% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/fp-solve-me-first

-module(solution).
-export([main/0]).

main() ->
    Data = read_data(),
    output_data(calculate(Data)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Calculate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calculate({N1,N2}) ->
    N1 + N2.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(Data) -> 
    io:format("~p", [Data]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    [N1, N2|_] = read_input([]),
    {str2int(N1), str2int(N2)}.

str2int(Str) ->
    element(1,string:to_integer(Str)).

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