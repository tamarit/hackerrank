% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/eval-ex

-module(solution).
-export([main/0]).

main() ->
    Data = read_data(),
    output_data(calculate(Data)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Calculate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calculate(Tests) ->
    lists:map(fun calculate_test/1, Tests).
    
calculate_test(T) ->
    Initial = T*T / 2,
    1 + T + Initial + 
        lists:sum(
            element(1,lists:mapfoldl(
                fun(E, C) -> NE = (T*C)/E, {NE,NE} end, 
                Initial,
                lists:seq(3,9)
            ))
        ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(Data) ->
    lists:foreach(fun print_float/1, Data). 

print_float(F) ->
    io:format("~.4f\n", [F]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    Tests = tl(read_input([])),
    [str2float(T) || T <- Tests].

str2float(Str) ->
    element(1,string:to_float(Str)).

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