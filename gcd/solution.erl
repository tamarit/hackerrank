% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/functional-programming-warmups-in-recursion---gcd

-module(solution).
-export([main/0]).

main() ->
    Data = read_data(),
    output_data(calculate(Data)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Calculate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calculate([A0, B0]) ->
    {A,B} = 
        case A0 > B0 of 
            true ->
                {A0, B0};
            false ->
                {B0, A0}
        end,
    calculate(A,B).

calculate(A, B) ->
    Rem = A rem B,
    case Rem of 
        0 -> 
            B;
        _ ->
           calculate(B, Rem) 
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(Data) -> 
    io:format("~p\n", [Data]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    Input = hd(read_input([])),
    NumsStr = 
        string:tokens(Input, " "),
    [str2int(NumStr)  || NumStr <- NumsStr].

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