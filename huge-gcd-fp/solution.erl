% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/huge-gcd-fp

-module(solution).
-export([main/0]).

main() ->
    Data = read_data(),
    output_data(calculate(Data)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Calculate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calculate({LA0, LB0}) ->
    Mult = 
        fun(L) ->
            lists:foldl(
                fun(E, Acc) -> E * Acc end,
                1,
                L
            )
        end,
    A0 = Mult(LA0),
    B0 = Mult(LB0),
    {A,B} = 
        case A0 > B0 of 
            true ->
                {A0, B0};
            false ->
                {B0, A0}
        end,
    calculate(A,B) rem 1000000007.

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
    [_,L1,_,L2] = read_input([]),
    {str2listint(L1), str2listint(L2)}.

str2int(Str) ->
    element(1,string:to_integer(Str)).

str2listint(Str) ->
    LStr = 
        string:tokens(Str, " "),
    [str2int(S)  || S <- LStr].

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