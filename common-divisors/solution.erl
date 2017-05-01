% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/common-divisors

-module(solution).
-export([main/0]).

main() ->
    Data = read_data(),
    output_data(calculate(Data)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Calculate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calculate(List) ->
    calculate(List, []).

calculate([[A,B]|T], Acc) ->
    DA = divs(A),
    DB = divs(B),
    calculate(T, 
        [sets:size(
            sets:intersection(
                sets:from_list(DA), 
                sets:from_list(DB)))
        | Acc]);
calculate([], Acc) ->
    lists:reverse(Acc).


% From https://rosettacode.org/wiki/Proper_divisors#Erlang
divs(0) -> [];
divs(1) -> [1];
divs(N) -> [1, N] ++ divisors(2,N,math:sqrt(N)).
 
divisors(K,_N,Q) when K > Q -> [];
divisors(K,N,Q) when N rem K =/= 0 -> 
    divisors(K+1,N,Q);
divisors(K,N,Q) when K * K  == N -> 
    [K] ++ divisors(K+1,N,Q);
divisors(K,N,Q) ->
    [K, N div K] ++ divisors(K+1,N,Q).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(Data) -> 
    lists:foreach(fun(D) -> io:format("~p\n", [D]) end, Data).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    Input = tl(read_input([])),
    NumsStr = 
        [string:tokens(Str, " ") || Str <- Input],
    Res = [[str2int(NumStr)  || NumStr <- S] || S <- NumsStr],
    Res.

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