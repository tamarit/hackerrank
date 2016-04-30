% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/missing-numbers-fp

-module(solution).
-export([main/0]).

main() ->
    Data = read_data(),
    output_data(calculate(Data)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Calculate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calculate({A, B}) ->
    calculate(A, B , []).

calculate(A, [H|T], Acc) ->
    {Found, NA} = search(H,A, []),  
    calculate(NA, T, case Found of true -> Acc; false -> [H|Acc] end);
calculate(_, [], Acc) ->
    lists:usort(Acc).

search(Elem, [Elem|T], Acc) ->
    {true, lists:reverse(Acc) ++ T};
search(Elem, [H|T], Acc) ->
    search(Elem, T, [H|Acc]);
search(_, [], Acc) ->
    {false, lists:reverse(Acc)}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(Data) -> 
    String = [lists:flatten(io_lib:format("~p", [D])) || D <- Data],
    io:format("~s\n", [string:join(String, " ")]).


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