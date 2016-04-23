% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/area-under-curves-and-volume-of-revolving-a-curv

-module(solution).
-export([main/0]).

main() ->
    Data = read_data(),
    output_data(calculate(Data)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Calculate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calculate({As, Bs, [L,R]}) ->
    AsBs = lists:zip(As,Bs),
    Int = seq_float(L, R, 0.001),
    {Areas, AreaTotal} = 
        lists:mapfoldl(
            fun(X, Acc) ->
                Area = lists:sum([A * math:pow(abs(X), B) || {A, B} <- AsBs]),
                {Area, Area + Acc}
            end,
            0,
            Int
            ),
    Volume = 
        lists:foldl(
            fun(Area, Acc) ->
                math:pow(Area,2) + Acc
            end,
            0,
            Areas
            ),
    [AreaTotal / (1/0.001), (math:pi() * Volume) / (1/0.001)].


% From https://gist.github.com/andruby/241489
seq_float(Min, Max, Inc, Counter, Acc) when (Counter*Inc + Min) >= Max -> 
    lists:reverse([Max|Acc]);
seq_float(Min, Max, Inc, Counter, Acc) -> 
    seq_float(Min, Max, Inc, Counter+1, [Inc * Counter + Min|Acc]).
seq_float(Min, Max, Inc) -> 
    seq_float(Min, Max, Inc, 0, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(Data) ->
    lists:foreach(fun print_float/1, Data). 

print_float(F) ->
    io:format("~.1f\n", [F]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    Input = read_input([]),
    [As, Bs, Int] = lists:map(fun strList2intList/1, Input),
    {As, Bs, Int}.

strList2intList(StrList) ->
    [str2int(Str) || Str <- string:tokens(StrList, " ")].

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