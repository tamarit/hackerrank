% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/functions-and-fractals-sierpinski-triangles

-module(solution).
-export([main/0, main2/0]).

main() ->
    Data = read_data(),
    output_data(calculate(Data)),
    ok.

main2() ->
    lists:foreach(
        fun(I) -> 
            output_data(calculate(I)) 
        end, 
        lists:seq(0,5)).

 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Calculate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calculate(N) ->
    Tri = triangle(N),
    replicate(N,Tri).

replicate(0, Tri) ->
    Tri;
replicate(N, Tri) ->
    Space = lists:duplicate(length(Tri), $_),
    Upper = lists:map(fun(R) -> Space ++ R ++ Space end,Tri),
    Lower = lists:map(fun(R) -> R ++ "_" ++ R end,Tri),
    replicate(N - 1, Upper ++ Lower).

triangle(N) ->
    Cols = trunc(math:pow(2, 6 - N)) - 1,
    triangle_draw(lists:duplicate(Cols, $1), Cols, []).

triangle_draw(L = [$1, $1|T], Cols, Acc) ->
    triangle_draw(T, Cols, [draw_line(L, Cols) | Acc]);
triangle_draw([$1], Cols, Acc) ->
    [draw_line([$1], Cols) | Acc].

draw_line(Tri, Cols) ->
    SizeSpaces = trunc((Cols - length(Tri))/2),
    Space = lists:duplicate(SizeSpaces, $_),
    Space ++ Tri ++ Space.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(Data) ->
    Output = 
        lists:foldl(
            fun(S, Acc) -> 
                [Acc | io_lib:format("~s\n", [S])]
            end,
            "", Data), 
    io:format("~s\n", [lists:droplast(lists:flatten(Output))]). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    [NTestsStr|_] = read_input([]),
    str2int(NTestsStr).

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
