% https://www.hackerrank.com/challenges/grid-challenge

% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

-module(solution).
-export([main/0]).

main() ->
    InputContent = read_input([]),
    Tests = parse_input(InputContent),
    Results = lists:map(fun analyze/1, Tests),
    [io:format("~s\n", [R])  || R <- Results].

parse_input([]) ->
    [];
parse_input([TestsStr | RestInput]) ->
    Tests = 
        element(1,string:to_integer(TestsStr)),
    read_tests(Tests, RestInput, []).

read_tests(0, _, Acc) ->
    lists:reverse(Acc);
read_tests(N, [RowsStr | RestInput], Acc) ->
    Rows = 
        element(1,string:to_integer(RowsStr)),
    RowsContent = 
        [ lists:nth(I, RestInput) 
         || I <- lists:seq(1,Rows)],
    read_tests(
        N - 1, 
        lists:nthtail(Rows, RestInput) , 
        [{Rows, RowsContent} | Acc]).


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

analyze({Rows, Grid}) ->
    SortedRows = 
        [lists:sort(R) || R <- Grid],
    case sorted_cols(1, Rows + 1, SortedRows) of 
        true -> 
            "YES";
        false -> 
            "NO"
    end.

is_sorted([]) ->
    true;
is_sorted([_]) ->
    true;
is_sorted([A, B|Rest]) when A =< B ->
    is_sorted([B|Rest]);
is_sorted(_) ->
    false.

sorted_cols(SupCols, SupCols, _) ->
    true;
sorted_cols(ColNumber, SupCols, Grid) ->
    Col = 
        [lists:nth(ColNumber, R) || R <- Grid],
    case is_sorted(Col) of 
        true ->
            sorted_cols(ColNumber + 1, SupCols, Grid) ;
        false ->
            false 
    end.
