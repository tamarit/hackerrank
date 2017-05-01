% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/crosswords-101

-module(solution).
-export([main/0]).

main() ->
    Data = read_data(),
    output_data(calculate(Data)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Calculate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calculate({Grid ,Words}) ->
    Holes = search_holes(Grid),
    InitialDict = 
        lists:foldl(
            fun(H, CDict) -> dict:store(H,$+,CDict) end,
            dict:new(),
            [{I,J} || I <- lists:seq(1,10), J <- lists:seq(1,10)]
        ),
    % io:format("\n**********\nHoles: ~p\nDict: ~p\n**********\n", [Holes, dict:to_list(InitialDict)]),
    % fill_grid(Holes, Words, InitialDict),
    % InitialDict.
    {true, FDict} = 
        fill_grid(Holes, Words, InitialDict),
    FDict.

fill_grid([Hole = {_, _, _, Length} | Holes], Words, Dict) ->
    % io:format("\n**********\nHole: ~p\nWords: ~p\n**********\n", [Hole, Words]),
    WordsFit = 
        lists:flatten(
            [
                case check_chars(Hole, W, Dict) of 
                    {true, NDict} -> 
                        [{W, NDict}];
                    false -> 
                        []
                end  
                || W <- Words, length(W) == Length
            ]
        ),
    FinalDicts = 
        lists:flatten(
            [
                case fill_grid(Holes, Words -- [W], NDict) of
                    {true, FDict} ->
                        [FDict];
                    false -> 
                        []
                end
                || {W, NDict} <- WordsFit
            ]
        ),
    case FinalDicts of 
        [] -> 
            false;
        [FDict|_] -> 
            {true, FDict}
    end;
fill_grid([], [], Dict) ->
    {true, Dict};
fill_grid([], _, _) ->
    false;
fill_grid(_, [], _) ->
    false.


% Check if word chars match current ones
check_chars({NR, NC, Direction, Length}, [C|Cs], Dict) ->  
    % io:format("\n**********\nNR: ~p\nNC: ~p\nWord: ~s\n**********\n", [NR, NC, Word]),
    {NewNR, NewNC} =
        case Direction of 
            right -> 
                {NR, NC + 1};
            down -> 
                {NR + 1, NC}
        end,
    % check_chars({NewNR, NewNC, Direction, Length}, Cs, Dict);
    case dict:fetch({NR,NC}, Dict) of 
        $+ ->
            check_chars({NewNR, NewNC, Direction, Length}, Cs, dict:store({NR, NC}, C, Dict));
        C -> 
            check_chars({NewNR, NewNC, Direction, Length}, Cs, Dict);
        _ ->
            false
    end;
check_chars(_, [], Dict) ->
    {true, Dict}.

% Read grid holes
search_holes(Grid) ->
    search_holes_by_rows(Grid, 1, []) ++ search_holes_by_cols(Grid, 1, []).

search_holes_by_rows([R|Rs], NR, Acc) ->
    HolesR = 
        [{NR, NC, right, Length} 
         || {NC, Length} <- search_holes_line(R, 1, [])],
    search_holes_by_rows(Rs, NR + 1, HolesR ++ Acc);
search_holes_by_rows([], _, Acc) ->
    Acc.

search_holes_by_cols(Grid, NC, Acc) when NC =< 10 ->
    C = [lists:nth(NC,R) || R <- Grid],
    HolesC = 
        [{NR, NC, down, Length} 
         || {NR, Length} <- search_holes_line(C, 1, [])],
    search_holes_by_cols(Grid, NC + 1, HolesC ++ Acc);
search_holes_by_cols(_, NC, Acc) when NC > 10 ->
    Acc.

search_holes_line([$+|Cs], N, Acc) ->
    search_holes_line(Cs, N + 1, Acc);
search_holes_line([$-|Cs], N, Acc) ->
    CsWithoutHole = 
        lists:dropwhile(fun($-) -> true; (_) -> false end, Cs),
    NAcc = 
        case (length(Cs) - length(CsWithoutHole)) of 
            0 -> 
                Acc;
            Diff ->
                [{N, Diff + 1} | Acc]
        end,
    search_holes_line(CsWithoutHole, N + 1, NAcc);
search_holes_line([], _, Acc) ->
    Acc.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(Data) -> 
    GridStr = 
        [[dict:fetch({R,C}, Data) || C <- lists:seq(1,10)] || R <- lists:seq(1,10)],
    io:format(
        "~s\n", 
        [string:join(GridStr, "\n")]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    Read = 
        read_input([]),
    [WordsStr|GridR] = 
        lists:reverse(Read),
    WordsPre = 
        string:tokens(WordsStr, ";"),
    Words = 
        [string:strip(W, both) || W <- WordsPre], 
    Grid = 
        [string:strip(R, both) || R <- lists:reverse(GridR)],
    {Grid, Words}.

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