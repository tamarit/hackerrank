% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/the-tree-of-life

-module(solution).
-export([main/0, prof/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main() ->
    Data = read_data(),
    Res = calculate(Data),
    output_data(Res),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Profiling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prof() ->
    eprof:start(),
    eprof:start_profiling([self()]),
    main2(),
    eprof:stop_profiling(),
    eprof:analyze(total). 

main2() ->
    StartInput = os:timestamp(),
    Data = read_data(),
    io:format(
        "INPUT: total time taken ~p seconds~n", 
        [timer:now_diff(os:timestamp(), StartInput)/1000000]),
    StartRes = os:timestamp(),
    Res = calculate(Data),
    io:format(
        "CALCULATE: total time taken ~p seconds~n", 
        [timer:now_diff(os:timestamp(), StartRes)/1000000]),
    StartOutput = os:timestamp(),
    output_data(Res),
    io:format(
        "OUTPUT: total time taken ~p seconds~n", 
        [timer:now_diff(os:timestamp(), StartOutput)/1000000]),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Calculate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Notes from Discussion: 
% -------------------------
% Apparently we have to assume that the "parent of the root and children of the leaves" are always False, for purposes of evolving the cellular automata.
% If path does not exitst, then False
% DOUBT: The tree is reseted each test or is the result of previous test.

calculate({TreeStr, Rule, Tests}) ->
    Keys = 
        [int2bin(N, 4) || N  <- lists:seq(15, 0, -1)],
    RuleDict = 
        dict:from_list(lists:zip(Keys, Rule)),
    {TreeDictIni, []} = 
        parse_tree(TreeStr, [], dict:new()),
    % io:format(
    %     "RuleDict: ~p\n", 
    %     [lists:sort(dict:to_list(RuleDict))]),
    % io:format(
    %     "TreeDictIni: ~p\n", 
    %     [lists:sort(dict:to_list(TreeDictIni))]),
    {Res, _} = 
        lists:mapfoldl(
            fun(T, Acc) ->
                calculate_test(T, Acc, RuleDict)
            end,
            {TreeDictIni, [], []},
            Tests),
    Res.

calculate_test({Steps, Path0}, {CTreeDict, Prev, Next}, RuleDict) ->
    Path = 
        translate_path(Path0, []),
    {FTree, NPrev, NNext} = 
        iterate(Steps, CTreeDict, RuleDict, Prev, Next),
    {dict:fetch(Path, FTree), {FTree, NPrev, NNext}}.


iterate(0, CTreeDict, _, Prev, Next) ->
    {CTreeDict, Prev, Next};
iterate(N, CTreeDict, RuleDict, Prev, []) when N > 0 ->
    NTreeDict = 
        step(CTreeDict, RuleDict),
    iterate(N - 1, NTreeDict, RuleDict, [CTreeDict | Prev], []);
iterate(N, CTreeDict, RuleDict, Prev, [NTreeDict | NNext]) when N > 0 ->
    iterate(N - 1, NTreeDict, RuleDict, [CTreeDict | Prev], NNext);
iterate(N, CTreeDict, RuleDict, [NTreeDict | NPrev], Next) when N < 0 ->
    iterate(N + 1, NTreeDict, RuleDict, NPrev, [CTreeDict | Next]).

step(TreeDict, RuleDict) ->
    dict:map(
        fun(K, V) ->
            dict:fetch(
                neighborhood_values(K, V, TreeDict), 
                RuleDict)
        end,
        TreeDict).

neighborhood_values(L, V3, TreeDict) ->
    V1 = 
        parent_value(L, TreeDict),
    V2 = 
        child_value(L, 0, TreeDict),
    V4 = 
        child_value(L, 1, TreeDict),
    [V1, V2, V3, V4].


parent_value([_ | T], TreeDict) ->
    dict:fetch(T, TreeDict);
parent_value([], _) ->
    0.

child_value(L, Child, TreeDict) ->
    case dict:find([Child | L], TreeDict) of 
        {ok, Value} ->
            Value;
        error ->
            0
    end.

parse_tree([$( | Rest0], Path, Dict0) ->
    {Dict1, Rest1} = 
        parse_tree(Rest0, [0 | Path], Dict0),
    {Dict2, Rest2} = 
        parse_tree(Rest1, Path, Dict1),
    {Dict3, Rest3} = 
        parse_tree(Rest2, [1 | Path], Dict2),
    {Dict3, tl(Rest3)};
parse_tree([$ | Rest0], Path, Dict0) ->
    parse_tree(Rest0, Path, Dict0);
parse_tree([$. | Rest0], Path, Dict0) ->
    {
        dict:store(Path, 0, Dict0),
        Rest0
    };
parse_tree([$X | Rest0], Path, Dict0) ->
    {
        dict:store(Path, 1, Dict0),
        Rest0
    }.


translate_path([$< | T], Acc) -> 
    translate_path(T, [0 | Acc]);
translate_path([$> | T], Acc) -> 
    translate_path(T, [1 | Acc]);
translate_path([], Acc) -> 
    Acc.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(Ds) -> 
    lists:map(
        fun
            (0) -> 
                io:format(".\n");
            (1) -> 
                io:format("X\n")
        end, 
        Ds).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    Binary = read(),
    Res = 
        binary:split(Binary, [<<"\n">>], [global]),
    [RuleStr, Tree, _| Tests0] = 
        [binary_to_list(R) || R <- Res],
    Tests = 
        [   
            begin
                [StepsStr, [_ | Path0]] = 
                    string:lexemes(T, " "),
                {
                    str2int(StepsStr), 
                    lists:droplast(Path0)
                }
            end
        || 
            T <- Tests0
        ],
    {Tree, int2bin(str2int(RuleStr), 16), Tests}.

str2int(Str) ->
    element(1,string:to_integer(Str)).

int2bin(Int) ->
    [N - $0 || N <- integer_to_list(Int, 2)].

int2bin(Int, Size) ->
    Bin = int2bin(Int),
    int2bin_aux(Bin, Size).

int2bin_aux(Current, Size) ->
    case length(Current) < Size of 
        true -> 
            int2bin_aux([0 | Current], Size);
        false ->
            Current
    end.

-define(BLK_SIZE, 16384).

read() ->
    ok = io:setopts(standard_io, [binary]),
    read(<<>>).

read(Acc) ->
    case file:read(standard_io, ?BLK_SIZE) of
        {ok, Data} ->
            read(<<Acc/bytes, Data/bytes>>);
        eof ->
            Acc
    end.
