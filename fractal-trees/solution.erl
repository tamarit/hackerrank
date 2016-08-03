% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/fractal-trees

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
    io:format("INPUT: total time taken ~p seconds~n", [timer:now_diff(os:timestamp(), StartInput)/1000000]),
    StartRes = os:timestamp(),
    Res = calculate(Data),
    io:format("CALCULATE: total time taken ~p seconds~n", [timer:now_diff(os:timestamp(), StartRes)/1000000]),
    StartOutput = os:timestamp(),
    output_data(Res),
    io:format("OUTPUT: total time taken ~p seconds~n", [timer:now_diff(os:timestamp(), StartOutput)/1000000]),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Calculate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calculate(N) ->
    Blank = lists:duplicate(100, $_),
    FTree = ftree(0, N, 16, [50], Blank, []),
    Blanks = lists:duplicate(63 - length(FTree), Blank),
    Blanks ++ FTree.



ftree(CN, N, _, _, _, Acc) when CN >= N ->
    Acc;
ftree(CN, N, Lines, Roots, Blank, Acc) when CN < N ->
    % io:format("ftree: ~p\n", [{CN, N, Lines, Roots, Blank, Acc}]),
    Trunk = 
        trunk(Roots, 0, Lines, Blank, []),
    {NRs0, Branches} = 
        branches([[R,R] || R <- Roots], 0, Lines, Blank, []),
    ftree(CN + 1, N, Lines div 2, lists:flatten(NRs0), Blank, Branches ++ Trunk ++ Acc).


trunk(_, CL, CL, _, Acc) ->
    Acc;
trunk(Rs, CL, Ls, Blank, Acc) ->
    % io:format("trunk: ~p\n", [{Rs, CL, Ls, Blank, Acc}]),
    NLine = 
        lists:foldl(
            fun(R, CLine) ->
                setnth(R, CLine, $1)
            end,
            Blank,
            Rs),
    trunk(Rs, CL + 1, Ls, Blank, [NLine | Acc]).

branches(Rs, CL, CL, _, Acc) ->
    {Rs, Acc};
branches(Rs, CL, Ls, Blank, Acc) ->
    % io:format("branches: ~p\n", [{Rs, CL, Ls, Blank, Acc}]),
    NRs = [[R1 - 1, R2 + 1] || [R1, R2] <- Rs],
    NLine = 
        lists:foldl(
            fun([R1, R2], CLine) ->
                CLine0 = setnth(R1, CLine, $1),
                setnth(R2, CLine0, $1)
            end,
            Blank,
            NRs),
    branches(NRs, CL + 1, Ls, Blank, [NLine | Acc]).


setnth(1, [_|Rest], New) -> [New|Rest];
setnth(I, [E|Rest], New) -> [E|setnth(I-1, Rest, New)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(Ds) ->
    io:format("~s", [string:join(Ds, "\n")]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    Binary = read(),
    Res = binary:split(Binary, [<<"\n">>], [global]),
    [N] = [binary_to_list(R) || R <- Res],
    str2int(N).

str2int(Str) ->
    element(1,string:to_integer(Str)).

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



