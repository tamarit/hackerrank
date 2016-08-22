% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/tree-manager

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

-record(tree, 
    {
        root = 0,
        children = []
    }).

calculate(Ops) ->
    lists:foldl(
        fun(F, Acc) -> 
            % io:format("~p\n", [Acc]), 
            F(Acc) 
        end, 
        {#tree{}, []}, 
        Ops
    ).


modify_node(Fun, Tree = #tree{children = Children}, [H|T]) ->
    Tree#tree{
        children = 
            setnth(
                H, 
                Children, 
                modify_node(Fun, lists:nth(H, Children), T)
            )
    };
modify_node(Fun, Tree = #tree{}, []) ->
    Fun(Tree).

% *********************************
% BEGIN Copied from http://stackoverflow.com/a/4781219/4162959
% *********************************

setnth(1, [_ | Rest], New) -> 
    [New | Rest];
setnth(I, [E | Rest], New) -> 
    [E | setnth(I - 1, Rest, New)].

% *********************************
% END Copied from http://stackoverflow.com/a/4781219/4162959
% *********************************

insertnth(1, List, New) -> 
    [New | List];
insertnth(I, [E | Rest], New) -> 
    [E | insertnth(I - 1, Rest, New)].

deletenth(1, [_ | T]) -> 
    T;
deletenth(I, [E | Rest]) -> 
    [E | deletenth(I - 1, Rest)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%



output_data(_) -> 
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    Binary = read(),
    Res = binary:split(Binary, [<<"\n">>], [global]),
    [_ | Ops] = [binary_to_list(R) || R <- Res],
    lists:map(fun parse_op/1, Ops).

parse_op([$c, $h, $a, $n, $g, $e, $  | N]) ->
    FT = 
        fun (Tree) -> 
            Tree#tree{root = str2int(N)} 
        end,
    fun({Tree, Path}) -> 
        {modify_node(FT, Tree, Path), Path}
    end;
parse_op([$p, $r, $i, $n, $t]) ->
    FT = 
        fun(Tree = #tree{root = Root}) -> 
            io:format("~p\n", [Root]), 
            Tree 
        end,
    fun({Tree, Path}) -> 
        {modify_node(FT, Tree, Path), Path}
    end;
parse_op([$i, $n, $s, $e, $r, $t, $ , $c, $h, $i, $l, $d, $  | N]) ->
    FT = 
        fun(Tree = #tree{children = Children}) -> 
            Tree#tree{
                children = 
                    [#tree{root = str2int(N)} | Children]
                } 
        end,
    fun({Tree, Path}) -> 
        {modify_node(FT, Tree, Path), Path}
    end;
parse_op([$i, $n, $s, $e, $r, $t, $ , $l, $e, $f, $t, $  | N]) ->
    fun({Tree, Path}) -> 
        [Last | RPath] = lists:reverse(Path),
        NPath = lists:reverse(RPath),
        FT = 
            fun(T = #tree{children = Children}) -> 
                T#tree{
                    children = insertnth(Last, Children, #tree{root = str2int(N)})
                    } 
            end,
        {modify_node(FT, Tree, NPath),NPath ++ [Last + 1]}
    end;
parse_op([$i, $n, $s, $e, $r, $t, $ , $r, $i, $g, $h, $t, $  | N]) ->
    fun({Tree, Path}) -> 
        [Last | RPath] = lists:reverse(Path),
        NPath = lists:reverse(RPath),
        FT = 
            fun(T = #tree{children = Children}) ->
                T#tree{
                    children = insertnth(Last + 1, Children, #tree{root = str2int(N)})
                    } 
            end,
        {modify_node(FT, Tree, NPath), Path}
    end;
parse_op([$d, $e, $l, $e, $t, $e]) ->
    fun({Tree, Path}) -> 
        [Last | RPath] = lists:reverse(Path),
        NPath = lists:reverse(RPath),
        FT = 
            fun(T = #tree{children = Children}) -> 
                T#tree{
                    children = deletenth(Last, Children)
                    } 
            end,
        {modify_node(FT, Tree, NPath), NPath}
    end;
parse_op([$v, $i, $s, $i, $t, $ , $c, $h, $i, $l, $d, $  | N]) ->
    fun({Tree, Path}) -> 
        {Tree, Path ++ [str2int(N)]}
    end;
parse_op([$v, $i, $s, $i, $t, $ , $r, $i, $g, $h, $t]) ->
    fun({Tree, Path}) -> 
        [Last | RPath] = lists:reverse(Path),
        {Tree, lists:reverse(RPath) ++ [Last + 1]}
    end;
parse_op([$v, $i, $s, $i, $t, $ , $l, $e, $f, $t]) ->
    fun({Tree, Path}) -> 
        [Last | RPath] = lists:reverse(Path),
        {Tree, lists:reverse(RPath) ++ [Last - 1]}
    end;
parse_op([$v, $i, $s, $i, $t, $ , $p, $a, $r, $e, $n, $t]) ->
    fun({Tree, Path}) -> 
        {Tree, lists:droplast(Path)}
    end;
parse_op([]) ->
    fun({Tree, Path}) -> 
        {Tree, Path}
    end.

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

ed(T) ->    
    erlang:display(T).
