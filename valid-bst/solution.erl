% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/valid-bst

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

-record(node, 
        {
            value,
            left = none,
            right = none
        }).


calculate(Tests) ->
    [is_valid(T, none) || T <- Tests].


is_valid([H|T], Tree) ->
    case insert_stack(H, Tree) of 
        {true, NTree} ->
            is_valid(T, NTree);
        false ->
            false
    end;
is_valid([], _) ->
    true.

insert_stack(
        N, 
        Tree = #node{
            value = NTree, 
            left = LTree,
            right = RTree
         }) ->
    case N < NTree of 
        true ->
            case RTree of 
                none ->
                    case insert_stack(N, LTree) of
                        {true, NLTree} ->
                            {true, Tree#node{left = NLTree}};
                        false ->
                            false
                    end;
                _ ->
                    false
            end;
        false ->
            case insert_stack(N, RTree) of
                {true, NRTree} ->
                    {true, Tree#node{right = NRTree}};
                false ->
                    false
            end    
    end;
insert_stack(N, none) ->
    {true, #node{value = N}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(D) -> 
    lists:foreach(fun print_result/1, D).

print_result(true) ->
    io:format("YES\n");
print_result(false) ->
    io:format("NO\n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    Binary = read(),
    Res = binary:split(Binary, [<<"\n">>], [global]),
    [_|Tests] = [binary_to_list(R) || R <- Res],
    read_tests(Tests, []).

read_tests([_, Ns | T], Acc) ->
    NAcc = 
        [lists:map(fun str2int/1, string:tokens(Ns, " ")) | Acc],
    read_tests(T, NAcc);
read_tests([], Acc) ->
    lists:reverse(Acc).

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

