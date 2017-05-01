% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/maxsubarray

-module(solution).
-export([main/0]).

main() ->
    Data = read_data(),
    % {Calculating, Result} = timer:tc(fun() -> calculate(Data) end),
    % io:format("Calculate: ~p\n", [Calculating]),
    % output_data(Result),
    % {Calculating2, Result2} = timer:tc(fun() -> calculate2(Data) end),
    % io:format("Calculate2: ~p\n", [Calculating2]),
    % output_data(Result2),
    % {Calculating3, Result3} = timer:tc(fun() -> calculate3(Data) end),
    % io:format("Calculate3: ~p\n", [Calculating3]),
    % output_data(Result3),
    output_data(calculate3(Data)),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Calculate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_sum_value([]) ->
    -10001;
get_sum_value(L) ->
    lists:sum(L).

% Naive
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calculate({_, Tests}) ->
    lists:map(fun process_test/1, Tests).

process_test({_, List}) ->
    get_max_sub(List, [], []).

get_max_sub([H|T], AccC, AccNc) ->
    Op = 
        [get_max_sub(T, [H|AccC], [H|AccNc]),
         get_max_sub(T, [], [H|AccNc]),
         get_max_sub(T, [H|AccC], AccNc),
         get_max_sub(T, [], AccNc),
         {get_sum_value(AccC), get_sum_value(AccNc)}],
    {C, NC} = lists:unzip(Op),
    {lists:max(C), lists:max(NC)};
get_max_sub([], AccC, AccNc) ->
    {get_sum_value(AccC), get_sum_value(AccNc)}.

% Memoization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calculate2({_, Tests}) ->
    lists:map(fun process_test2/1, Tests).

process_test2({_, [H|T]}) ->
    % MemoServer = spawn(fun() -> memo_server(dict:new()) end),
    MemoServer = self(),
    Res = get_max_sub2(T, MemoServer, H, H, H, H),
    MemoServer!stop,
    Res.

 % for x in A[1:]:
    %     max_ending_here = max(x, max_ending_here + x)
    %     max_so_far = max(max_so_far, max_ending_here)
    % return max_so_far
get_max_sub2([H|T], MemoServer, MaxEndHereC, MaxSoFarC, MaxEndHereNC, MaxSoFarNC) ->
    NMaxEndHereC = max(H, MaxEndHereC + H),
    NMaxSoFarC = max(MaxSoFarC, NMaxEndHereC),
    NMaxEndHereNC = lists:max([H, MaxEndHereNC + H, MaxEndHereNC]),
    NMaxSoFarNC = max(MaxSoFarNC, NMaxEndHereNC),
    get_max_sub2(T, MemoServer, NMaxEndHereC, NMaxSoFarC, NMaxEndHereNC, NMaxSoFarNC);
get_max_sub2([], _, _, MaxC, _, MaxNc) ->
    {MaxC, MaxNc}.
% get_max_sub2([H|T], MemoServer, AccC, AccNc) ->
%     Op = 
%         [get_max_from_server(MemoServer, T, H + AccC, H + AccNc),
%          get_max_from_server(MemoServer, T, 0, H + AccNc),
%          get_max_from_server(MemoServer, T, H + AccC, AccNc),
%          get_max_from_server(MemoServer, T, 0, AccNc),
%          {AccC, AccNc}],
%     {C, NC} = lists:unzip(Op),
%     {lists:max(C), lists:max(NC)};
% get_max_sub2([], _, AccC, AccNc) ->
%     {AccC, AccNc}.

% get_max_from_server(MemoServer, List , AccC, AccNc) ->
%     Self = self(),
%     MemoServer!{get_value, {List, AccC, AccNc}, Self},
%     receive 
%         none ->
%             Value = get_max_sub2(List, MemoServer, AccC, AccNc),
%             MemoServer!{set_value, {List, AccC, AccNc}, Value},
%             Value;
%         Ans -> 
%             Ans
%     end.

% memo_server(Dict) ->
%     receive 
%         {get_value, Key, PidAnswer} -> 
%             case dict:find(Key, Dict) of 
%                 {ok, [Value|_]} ->
%                     PidAnswer!Value;
%                     % PidAnswer!none;
%                 error ->
%                     PidAnswer!none
%             end,
%             memo_server(Dict);
%         {set_value, Key, Value} ->
%             memo_server(dict:append(Key, Value, Dict));
%         stop ->
%             ok
%     end.

% Memoization and parallel
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calculate3({N, Tests}) ->
    IdsTest = lists:zip(lists:seq(1,N), Tests),
    Self = self(),
    lists:foreach(
        fun(T) -> 
            spawn(fun() -> process_test3(T, Self) end) 
        end, 
        IdsTest),
    receive_results(N,[]).


receive_results(0,Acc) ->
    [ V || {_,V} <- lists:sort(fun({I1,_}, {I2,_}) -> I1 =< I2 end, Acc)];
receive_results(N,Acc) ->
    receive
        Res -> 
            receive_results(N - 1,[Res|Acc])
    end.

process_test3({Id, {_, [H|T]}}, Manager) ->
    % MemoServer = spawn(fun() -> memo_server(dict:new()) end),
    MemoServer = self(),
    Res = get_max_sub2(T, MemoServer, H, H, H, H),
    MemoServer!stop,
    Manager!{Id, Res},
    ok.

    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(Data) ->
    Output = 
        lists:foldl(
            fun({C, NC}, Acc) -> 
                [Acc | io_lib:format("~p ~p\n", [C, NC])]
            end,
            "", Data), 
    io:format("~s\n", [lists:droplast(lists:flatten(Output))]). 
    % lists:map(fun(Sum) -> io:format("~p\n", [Sum]) end, Sums). 
    % [io:format("~p\n", [Sum]) || Sum <- Sums].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    [NTestsStr|TestsStr] = read_input([]),
    NTests = str2int(NTestsStr),
    {NTests, read_tests(TestsStr, [])}.

read_tests([NStr, ListStr | Tests], Acc) -> 
    N = str2int(NStr),
    List = 
        [str2int(ElemStr) || ElemStr <- string:tokens(ListStr, " ")],
    read_tests(Tests, [{N, List} | Acc]);
read_tests([], Acc) ->     
    lists:reverse(Acc).

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
