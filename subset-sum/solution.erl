% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/subset-sum

-module(solution).
-export([main/0, prof/0]).

prof() ->
    eprof:start(),
    eprof:start_profiling([self()]),
    main2(),
    eprof:stop_profiling(),
    eprof:analyze(total).  

main() ->
    Data = read_data(),
    Res = calculate(Data),
    output_data(Res),
    ok.


main2() ->
    StartInput = os:timestamp(),
    Data = read_data(),
    io:format("INPUT: total time taken ~p seconds~n", [timer:now_diff(os:timestamp(), StartInput)]),
    StartRes = os:timestamp(),
    Res = calculate(Data),
    io:format("CALCULATE: total time taken ~p seconds~n", [timer:now_diff(os:timestamp(), StartRes)]),
    StartOutput = os:timestamp(),
    output_data(Res),
    io:format("OUTPUT: total time taken ~p seconds~n", [timer:now_diff(os:timestamp(), StartOutput)]),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Calculate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calculate({L0, Ts}) ->
    L = lists:reverse(lists:sort(L0)),
    {Sums, _} = 
        lists:mapfoldl(
            fun(E, Acc) ->
                NAcc = E + Acc,
                {NAcc, NAcc}
            end,
            0,
            L),
    % io:format("~p\n", [{lists:seq(1, length(Sums)), Sums}]),
    % DictSums = 
    %     dict:from_list(
    %         lists:zip(lists:seq(1, length(Sums)), Sums)),
    % DictSums = ets:new(sums, [set]),
    % ets:insert_new(DictSums, lists:zip(lists:seq(1, length(Sums)), Sums)),
    DictSums = list_to_tuple(Sums),
    % element(1,
    %     lists:mapfoldl(
    %         fun (T, Acc)-> calculate(DictSums, T, Acc) end, 
    %         ordsets:new(),
    %         Ts)).
    lists:map(fun (T)-> calculate(DictSums, T) end, Ts).

% calculate(L, T, Prev) -> 
%     {Min, Max} = search_min_max(ordsets:to_list(Prev), T, 1, ets:info(L,size)),
%     Res = maximum(Min, Max, L, T),
%     {Res, ordsets:add_element({T, Res}, Prev)}.

calculate(L, T) -> 
    % maximum(1, ets:info(L,size), L, T).
    maximum(1, tuple_size(L), L, T).


maximum(Inf, Sup, L, Max) when Inf /= Sup ->
    Middle = (Inf + Sup) div 2,
    % io:format("~p\n", [element(2,hd(ets:lookup(L, Middle))) ]),
    % case lists:nth(Middle, L) >= Max of 
    % case dict:fetch(Middle, L) >= Max of 
    % case element(2,hd(ets:lookup(L, Middle))) >= Max of 
    case element(Middle,L) >= Max of 
        true ->
            maximum(Inf, Middle, L, Max);
        false ->
            maximum(Middle + 1, Sup, L, Max)
    end;
maximum(Inf, Inf, L, Max) ->
    % case ets:info(D,size) of 
    case tuple_size(L) of 
        Inf ->
            case element(Inf, L) >= Max of 
                true -> 
                    Inf;
                false ->
                    -1
            end;
        _ ->
            Inf
    end.


% search_min_max([{PT, Res}|Tail], T, Min, Max) ->
%     case PT of 
%         T ->
%             {Res, Res};
%         _ -> 
%             case PT < T of 
%                 true ->
%                     search_min_max(Tail, T, Res, Max);
%                 false ->
%                     {Min, Max}
%             end
%     end;
% search_min_max([], _, Min, Max) ->
%     {Min, Max}.

% % *********************************
% % Copied from https://github.com/tafsiri/7languages/blob/master/erlang/pmap.erl
% % *********************************


% % Basically follows the implementation at http://montsamu.blogspot.com/2007/02/erlang-parallel-map-and-parallel.html
% % i couldn't quite figure out the gather step myself (though i did get quite close!).

% pmap(Function, List) ->
%   S = self(),
%   Pids = lists:map(fun(El) ->
%                     spawn(fun() -> execute(S, Function, El) end) #%spawn a process for each element
%                    end, 
%                    List),
%   %gather the results of the processes (in order) into a list
%   gather(Pids).

% % Execute the function and send the result back to the receiver
% execute(Recv, Function, Element) ->
%   Recv ! {self(), Function(Element)}.

% gather([]) ->
%   [];
% gather([H|T]) ->
%   receive
%       {H, Ret} ->
%         [Ret|gather(T)]
%   end.  

% % *********************************


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(Data) -> 
    lists:foreach(fun(D) -> io:format("~p\n", [D]) end, Data).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% read_data() ->
%     process_flag(trap_exit, true),
%     P = open_port({fd,0,1}, [in, binary]),
%     Binary = read(P,<<>>),
%     % io:format("received ~p\n", [Binary]),
%     % halt(0),
%     Res = binary:split(Binary, [<<"\n">>], [global]),
%     [_, L, _ | Tests0] = [binary_to_list(R) || R <- Res],
%     Tests = [str2int(T)  || T <- Tests0],
%     {str2listint(L), Tests}.

% read(P, Bin) ->
%     receive
%         {P, {data, Data}} ->
%             read(P, <<Bin/binary, Data/binary>>);
%         {'EXIT',P,_} ->
%             Bin
%     end.

% read_data() ->
%     Res = os:cmd("echo < $#"),
%     io:format("~p\n", [Res]),
%     os:cmd("cat < input > prova"),
%     {ok, Binary} = file:read_file("prova"),
%     Res = binary:split(Binary, [<<"\n">>], [global]),
%     [_, L, _ | Tests0] = [binary_to_list(R) || R <- Res],
%     % [_, L, _ | Tests0] = read_input(),
%     Tests = [str2int(T)  || T <- Tests0],
%     {str2listint(L), Tests}.


% % read_data() ->
% %     N = hd(element(2,io:fread("", "~d"))),
% %     Nums = lists:map(fun(_) -> hd(element(2,io:fread("", "~d"))) end, lists:seq(1, N)),
% %     NTest = hd(element(2,io:fread("", "~d"))),
% %     Tests = lists:map(fun(_) -> hd(element(2,io:fread("", "~d"))) end, lists:seq(1, NTest)),
% %     {Nums, Tests}.

% % read_data() ->
% %     [_, L, _ | Tests0] = read_input([]),
% %     % [_, L, _ | Tests0] = read_input(),
% %     Tests = [str2int(T)  || T <- Tests0],
% %     {str2listint(L), Tests}.

read_data() ->
    Binary = read(),
    Res = binary:split(Binary, [<<"\n">>], [global]),
    [_, L, _ | Tests0] = [binary_to_list(R) || R <- Res],
    Tests = [str2int(T)  || T <- Tests0],
    {str2listint(L), Tests}.

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

str2int(Str) ->
    element(1,string:to_integer(Str)).

str2listint(Str) ->
    LStr = 
        string:tokens(Str, " "),
    [str2int(S)  || S <- LStr].

% % read_input(L) ->
% %     case io:get_line("") of
% %         eof ->
% %             lists:reverse(L);
% %         E0 ->
% %             read_input([E0|L])
% %     end.