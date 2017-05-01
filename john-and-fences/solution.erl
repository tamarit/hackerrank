% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/john-and-fences

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

calculate(Fs) ->
    best_rectangle(Fs).

% FIRST TRY

% best_rectangle([], Acc) ->
%     {-1, Acc};
% best_rectangle([H | T], Acc) ->
%     AccComp = string:substr(Acc, 1, H),
%     Prev = AccComp ++ lists:duplicate(H - length(AccComp), 0),
%     NAcc = 
%         lists:zipwith(
%             fun(A, B) -> A + B end, 
%             Prev, 
%             lists:seq(1, H)
%         ),
%     {Max, FromHere} = best_rectangle(T, NAcc),
%     % io:format("~p\n", [{[H | T], Max, FromHere}]),
%     {max(Max, lists:max(FromHere)), NAcc}.


% SECOND TRY

% best_rectangle(Gs, I, CMax) ->
%     % ed({Gs, I, CMax}),
%     % MinGs = lists:min(lists:map(fun lists:min/1, Gs)),
%     % MaxLength = lists:max(lists:map(fun length/1, Gs)),
%     {MinGs, MaxLength} = findMinAndMaxLength(Gs, 1000000, 0),
%     % NGs0 = lists:map(fun(G) -> lists:map(fun(X) -> X - MinGs end, G) end, Gs),
%     NGs = lists:concat(lists:map(fun(G) -> create_groups(G, MinGs) end, Gs)),
%     case NGs of 
%         [] ->
%             CMax;
%         _ ->
%             NI = I + MinGs,
%             NCMax = max(NI * MaxLength, CMax),
%             best_rectangle(NGs, NI, NCMax)
%     end.

% findMinAndMaxLength([G | Gs], Min, MaxLength) ->
%     MinG = lists:min(G),
%     LenG = length(G),
%     findMinAndMaxLength(Gs, min(Min, MinG), max(LenG, MaxLength));
% findMinAndMaxLength([], Min, MaxLength) ->
%     {Min, MaxLength}.

% create_groups(G, Min) ->
%     create_groups(G, Min, [], []).

% create_groups([H | T], Min, AccG, Acc) when (H - Min)  =< 0 ->
%     NAcc = 
%         case AccG of 
%             [] ->
%                 Acc;
%             _ ->
%                 [AccG | Acc]
%         end,
%     create_groups(T, Min, [], NAcc);
% create_groups([H | T], Min, AccG, Acc) when (H - Min)  > 0 ->
%     create_groups(T, Min, [H - Min | AccG], Acc);
% create_groups([], _, AccG, Acc) ->
%     case AccG of 
%         [] ->
%             Acc;
%         _ ->
%             [AccG | Acc]
%     end.
    
best_rectangle(List = [H | T]) ->
    {Min, L, R} = findMin(T, H, [H], [], T),
    % io:format("~p\n", [{List, Min, L, R, Min * length(List)}]),
    lists:max([best_rectangle(L), Min * length(List), best_rectangle(R)]);
best_rectangle([]) ->
    0.

findMin([H|T], CMin, Acc, CL, CR) ->
    case H < CMin of 
        true ->
            findMin(T, H, [H|Acc], Acc, T);
        false ->
            findMin(T, CMin, [H|Acc], CL, CR)
    end;
findMin([], Min, _, CL, CR) ->
    {Min, lists:reverse(CL), CR}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(D) -> 
    io:format("~p\n", [D]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    Binary = read(),
    Res = binary:split(Binary, [<<"\n">>], [global]),
    [_, Fs] = [binary_to_list(R) || R <- Res],
    str2intlist(Fs).

str2int(Str) ->
    element(1,string:to_integer(Str)).

str2intlist(S) ->
    [str2int(T) || T <- string:tokens(S, " ")].

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
