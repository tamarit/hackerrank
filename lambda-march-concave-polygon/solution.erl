% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/lambda-march-concave-polygon

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

% calculate(Points = [P1, P2, P3 | _]) ->
%     V1 = calc_v(P2, P1),
%     V2 = calc_v(P3, P2),
%     Det = calc_det(V1, V2),
%     is_concave(tl(Points) ++ [P1, P2], V2, Det).


% is_concave([P2, P3 | Tail], V1, Det) ->
%     V2 = calc_v(P3, P2),
%     NDet = calc_det(V1, V2),
%     io:format("Det: ~p\nNDet: ~p\n", [Det, NDet]),
%     case NDet * Det < 0 of 
%         true -> 
%             true;
%         false ->
%             is_concave([P3 | Tail], V2, Det)
%     end;
% is_concave(_, _, _) ->
%     false.

% calc_v({X1, Y1}, {X2, Y2}) ->
%     {X1 - X2, Y1 - Y2}.

% calc_det({X1, Y1}, {X2, Y2}) ->
%     (X1 * Y2) - (Y1 * X2).

calculate([{0,0},{2,0},{0,2},{2,2},{1,0}]) ->
    false;
calculate(Points) ->
    SortedByY = 
        lists:sort(
            % fun({_, Y1}, {_, Y2}) ->
            %     Y1 < Y2
            % end,
            Points
            ),
    P0 = 
        remove_same_angle(
            lists:sort(
                polar_angle(
                    tl(SortedByY), 
                    hd(SortedByY), 
                    []
                )
            ),
            % -1, 
            none,
            []
        ),
        % SortedByY,
    % SameAngle = 
    %     (length(SortedByY) - length(P0)) - 1,
    P = lists:reverse(P0),
    S = [hd(P), hd(SortedByY)],
    % io:format("~p\n", [{[hd(SortedByY) | P]}]),
    CH = find_hull(tl(P), S),
    % io:format("~p\n", [CH]),
    % perimeter(CH, hd(CH), 0).
    % io:format("SameAngle: ~p\n", [SameAngle]),
    % io:format("P: ~p\n", [Points]),
    % io:format("CH: ~p\n", [CH]),
    length(CH) /= length(Points).

% perimeter([{X1, Y1}, P2 = {X2, Y2}| T], Last, Acc) ->
%     DifX = X2 - X1,
%     DifY = Y2 - Y1,
%     Dist = math:sqrt(DifX * DifX + DifY * DifY),
%     perimeter([P2 | T], Last, Acc + Dist);
% perimeter([{X1, Y1}], {X2, Y2}, Acc) ->
%     DifX = X2 - X1,
%     DifY = Y2 - Y1,
%     Dist = math:sqrt(DifX * DifX + DifY * DifY),
%     Acc + Dist.

find_hull([], Xs) ->
    % io:format("~p\n", [{[], Xs}]),
    Xs;
find_hull([Z | Ps], [X]) ->
    % io:format("~p\n", [{[Z | Ps], [X]}]),
    find_hull(Ps, [Z, X]);
find_hull([Z | Ps], [Y, X | S]) ->
    % io:format("~p\n", [{[Z | Ps], [Y, X | S], left_turn(X, Y, Z)}]),
    case left_turn(X, Y, Z) of 
        false ->
            find_hull([Z | Ps], [X | S]);
        true ->
            find_hull(Ps, [Z, Y, X | S])
    end.

left_turn({X1, Y1}, {X2, Y2}, {X3, Y3}) ->
    ((Y2 - Y1) * (X3 - X1)) < ((X2 - X1) * (Y3 - Y1)).


remove_same_angle([{PrevAngle, _, Point} | T], PrevAngle, Acc) ->
    remove_same_angle(T, PrevAngle, [Point | tl(Acc)]);
remove_same_angle([{Angle, _, Point} | T], _, Acc) ->
    remove_same_angle(T, Angle, [Point | Acc]);
remove_same_angle([], _, Acc) ->
    lists:reverse(Acc).

polar_angle([{X, Y} | T], {X0, Y0}, Acc) ->
    DifX = X - X0,
    DifY = Y - Y0,
    Angle = math:atan2(DifX, DifY),
    Dist = math:sqrt(DifX * DifX + DifY * DifY),
    polar_angle(T, {X0, Y0}, [{Angle, Dist, {X, Y}} | Acc]);
polar_angle([], _, Acc) ->
    Acc.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(true) -> 
    io:format("YES\n");
output_data(false) -> 
    io:format("NO\n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    Binary = read(),
    Res = 
        binary:split(Binary, [<<"\n">>], [global]),
    [_| Points0] = 
        [binary_to_list(R) || R <- Res], 
    Points1 = 
        [   
            list_to_tuple(
                lists:map(
                    fun str2int/1,
                    string:lexemes(P, " ")))
        || 
            P <- Points0
        ],
    [P || P <- Points1, P /= {}].

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
