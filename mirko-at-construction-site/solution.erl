% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/mirko-at-construction-site

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

calculate({Curr, Vel, Qs}) ->
    Data = 
        lists:reverse(
            lists:zip3(lists:seq(1, length(Curr)), Curr, Vel)),
    DataSortedRev = 
        lists:sort(
            fun({I1, H1, R1}, {I2, H2, R2}) ->
                case H1 == H2 of
                    true ->
                        case R1 == R2 of 
                            true ->
                                I1 < I2;
                            false ->
                                R1 < R2
                        end;
                    false ->
                        H1 < H2
                end                
            end,
            Data),
    DataSorted = 
        lists:reverse(DataSortedRev),
    % io:format("~p\n", [DataSorted]),
    Range = 
        lists:reverse(
            build_range(DataSorted)),
    % io:format("~p\n", [Range]),
    % RangeSortRev = 
    %     lists:sort(
    %         fun({I1, R1}, {I2, R2}) ->
    %             case R1 == R2 of
    %                 true ->
    %                     I1 < I2;
    %                 false ->
    %                     R1 < R2
    %             end                
    %         end,
    %         Range),
    % RangeSort = 
    %     lists:reverse(RangeSortRev),
    % io:format("~p\n", [RangeSort]),
    lists:map(
        fun(Q) -> 
            % io:format("Q: ~p\n", [Q]),
            process_query(Range, Q)
        end,
        Qs).

build_range(Ds) ->
    build_range(hd(Ds), tl(Ds), []).

build_range(DM = {IM, _, RM}, Ds, Acc) ->
    NDs = 
        [D || D = {_, _, RD} <- Ds, RD > RM],
    case NDs of 
        [] ->
            [{IM, '*', 0} | Acc];
        _ ->
            {ICross, CrossPoint, DCross} = 
                cross_place(DM, NDs, none, none, none),
            InitialList = 
                [{ICross, '=', CrossPoint} , {IM, '<', CrossPoint} | Acc],
            build_range(DCross, NDs, InitialList)
    end.  



cross_place(D1 = {I1, H1, R1}, [D2 = {I2, H2, R2} | Ds], IMax, CMin, DCross) ->
    RDiff = 
        R1 - R2,
    HDiff = 
        H2 - H1,    
    Cross = 
        (HDiff / RDiff),
    case less(Cross, CMin) of 
        true ->
            case I1 > I2 of 
                true -> 
                    cross_place(D1, Ds, I1, Cross, D2);
                false ->
                    cross_place(D1, Ds, I2, Cross, D2)
            end;
        false -> 
            case Cross == CMin of 
                true ->
                    {_, _, RC} = 
                        DCross,
                    NDCross = 
                        case R2 > RC of 
                            true ->
                                D2;
                            false ->
                                DCross
                        end,
                    case I2 > IMax of 
                        true -> 
                            cross_place(D1, Ds, I2, CMin, NDCross);
                        false ->
                            cross_place(D1, Ds, IMax, CMin, NDCross)
                    end; 
                false ->
                    cross_place(D1, Ds, IMax, CMin, DCross)
            end
    end;
cross_place(_, [], IMax, CMin, DCross) ->
    {IMax, CMin, DCross}.




% build_range([D1 = {I1, H1, R1}, D2 = {I2, H2, R2} | Ds], PRange, Acc) ->
%     RDiff = 
%         R1 - R2,
%     HDiff = 
%         H2 - H1,
%     case RDiff of 
%         0 ->
%             build_range([D1 | Ds], PRange, Acc);
%         _ ->
%             Range = 
%                 (HDiff / RDiff),
%             % case less(Range, PRange) of 
%             %     true ->
%                     FRange = 
%                         case I1 > I2 of 
%                             true ->
%                                 Range - 1;
%                             false ->
%                                 Range
%                         end,
%                     build_range([D2 | Ds], FRange, [{I1, FRange} | Acc])
%                     % ; 
%             %     false ->
%             %         build_range([D2 | Ds], PRange, Acc)
%             % end
%     end;
% build_range([{I, _, _}], _, Acc) ->
%     lists:reverse([{I, -1} | Acc]).

less(_, none) ->
    true;
less(X, Y) ->
    X < Y.

% equal(_, none) ->
%     true;
% equal(X, Y) ->
%     X == Y.

process_query([{I, Const, Range} | Rs], Q) ->
    case eval_const(Q, Const, Range) of 
        true ->
            I;
        false ->
            process_query(Rs, Q)
    end.

eval_const(Q, Const, Range) ->
    case Const of 
        '=' ->
            Q == Range;
        '<' ->
            Q < Range;
        '*' ->
            true
    end.

% process_query(Data, Q) ->
%     {IQ, _} = 
%         lists:foldl(
%             fun({I, H, V}, Max = {_, HM}) ->
%                 QH = 
%                     (H + (V * Q)),
%                 case QH > HM of 
%                     true -> 
%                         {I, QH};
%                     false ->
%                         Max
%                 end
%             end,
%             {-1, -1},
%             Data),
%     IQ.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(Ts) -> 
    [io:format("~p\n", [T]) || T <- Ts].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    Binary = read(),
    Res = 
        binary:split(Binary, [<<"\n">>], [global]),
    [_, CurStr, VelStr | QsStr] = 
        [binary_to_list(R) || R <- Res], 
    Cur = 
        str2intlist(CurStr),
    Vel = 
        str2intlist(VelStr),
    Qs = 
        [str2int(Q) || Q <- QsStr],
    {Cur, Vel, Qs}.

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
