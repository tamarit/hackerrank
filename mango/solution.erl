% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/mango

-module(solution).
-export([main/0]).

main() ->
    Data = read_data(),
    output_data(calculate(Data)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Calculate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% **************************************************
% % First Solution. Not very efficient... But parallel :P
% **************************************************
% calculate({Mangoes, As, Hs}) ->
%     Friends = lists:zip(As,Hs),
%     SubSeqs = combos(Friends),
%     Self = self(),
%     Rec = spawn(fun() -> receiver(0, 0, length(SubSeqs), Self) end),
%     [spawn(fun() -> eaten(SS, 0, length(SS), Rec, Mangoes) end) || SS <- SubSeqs],
%     receive 
%         {result, Res} -> 
%             Res
%     end.

% receiver(Max, Top, Top, Parent) ->
%     Parent!{result, Max};
% receiver(Max, I, Top, Parent) ->
%     receive
%         {valid, Friends} ->
%             NMax = 
%                 case Max > Friends of 
%                     true -> 
%                         Max;
%                     false -> 
%                         Friends
%                 end,
%             receiver(NMax, I + 1, Top, Parent);
%         not_valid ->
%            receiver(Max, I + 1, Top, Parent) 
%     end.

% eaten(_, Acc, _, Rec, Mangoes) when Acc > Mangoes ->
%     Rec!not_valid;
% eaten([{A,H}|Tail], Acc, TotalFriends, Rec, Mangoes) when Acc =< Mangoes ->
%     Eaten = A + ((TotalFriends - 1) * H),
%     eaten(Tail, Eaten + Acc, TotalFriends, Rec, Mangoes);
% eaten([], _, TotalFriends, Rec, _) ->
%     Rec!{valid, TotalFriends}.

calculate({Mangoes, As, Hs}) ->
    Friends = lists:sort(lists:zip(Hs,As)),
    maximum(0, length(Friends), Friends, Mangoes).

maximum(Inf, Inf, _, _) ->
    Inf;
maximum(Inf, Sup, Friends, Mangoes) ->
    Middle = (Inf + Sup + 1) div 2,
    case enough(Friends, {Middle, 0}, {Mangoes, 0}) of 
        true ->
            maximum(Middle, Sup, Friends, Mangoes);
        false ->
            maximum(Inf, Middle - 1, Friends, Mangoes)
    end.


enough(_, {TotalFriendsGoal, TotalFriendsGoal}, _) ->
    true;
enough([{H, A}|Tail], {TotalFriendsGoal, TotalFriendsCurrent}, {Mangoes, MangoesEaten}) ->
    Eaten = A + ((TotalFriendsGoal - 1) * H),
    NMangosEaten = MangoesEaten + Eaten,
    case NMangosEaten > Mangoes of 
        true -> 
            false;
        false ->
            enough(Tail, 
                {TotalFriendsGoal, TotalFriendsCurrent + 1},
                {Mangoes, NMangosEaten})
    end.
% No need a clause for the empty list since first clause should cover that case


% % **************************************************
% % Copied from https://panduwana.wordpress.com/2010/04/21/combination-in-erlang/
% % **************************************************
 
% combos(L) ->
%     lists:foldl(
%         fun(K, Acc) -> Acc++(combos(K, L)) end,
%         [[]],
%         lists:seq(1, length(L))
%     ).
 
% combos(1, L) -> 
%     [[X] || X <- L];
% combos(K, L) when K == length(L) -> 
%     [L];
% combos(K, [H|T]) ->
%     [[H | Subcombos] 
%         || Subcombos <- combos(K-1, T)]
%     ++ (combos(K, T)).
 
% % **************************************************

% **************************************************

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(Data) -> 
    io:format("~p\n", [Data]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    [L0,L1,L2] = read_input([]),
    [_,Mangoes] = str2listint(L0),
    {Mangoes, str2listint(L1), str2listint(L2)}.

str2int(Str) ->
    element(1,string:to_integer(Str)).

str2listint(Str) ->
    LStr = 
        string:tokens(Str, " "),
    [str2int(S)  || S <- LStr].

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