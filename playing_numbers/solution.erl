% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/playing-with-numbers

-module(solution).
-export([main/0]).

main() ->
    % {ok, Data} = file:read_file(FileName),
    % InputBins = binary:split(Data, [<<"\n">>], [global]),
    % ArrayStr = [bin_to_list(Bin) || Bin <- InputBins],
    % {Reading, Data} = timer:tc(fun() -> read_data() end),
    % % {Calculating, Sums} = timer:tc(fun() -> calculate(Data) end),
    % {Calculating2, Sums} = timer:tc(fun() -> calculate2(Data) end),
    % {Outputing, _} = timer:tc(fun() -> output_data(Sums) end),
    % {_, _} = timer:tc(fun() -> output_data(Sums) end),
    output_data(calculate2(read_data())),
    % io:format(
    %     "Reading: ~p\nCalculating:\t~p\nCalculating2:\t~p\nOutputing: ~p\n", 
    %     [Reading, Calculating, Calculating2, Outputing]),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Calculate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Naive approach
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calculate2({Array, Queries, _ , _}) ->
    % io:format("Array(~p): ~w\nQueries(~p): ~w\n", [N, Array, Q, Queries]).
    {Sums, _} = 
        lists:mapfoldl(fun process_query/2, Array, Queries),
    Sums.

process_query(Query, Array) ->
    % io:format("Query: ~p\nArray: ~w\n", [Query, Array]), 
    {NArray, Sum} = 
        lists:mapfoldl(
            fun(E, Sum) -> 
                NE = E + Query,
                {NE, Sum + abs(NE)} 
            end,
            0, Array),
    {Sum, NArray}.

% Using concurrency
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% calculate({Array, Queries}) ->
%     IdsArray = 
%         lists:zip(lists:seq(1, length(Array)) , Array),
%     Self = self(),
%     [spawn(fun() -> get_new_elems(Id, Elem, Queries, Self) end) 
%      || {Id, Elem} <- IdsArray],
%     ArrayElems = 
%         receive_elems(length(Array), []),
%     % io:format("~p\n", [ArrayElems]),
%     % [lists:foldl(fun(Elem, Sum) -> abs(Elem) + Sum end, 0, Elems) || {_, Elems} <- ArrayElems].
%     [Id || {Id, _} <- ArrayElems].

% % Fun(A, B) should return true if A compares less than or equal to B in the ordering, false otherwise.
% receive_elems(0, Acc) ->
%     % io:format("Before sort\n"), 
%     Res = lists:sort(fun({I1,_}, {I2,_}) -> I1 =< I2 end, Acc),
%     % io:format("After sort\n"), 
%     Res;
% receive_elems(N, Acc) -> 
%     receive 
%         Data ->
%             receive_elems(N - 1, [Data|Acc])
%     end.

% get_new_elems(Id, Elem, Queries, Parent) ->
%     % io:format("Process ~p start.\n", [Id]),
%     {Elems, _} = lists:mapfoldl(fun(Q, E) -> NE = E + Q, {NE, NE} end, Elem, Queries),
%     % io:format("Process ~p end.\n", [Id]),
%     Parent!{Id, Elems}.


% Using Fenwick (not working)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% calculate({Array, Queries, N, _}) ->
%     % io:format("N: ~p\n", [length(Array)]),
%     % io:format("Q: ~p\n", [length(Queries)]),
%     IdsArray = 
%         lists:zip(lists:seq(0, N - 1), Array),
%     FTreeInit = 
%         lists:foldl(
%             fun(Value, CFTree) -> 
%                 add_value(Value, CFTree) 
%             end, 
%             array:new(N), 
%             IdsArray),
%     % io:format("FTreeInit: ~p\n", [FTreeInit]),
%     {Sums,_} = 
%         lists:mapfoldl(
%             fun(Query, CFTreeQ) ->
%                 NFTree = 
%                     lists:foldl(
%                         fun(I, CFTree) -> 
%                             add_value({I, Query}, CFTree) 
%                         end, 
%                         CFTreeQ, 
%                         lists:seq(0, N - 1)
%                     ),
%                 % io:format("NFTree: ~p\n", [NFTree]),
%                 {sum_abs(N - 1, NFTree), NFTree}
%             end,
%             FTreeInit,
%             Queries
%         ),
%     Sums.

% Using proposed solution
%%%%%%%%%%%%%%%%%%%%%%%%%%%%


calculate({Array, Queries, _, _}) ->
    get_sums(Queries, get_inital_cum(Array), 0, []).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(Sums) ->
    Output = 
        lists:foldl(
            fun(Sum, Acc) -> 
                [Acc | io_lib:format("~p\n", [Sum])]
            end,
            "", Sums), 
    io:format("~s\n", [lists:droplast(lists:flatten(Output))]). 
    % lists:map(fun(Sum) -> io:format("~p\n", [Sum]) end, Sums). 
    % [io:format("~p\n", [Sum]) || Sum <- Sums].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    Input = read_input([]),
    ArrayStr = 
        string:tokens(lists:nth(2, Input), " "),
    Array = 
        [str2int(ElemStr) || ElemStr <- ArrayStr],
    QueriesStr = 
        string:tokens(lists:nth(4, Input), " "),
    Queries = 
        [str2int(ElemStr) || ElemStr <- QueriesStr],
    N = str2int(lists:nth(1, Input)),
    Q = str2int(lists:nth(3, Input)),
    {Array, Queries, N, Q}.

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Fenwick tree
% Adapted from: http://www.algorithmist.com/index.php/Fenwick_tree
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% % class Fenwick_Tree_Sum
% % {
% %     vector<int> tree;
% %     Fenwick_Tree_Sum(const vector<int>& Arg)//Arg is our array on which we are going to work
% %     {
% %         tree.resize(Arg.size());
 
% %         for(int i = 0 ; i<tree.size(); i++)
% %             increase(i, Arg[i]);
 
% %     }

% add_value({Pos, Value}, FTree) ->
%     increase(Pos, Value, FTree).
%     % lists:foldl(
%     %     fun (I, CFTree) -> 
%     %         increase(I, Value, CFTree)
%     %     end, 
%     %     FTree, 
%     %     lists:seq(0, array:size(FTree) - 1)
%     % ).


 
% %     // Increases value of i-th element by ''delta''.
% %     void increase(int i, int delta)
% %     {
% %         for (; i < (int)tree.size(); i |= i + 1)
% %             tree[i] += delta;
% %     }

% increase(I, Delta, FTree) ->
%     case I < array:size(FTree) of 
%         true -> 
%             % io:format("I: ~p\nCurrent: ~p\n", [I, array:get(I, FTree)]),
%             NValue = 
%                 case array:get(I, FTree) of 
%                     undefined -> 
%                         Delta;
%                     V ->
%                         V + Delta
%                 end,
%             NFTree = 
%                 array:set(I, NValue, FTree),
%             increase(I bor (I + 1), Delta, NFTree);
%         false ->    
%             FTree         
%     end. 
 
% %     // Returns sum of elements with indexes left..right, inclusive; (zero-based);
% %     int getsum(int left, int right)
% %     {
% %         return sum(right) - sum(left-1); //when left equals 0 the function hopefully returns 0
% %     }

% % % Not used
% % get_sum(Left, Right, FTree) ->
% %     sum(Right, FTree) - sum(Left - 1, FTree).
 
% %     int sum(int ind)
% %     {
% %         int sum = 0;
% %         while (ind>=0)
% %         {
% %             sum += tree[ind];
% %             ind &= ind + 1;
% %             ind--;
% %         }
% %         return sum;
% %     }
% % };

% sum_abs(I, Ftree) ->
%     sum_abs(I, Ftree, 0).

% sum_abs(I, _, Sum) when I < 0->
%     Sum;
% sum_abs(I, FTree, Sum) when I >= 0->
%     % Use of absolute value is because the problem. 
%     % Usually it should be without. 
%     % NSum = Sum + abs(array:get(I, FTree)),
%     NSum = Sum + array:get(I, FTree),
%     NI = I band (I + 1),
%     sum_abs(NI - 1, FTree, NSum).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Alternative approach 
% Adapted from: http://ideone.com/clAHhh
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% int main()
% {
%       int t=1;
%       //s(t);
%       while(t--)
%       {
%             int n,Q,q;
%             LL add = 0;
%             s(n);
%             REP(i,n)
%                   sl(inp[i]);
%             sort(inp,inp+n);
%             cum[0] = inp[0];
%             INC(i,1,n-1)
%                   cum[i] = cum[i-1]+inp[i];
%             s(Q);

get_inital_cum(Array) ->
    [H|T] = 
        % lists:sort([abs(E) || E <- Array]),
        lists:sort(Array),
    {NT,_} = 
        lists:mapfoldl(
            fun(E, Prev) ->
                NE = E + Prev,
                {NE, NE}
            end,
            H,
            T
        ),
    NArray = [H|NT],
    % io:format("List: ~w\n", [NArray] ),
    array:from_list(NArray).



%             while(Q--)
%             {
%                   s(q);
%                   add+=q;
%                   int pos = lower_bound(inp,inp+n,-add)-inp;
%                   LL ans;
%                   if(pos>0)
%                         ans = (cum[n-1]-cum[pos-1]+add*(n-pos))-(cum[pos-1]+add*pos);
%                   else 
%                         ans = (cum[n-1]+add*n);
%                   printf("%lld\n",ans);
%             }
%       }
%       return 0;
% }

% Sample Input

% 3
% -1 2 -3
% 3
% 1 -2 3 
% Sample Output

% 5
% 7
% 6
% Explanation

% After Query 1 : [ 0 , 3 , -2 ] => sum = 0 + 3 + 2 = 5 
% After Query 2 : [ -2 , 1 , -4 ] => sum = 2 + 1 + 4 = 7 
% After Query 3 : [ 1 , 4 , -1 ] => sum = 1 + 4 + 1 = 6

get_sums([], _, _, Acc) ->
    lists:reverse(Acc);
get_sums([Q | Qs], Array, Add, Acc) ->
    N = array:size(Array),
    % io:format("Size: ~p\n", [N]),
    % io:format("E0: ~p\n", [array:get(0, Array)]),
    % io:format("E1: ~p\n", [array:get(1, Array)]),
    % io:format("E2: ~p\n", [array:get(2, Array)]),
    NAdd = Add + Q,
    FunAll = 
        fun() -> 
            array:get(N - 1, Array) + (NAdd * N)
        end,
    Sum = 
        case get_pos(Array, -NAdd, 0, N) of 
            not_found -> 
                FunAll();
            0 -> 
                FunAll();
            Pos ->
                % io:format("Pos: ~p\n", [Pos]),
                Pos_1 = array:get(Pos - 1, Array) ,
                (array:get(N - 1, Array) - Pos_1 + (NAdd * (N - Pos)))
                - (Pos_1+ (NAdd * Pos))
        end,
    get_sums(Qs, Array, NAdd, [Sum | Acc]).

get_pos(_, _, Max, Max) ->
    not_found;
get_pos(Array, Looked, I, Max) ->
    case array:get(I, Array) of 
        Looked ->
            I;
        _ ->
           get_pos(Array, Looked, I + 1, Max) 
    end.


