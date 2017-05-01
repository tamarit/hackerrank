% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/prison-transport

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

calculate({Inmates, Pairs}) ->
    Forest = 
        singletons_from_list(lists:seq(1, Inmates)),
    lists:foreach(fun({A, B}) ->   union(Forest, find(Forest, A) ,find(Forest, B)) end, Pairs),
    SizesDict = 
        lists:foldl(
            fun(E, Dict) -> 
                Rep = find(Forest, E),
                case dict:is_key(Rep, Dict) of 
                    true -> 
                        Dict;
                    false ->
                        dict:store(Rep, set_size(Forest, Rep), Dict)
                end
            end,
            dict:new(),
            lists:seq(1, Inmates)),
    lists:sum([ceiling(math:sqrt(Size)) || {_, Size} <- dict:to_list(SizesDict)]).

% *********************************
% BEGIN Copied from https://github.com/tafsiri/7languages/blob/master/erlang/pmap.erl
% *********************************

ceiling(X) when X < 0 ->
    trunc(X);
ceiling(X) ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T + 1
    end.

% *********************************
% END Copied from https://github.com/tafsiri/7languages/blob/master/erlang/pmap.erl
% *********************************

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
    [InmatesStr, _ | PairsStr] = [binary_to_list(R) || R <- Res],
    Inmates = str2int(InmatesStr),
    Pairs = 
        [begin 
            [A, B] = strJoint2intList(PairStr), 
            {A,B} 
        end 
        || PairStr <- PairsStr],
    {Inmates, Pairs}.

str2int(Str) ->
    element(1,string:to_integer(Str)).

strJoint2intList(Str) ->
    [str2int(T) || T <- string:tokens(Str, " ")].

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


% *********************************
% BEGIN Copied from https://github.com/aggelgian/erlang-algorithms/blob/master/src/union_find.erl
% *********************************


-define(UNDEF_SIZE, undef).

%%
%% @type uf_forest(). A forest of union-find sets.
%%
-type uf_forest() :: ets:tid().
-type uf_find()   :: term() | uf_undef().
-type uf_size()   :: pos_integer() | uf_undef().
-type uf_union()  :: true | uf_undef() | {error, not_parent_elements}.
-type uf_undef()  :: {error, undef_element}.

%% =======================================================================
%% External Exports
%% =======================================================================

%% @doc Create a forest of singleton sets from a list of terms.
-spec singletons_from_list([term(), ...]) -> uf_forest().

singletons_from_list([]) ->
  erlang:error(badarg);
singletons_from_list(L) when is_list(L) ->
  singletons_from_list(fun id/1, L, ets:new(?MODULE, [ordered_set]));
singletons_from_list(_L) ->
  erlang:error(badarg).

%% Helper function singletons_from_list/3
-spec singletons_from_list(function(), [term()], uf_forest()) -> uf_forest().

singletons_from_list(_Fun, [], Forest) ->
  Forest;
singletons_from_list(Fun, [I|Is], Forest) ->
  ets:insert(Forest, {Fun(I), {root, 1}}),
  singletons_from_list(Fun, Is, Forest).

%% @doc Union of two sets.
%% <p>The parent elements of the two sets are needed.</p>
-spec union(uf_forest(), term(), term()) -> uf_union().

union(_Forest, _X, _X) -> true;
union(Forest, X, Y) ->
  case {ets:lookup(Forest, X), ets:lookup(Forest, Y)} of
    {[{X, {root, SzX}}], [{Y, {root, SzY}}]} ->
      ets:insert(Forest, {Y, {X, ?UNDEF_SIZE}}),
      ets:insert(Forest, {X, {root, SzX + SzY}});
    {[], _} ->
      {error, undef_element};
    {_, []} ->
      {error, undef_element};
    {_, _} ->
      {error, not_parent_elements}
  end.

%% @doc Find the parent element of the set which a term belongs to.
-spec find(uf_forest(), term()) -> uf_find().

find(Forest, X) -> find_and_compress(Forest, X, []).

-spec find_and_compress(uf_forest(), term(), [term()]) -> uf_find().

find_and_compress(Forest, X, Es) ->
  case ets:lookup(Forest, X) of
    [] ->
      {error, undef_element};
    [{X, {root, _SzX}}] ->
      lists:foreach(fun(E) -> ets:insert(Forest, {E, {X, ?UNDEF_SIZE}}) end, Es),
      X;
    [{X, {ParX, ?UNDEF_SIZE}}] ->
      find_and_compress(Forest, ParX, [X|Es])
  end.

%% @doc Return the size of the set which an element belongs to
-spec set_size(uf_forest(), term()) -> uf_size().

set_size(Forest, X) ->
  case ets:lookup(Forest, X) of
    [] -> {error, undef_element};
    [{X, {root, Sz}}] -> Sz;
    [{X, {ParX, ?UNDEF_SIZE}}] -> set_size(Forest, ParX)
  end.
  
-spec id(term()) -> term().
id(Arg) -> Arg.

% *********************************
% END Copied from https://github.com/aggelgian/erlang-algorithms/blob/master/src/union_find.erl
% *********************************
