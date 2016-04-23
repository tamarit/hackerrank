% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

% https://www.hackerrank.com/challenges/brainf-k-interpreter-fp

-module(solution).
-export([main/0]).

-define(MAX_OPERATIONS, 100000).

main() ->
    Data = read_data(),
    output_data(calculate(Data)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Calculate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, 
        {
            data_pointer = 0,
            memory,
            input = [],
            output = [],
            program = [],
            stack = [],
            operations = 0,
            continue = true
        }).

calculate({Input, Program}) ->
    InitialState = 
        #state{
            input = Input, 
            memory = dict:store(0, 0, dict:new()),
            program = Program
        },
    FinalState = eval(InitialState),
    % io:format("FinalState: ~p\n", [FinalState]),
    lists:reverse(FinalState#state.output).

eval(State = #state{continue = false}) ->
    State;
eval(State = #state{program = [Op = $>|T]}) ->
    NState0 = change_data_pointer(Op, State),
    eval(add_operation(Op, T, NState0));
eval(State = #state{program = [Op = $<|T]}) ->
    NState0 = change_data_pointer(Op, State),
    eval(add_operation(Op, T, NState0));
eval(State = #state{program = [Op = $+|T]}) ->
    NState0 = change_value(Op, State),
    eval(add_operation(Op, T, NState0));
eval(State = #state{program = [Op = $-|T]}) ->
    NState0 = change_value(Op, State),
    eval(add_operation(Op, T, NState0));
eval(State = #state{
        program = [Op = $.|T], 
        output = Output,
        data_pointer = DataPointer,
        memory = Memory}) ->
    NState0 = 
        State#state{
            output = [dict:fetch(DataPointer, Memory)|Output]
        },
    eval(add_operation(Op, T, NState0));
eval(State = #state{
        program = [Op = $,|T], 
        input = [IH|IT],
        data_pointer = DataPointer,
        memory = Memory}) ->
    NState0 = 
        State#state{
            input = IT,
            memory = dict:store(DataPointer, IH, Memory)
        },
    eval(add_operation(Op, T, NState0));
eval(State = #state{
        program = [Op = $[|T], 
        data_pointer = DataPointer,
        memory = Memory,
        stack = Stack}) ->
    Value = dict:fetch(DataPointer, Memory),
    {Prev, Next} = search_next_matching_bracket(T, 0, []),
    NState0 = State#state{stack = [[]|Stack]},
    NState1 = add_operation(Op, T, NState0),
    NState2 =
        case Value of 
            0 ->
                NState1#state{
                    program = Next,
                    stack = add_list_stack(Prev, NState1)
                };
            _ ->
                NState1
        end,
    eval(NState2);
eval(State = #state{
        program = [Op = $]|T], 
        data_pointer = DataPointer,
        memory = Memory}) ->
    Value = dict:fetch(DataPointer, Memory),
    NState0 = add_operation(Op, T, State),
    [HS|NStack] =  NState0#state.stack,
    NState1 = NState0#state{stack = NStack},
    NState2 = 
        case Value of 
            0 -> 
                NState1#state{
                    stack = add_list_stack(lists:reverse(HS), NState1)
                };
            _ ->
                NState1#state{
                    program = lists:reverse(HS) ++ T
                }
        end,
    eval(NState2);
eval(State = #state{program = [_|T]}) ->
    eval(State#state{program = T});
eval(State = #state{program = []}) ->
    State.

change_data_pointer(Op,
        State = 
            #state{
                data_pointer = DataPointer, 
                memory = Memory})->
    NDataPointer = 
        case Op of 
            $< -> 
                DataPointer - 1;
            $> ->
                DataPointer + 1
        end,
    State#state{
        data_pointer = NDataPointer,
        memory = 
            case dict:is_key(NDataPointer, Memory) of 
                true -> 
                    Memory;
                false -> 
                    dict:store(NDataPointer, 0, Memory)
            end
    }. 

change_value(Op,
        State = 
            #state{
                data_pointer = DataPointer, 
                memory = Memory}) ->
    PreviousValue = 
        dict:fetch(DataPointer, Memory),
    NValue0 = 
        case Op of 
            $+ -> 
                PreviousValue + 1;
            $- ->
                PreviousValue - 1
        end,
    NValue = 
        case NValue0 of 
            -1 -> 
                255;
            256 ->
                0;
            Other -> 
                Other
        end,
    State#state{
        memory = dict:store(DataPointer, NValue, Memory)
    }.

add_operation(Op, NProgram, State = #state{operations = Ops, output = Output}) ->
    {NContinue, NOutput} = 
        case {Ops + 1, Output} of
            {?MAX_OPERATIONS, "oC"}  ->
                {true, Output};
            {?MAX_OPERATIONS, _} ->
                {false, lists:reverse("\nPROCESS TIME OUT. KILLED!!!") ++ Output};
            _ ->
                {true, Output}
        end,
    State#state{
        operations = Ops + 1, 
        program = NProgram,
        stack = add_stack(Op, State),
        continue = NContinue,
        output = NOutput
    }.

add_stack(Op, #state{stack = [H|T]}) ->
    [[Op|H]|T];
add_stack(_, #state{stack = []}) ->
    [].

add_list_stack(List, State) ->
    NStateStack =
        lists:foldl(
            fun(E, CurrentState) ->
                NStack_ = add_stack(E, CurrentState),
                CurrentState#state{stack = NStack_}
            end,
            State,
            List
        ),
    NStateStack#state.stack.

search_next_matching_bracket([$]|T], 0, Acc) ->
    {lists:reverse(Acc),[$]|T]};
search_next_matching_bracket([$]|T], N, Acc) ->
    search_next_matching_bracket(T, N - 1, [$]|Acc]);
search_next_matching_bracket([$[|T], N, Acc) ->
    search_next_matching_bracket(T, N + 1, [$[|Acc]);
search_next_matching_bracket([C|T], N, Acc) ->
    search_next_matching_bracket(T, N, [C|Acc]).
% No need for a empty list clause since there will not be unpaired brackets.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(Data) -> 
    show_str(Data). 

show_str(Str) ->
    io:format("~s\n", [Str]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    [Input | Program] = tl(read_input([])),
    {lists:droplast(Input), lists:flatten(Program)}.

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