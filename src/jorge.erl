-module(jorge).

-export([jorge/1]).

-ifdef(TEST).
-export([jorge_test/0]).
-endif.

-type key() :: atom() | binary().
-type value() :: key() | integer() | float() | fun(() -> iolist()).
-type attrs() :: [{key(), value()}].

-spec jorge(undefined) -> undefined;
           (Node :: {key(), value()}) -> iolist();
           (Node :: {key(), attrs(), value()}) -> iolist().
jorge(undefined) ->
   undefined;
jorge(Node) ->
    jorge_node(Node).

jorge_node({Key, Value}) ->
    jorge_node(jorge_key(Key), jorge_node(Value));
jorge_node({Key, Attrs, Value}) ->
    jorge_node(jorge_key(Key), jorge_attrs(Attrs), jorge_node(Value));
jorge_node(Value) ->
    jorge_value(Value).

jorge_node(Key, Value) ->
    [<<$<, Key/binary, $>>>, Value, <<"</", Key/binary, $>>>].

jorge_node(Key, Attrs, Value) ->
    [<<$<, Key/binary, Attrs/binary, $>>>, Value, <<"</", Key/binary, $>>>].

jorge_attrs(Attrs) ->
    jorge_attrs(Attrs, <<>>).

jorge_attrs([], Acc) ->
    Acc;
jorge_attrs([{Key, Value} | Rest], Acc) ->
    jorge_attrs(jorge_attr(jorge_key(Key), jorge_value(Value)), Rest, Acc).

jorge_attrs(Attr, Rest, Acc) ->
    jorge_attrs(Rest, <<Acc/binary, $\s, Attr/binary>>).

jorge_attr(Key, Value) ->
    <<Key/binary, "=\"", Value/binary, $">>.

jorge_key(Key) when is_atom(Key) ->
    atom_to_binary(Key, utf8);
jorge_key(Key) when is_binary(Key) ->
    Key.

jorge_value(Value) when is_binary(Value) ->
    Value;
jorge_value(Value) when is_list(Value) ->
    jorge_list(Value);
jorge_value(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
jorge_value(Value) when is_integer(Value) ->
    list_to_binary(integer_to_list(Value));
jorge_value(Value) when is_float(Value) ->
    list_to_binary(hd(io_lib:format("~p", [Value])));
jorge_value(Value) when is_function(Value, 0) ->
    jorge_value(Value()).

jorge_list(List) ->
    jorge_list(List, []).

jorge_list([], []) ->
    [];
jorge_list([], Acc) ->
    lists:reverse(Acc);
jorge_list([undefined | Rest], Acc) ->
    jorge_list(Rest, Acc);
jorge_list([Head | Rest], Acc) ->
    jorge_list(Rest, [jorge_node(Head) | Acc]).

-ifdef(TEST).

jorge_test() ->
    Expected = [<<"<foo grault=\"garply\" waldo=\"fred\">">>,
                [<<"<bar>">>,
                 [[<<"<baz>">>,<<"2">>,<<"</baz>">>],
                  [<<"<quux>">>,<<"corge">>,<<"</quux>">>],
                  [<<"<plugh>">>,<<"5.0">>,<<"</plugh>">>],
                  [<<"<xyzzy>">>, <<"thud">>, <<"</xyzzy>">>],
                  [<<"<spam>">>, <<"eggs">>, <<"</spam>">>]],
                 <<"</bar>">>],
                <<"</foo>">>],
    Expected = jorge(
        {foo, [{grault, garply}, {waldo, fred}],
            {bar, [
                {baz, 2},
                {quux, corge},
                {plugh, 5.0},
                undefined,
                {xyzzy, <<"thud">>},
                {spam, fun() -> eggs end}
            ]}
        }
    ).

-endif.
