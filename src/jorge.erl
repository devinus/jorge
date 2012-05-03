-module(jorge).

-export([jorge/1]).

-ifdef(TEST).
-export([jorge_test/0]).
-endif.

-type key() :: atom() | string() | binary().
-type value() :: key() | integer() | float() | fun(() -> iolist()).
-type attr() :: {key(), value()}.

-spec jorge({Node :: key(), Value :: value()}) -> iolist();
           ({Node :: key(), Attrs :: [attr()], Value :: value()}) -> iolist();
           (Value :: value()) -> iolist().
jorge({Node, Value}) ->
    jorge_node(Node, Value);
jorge({Node, Attrs, Value}) ->
    jorge_node(Node, Attrs, Value);
jorge(Value) ->
    jorge_value(Value).

jorge_node(Node, Value) when is_binary(Node) ->
    [<<$<, Node/binary, $>>>, jorge(Value), <<"</", Node/binary, $>>>];
jorge_node(Node, Value) ->
    jorge_node(jorge_key(Node), Value).

jorge_node(Node, Attrs, Value) when is_binary(Node) ->
    Attrs1 = jorge_attrs(Attrs, <<>>),
    [<<$<, Node/binary, Attrs1/binary, $>>>,
         jorge(Value),
     <<"</", Node/binary, $>>>];
jorge_node(Node, Attrs, Value) ->
    jorge_node(jorge_key(Node), Attrs, Value).

jorge_attrs([], Acc) ->
    Acc;
jorge_attrs([{Key, Value} | Rest], Acc) ->
    Key1 = jorge_key(Key),
    Value1 = jorge_value(Value),
    Attr = <<Key1/binary, "=\"", Value1/binary, $">>,
    jorge_attrs(Rest, <<Acc/binary, $\s, Attr/binary>>).

jorge_key(Key) when is_atom(Key) ->
    atom_to_binary(Key, utf8);
jorge_key(Key) when is_list(Key) ->
    list_to_binary(Key);
jorge_key(Key) when is_binary(Key) ->
    Key.

jorge_value(Value) when is_binary(Value) ->
    Value;
jorge_value(Value) when is_list(Value) ->
    jorge_list(Value, []);
jorge_value(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
jorge_value(Value) when is_integer(Value) ->
    list_to_binary(integer_to_list(Value));
jorge_value(Value) when is_float(Value) ->
    list_to_binary(hd(io_lib:format("~p", [Value])));
jorge_value(Value) when is_function(Value, 0) ->
    jorge_value(Value()).

jorge_list([], []) ->
    [];
jorge_list([], Acc) ->
    lists:reverse(Acc);
jorge_list([Head | Rest], Acc) ->
    jorge_list(Rest, [jorge(Head) | Acc]).

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
                {xyzzy, <<"thud">>},
                {spam, fun() -> eggs end}
            ]}
        }
    ).

-endif.
