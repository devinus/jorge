# Jorge

Jorge (pronounced HOR-HAY) is a simple Erlang DSL for generating XML based on
tuples of tuples.

## Example

```erl-sh
1> Foo = {foo, [{grault, garply}, {waldo, fred}],
1>     {bar, [
1>         {baz, 2},
1>         {quux, corge},
1>         {plugh, 5.0},
1>         {xyzzy, <<"thud">>},
1>         {spam, fun() -> eggs end}
1>     ]}
1> }.
{foo,[{grault,garply},{waldo,fred}],
     {bar,[{baz,2},
           {quux,corge},
           {plugh,5.0},
           {xyzzy,<<"thud">>},
           {spam,#Fun<erl_eval.20.82930912>}]}}
2> jorge:jorge(Foo).
[<<"<foo grault=\"garply\" waldo=\"fred\">">>,
 [<<"<bar>">>,
  [[<<"<baz>">>,<<"2">>,<<"</baz>">>],
   [<<"<quux>">>,<<"corge">>,<<"</quux>">>],
   [<<"<plugh>">>,<<"5.0">>,<<"</plugh>">>],
   [<<"<xyzzy>">>,<<"thud">>,<<"</xyzzy>">>],
   [<<"<spam>">>,<<"eggs">>,<<"</spam>">>]],
  <<"</bar>">>],
 <<"</foo>">>]
```
