# elixir_rebar_example

An example of how to build Elixir code in a rebar project.

## Build

```
With Erlang/Elixir installed. Must have rebar in path.
$ make

With docker installed. Uses elixir docker container.
$ make docker
# cd /code
# make
```

## Run

In Erlang shell
```
$ rebar shell
Erlang/OTP 20 [erts-9.0.3] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:10] [hipe] [kernel-poll:false]
Eshell V9.0.3  (abort with ^G)
1> 'Elixir.ComplexMath.Example':lazy_example().
Eager example: Returned value 4613732 in 133.172 milliseconds
Lazy example: Returned value 4613732 in 0.045 milliseconds
4613732
```

In Elixir shell
```
$ iex
Erlang/OTP 20 [erts-9.0.3] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:10] [hipe] [kernel-poll:false]
Interactive Elixir (1.5.1) - press Ctrl+C to exit (type h() ENTER for help)
iex(1)> c "lib/complex_math.ex"
[ComplexMath, ComplexMath.Example, ComplexMath.Lazy, ComplexMath.Math,
 Benchmark]   
iex(2)> ComplexMath.Example.lazy_example
Eager example: Returned value 4613732 in 9.236 milliseconds
Lazy example: Returned value 4613732 in 0.059 milliseconds
4613732
```

## Tour of Elixir

Included Elixir code is meant to be a whirlwind tour of Elixir features.
The target audience is Erlang developers.

File | Lessons
-----|-------------
`hello.ex` | Basic "Hello, World!" in Elixir.
`complex_math` | Pattern matching and guards, list comprehension, lexical scope, module nesting, imports, lazy evaluation with Stream module, pipe operator, Task module used as a future, macros.
`alt_server.ex` | How to write a `gen_server`/`GenServer` in Elixir. Introduce Agent had GenServer alternative.
`otp.ex` | Example of OTP application. Includes starting supervisor without creating a separate module. Children are defined in `alt_server.ex`.
