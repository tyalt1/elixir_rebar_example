# elixir_example

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
## Tour of Elixir

Included Elixir code is meant to be a whirlwind tour of Elixir features.
The target audience is Erlang developers.

File | Lessons
-----|-------------
`hello.ex` | Basic "Hello, World!" in Elixir.
`complex_math` | Pattern matching and guards, list comprehension, lexical scope, module nesting, imports, lazy evaluation with Stream module, pipe operator, Task module used as a future, macros.
`alt_server.ex` | How to write a `gen_server`/`GenServer` in Elixir. Introduce Agent had GenServer alternative.
`otp.ex` | Example of OTP application. Includes starting supervisor without creating a separate module. Children are defined in `alt_server.ex`.
