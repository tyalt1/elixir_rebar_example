-module(elixir_rebar_example_app).
-behaviour(application).

%% Application callbacks
-export(
  [ start/2
  , stop/1
  ]).

%% Application callbacks

start(_StartType, _StartArgs) ->
  elixir_rebar_example_sup:start_link().

stop(_State) ->
  ok.
