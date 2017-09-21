%%%-------------------------------------------------------------------
%% @doc elixir_example public API
%% @end
%%%-------------------------------------------------------------------

-module(elixir_example_app).
-behaviour(application).

% Callbacks
-export(
  [start/2
  , stop/1
  ]).

%% ----- API -----
start(_StartType, _StartArgs) -> elixir_example_sup:start_link().

stop(_State) -> ok.

%% ----- Internal functions -----
