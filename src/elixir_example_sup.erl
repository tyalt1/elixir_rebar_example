%%%-------------------------------------------------------------------
%% @doc elixir_example top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(elixir_example_sup).
-behaviour(supervisor).

-export(
  [start_link/0

  % Callbacks
  , init/1
  ]).

%% ----- API functions -----
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ----- Supervisor callbacks -----
init([]) ->
  Children = [],
  SupSpecs = {one_for_all, 0, 1},
  {ok, {SupSpecs, Children}}.

%% ----- Internal functions -----
