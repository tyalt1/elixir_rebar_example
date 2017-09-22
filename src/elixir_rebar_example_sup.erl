-module(elixir_rebar_example_sup).
-behaviour(supervisor).

%% API
-export(
  [ start_link/0

  % Callbacks
  , init/1
  ]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% API functions
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Supervisor callbacks
init([]) ->
  Children = [],
  SupSpec = {one_for_one, 5, 10},
  {ok, {SupSpec, Children}}.
