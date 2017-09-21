defmodule MyApp do
  use Application

  @moduledoc """
  Example of OTP application starting counter servers from alt_server.ex.
  """

  def start(_type, _args) do
    import Supervisor.Spec

    children = [
      worker(AltServer.Server, [], [name: MyApp.Counter.Server]),
      worker(AltServer.Agent, [], [name: MyApp.Counter.Agent]),
    ]

    opts = [strategy: :one_for_one, name: MyApp.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
