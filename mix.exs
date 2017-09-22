defmodule Elixir.Rebar.Example.Mixfile do
  use Mix.Project

  def project do
    [
      app: :elixir_rebar_example,
      version: "1.0.0",
      elixir: "~> 1.2",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps: deps(),
      aliases: aliases()
    ]
  end

  def application do
    [
       applications: [],
       env: [],
       mod: {:elixir_rebar_example_app, []}
    ]
  end

  defp deps do
    []
  end

  defp aliases do
    []
  end

end
