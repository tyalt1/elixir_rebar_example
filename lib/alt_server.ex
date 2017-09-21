defmodule AltServer.Server do
  use GenServer

  #Public API
  def start_link(n \\ 0) do
    GenServer.start_link(__MODULE__, n)
  end

  def inc(pid), do: GenServer.cast(pid, :inc)
  def dec(pid), do: GenServer.cast(pid, :dec)
  def get(pid), do: GenServer.call(pid, :get)

  #Callbacks
  def handle_cast(:inc, n) do
    {:noreply, n+1}
  end
  def handle_cast(:dec, n) do
    {:noreply, n-1}
  end

  def handle_call(:get, _from, n) do
    {:reply, {:ok, n}, n}
  end
end

defmodule AltServer.Agent do
  #Public API
  def start_link(n \\ 0) do
    Agent.start_link(fn-> n end)
  end

  def inc(pid), do: Agent.update(pid, fn(n)-> n+1 end)
  def dec(pid), do: Agent.update(pid, fn(n)-> n-1 end)
  def get(pid), do: Agent.get(pid, fn(n)-> n end)
end
