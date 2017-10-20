defmodule Benchmark do
  alias :timer, as: Timer

  @doc "Macro for logging benchmarks of function calls."
  defmacro bench(label \\ "", do: block) do
    quote do
      IO.write(unquote(label) <> ": ")
      {time, val} = Timer.tc(fn -> unquote(block) end)
      IO.write("Returned value #{val} in #{time/1000} milliseconds\n")
      val
    end
  end
end

defmodule ComplexMath do
  defmodule Math do
    import :math, only: [sqrt: 1]
    import Integer, only: [is_even: 1]

    @doc "Returns true if number is prime."
    def prime?(2), do: true
    def prime?(n) when is_even(n), do: false
    def prime?(n) when n > 1, do: prime?(n, sqrt(n), 3)
    def prime?(_), do: false

    @doc false
    defp prime?(n, root, i) do
      cond do
        i > root -> true
        rem(n, i) === 0 -> false
        true -> prime?(n, root, i+2)
      end
    end

    def factors(n) do
      import List, only: [flatten: 1]

      for x <- 1..round(sqrt(n)), rem(n, x) === 0 do
        [x, div(n,x)]
      end
      |> flatten
      |> Enum.uniq
      |> Enum.sort
    end

    def divisors(n), do: factors(n) -- [n]
  end

  defmodule Lazy do

    @doc "Lazy Sequence of prime numbers."
    def prime(start \\ 2) do
      start
      |> Stream.iterate(fn(n)-> n+1 end)
      |> Stream.filter(&Math.prime?/1)
    end

    @doc "Lazy Sequence of fibonacci numbers."
    def fib(f1 \\ 1, f2 \\ 1) do
      {f1,f2}
      |> Stream.iterate(fn {x,y} -> {y,x+y} end)
      |> Stream.map(fn {x,_} -> x end)
    end
  end

  defmodule Example do
    import Integer, only: [is_even: 1]
    import Benchmark

    @doc "Shows off Pipe Operator. Returns the sum all even fibonacci numbers below 4 million."
    def pipe_example do
      Lazy.fib
      |> Stream.take_while(fn n -> n < 4_000_000 end)
      |> Stream.filter(&is_even/1)
      |> Enum.sum()
    end

    @doc "Same as calculation as pipe_example, but highlights Eager vs Lazy evaluation."
    def lazy_example do
      bench "Eager example" do
        Lazy.fib
        |> Enum.take_while(fn n -> n < 4_000_000 end)
        |> Enum.filter(&is_even/1)
        |> Enum.sum()
      end
      bench "Lazy example" do
        Lazy.fib
        |> Stream.take_while(fn n -> n < 4_000_000 end)
        |> Stream.filter(&is_even/1)
        |> Enum.sum()
      end
    end

    @doc "Shows off Task module (future)."
    def task_example do
      import :timer, only: [sleep: 1]

      expensive1 = fn-> sleep(1000); 5 end
      expensive2 = fn-> sleep(1000); 4 end

      bench "Starting sequential part" do
        expensive1.() + expensive2.()
      end

      bench "Starting threaded part" do
        task1 = Task.async(expensive1)
        task2 = Task.async(expensive2)
        Task.await(task1) + Task.await(task2)
      end
    end
  end
end
