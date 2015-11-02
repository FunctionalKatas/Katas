defmodule Kata do 
  defp do_is_prime?(_prime, _number, is_divisible) when is_divisible, do: false
  defp do_is_prime?(_prime, number, _is_divisible) when number == 1, do: true
  defp do_is_prime?(prime, number, _is_divisible) do
    do_is_prime?(prime, number-1, rem(prime, number) == 0)
  end
  def is_prime?(prime) do 
    do_is_prime? prime, prime-1, false
  end

  def find_prime_sum max_number do
    Agent.start_link(fn -> 0 end, name: :answer)
    Agent.start_link(fn -> 0 end, name: :answer_count)
    Agent.start_link(fn -> 0 end, name: :sum)
    Agent.start_link(fn -> 0 end, name: :count)
    primes = Enum.filter(2..max_number, &Kata.is_prime?/1)

    Enum.each primes, fn i ->

      Agent.update(:sum, fn _ -> i end)
      Agent.update(:count, fn _ -> 0 end)
      Enum.each primes, fn j ->
        if j > i do
          Agent.update(:sum , &(&1 + j))
          sum = Agent.get(:sum, &(&1))
          Agent.update(:count, &(&1 + 1))
          count = Agent.get(:count, &(&1))
          
          if sum <= max_number and is_prime?(sum) and count > Agent.get(:answer_count, &(&1)) do 
            Agent.update(:answer, fn _ -> sum end)
            Agent.update(:answer_count, fn _ -> count end)
          end
        end
      end 
    end
    Agent.get(:answer, &(&1))
  end

end

IO.puts Kata.find_prime_sum 100000000