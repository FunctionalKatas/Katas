defmodule Luhn do

  @doc ~S"""
  ## Examples

      iex> Luhn.credit_card [4,9,9,2,7,3,9,8,7,1,6]
      :valid
      iex> Luhn.credit_card [1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,0]
      :valid
      iex> Luhn.credit_card [4,9,9,2,7,3,9,8,7,1,7]
      :invalid
      iex> Luhn.credit_card [1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,8]
      :invalid

  """
  def credit_card(digits) do
    Enum.reverse(digits)
    |> luhn_sum
    |> check
  end

  defp luhn_sum([odd, even | rest]) when even >= 5,
    do: odd + 2 * even - 10 + 1 + luhn_sum(rest)

  defp luhn_sum([odd, even | rest]),
    do: odd + 2 * even + luhn_sum(rest)

  defp luhn_sum([odd]), do: odd

  defp luhn_sum([]), do: 0

  defp check(sum) when rem(sum, 10) == 0, do: :valid
  
  defp check(_), do: :invalid

end