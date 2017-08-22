defmodule NucleotideCount do
    def count(string, nucleotide) do
        count(string, nucleotide, 0)
    end

    defp count([], _, cnt) do
        cnt
    end

    defp count([h|t], nucleotide, cnt) do
        case h do
            ^nucleotide ->
                count(t, nucleotide, cnt+1)
            _ ->
                count(t, nucleotide, cnt)
        end
    end

    def histogram(string) do
        histogram(string, %{?A => 0, ?C => 0, ?G => 0, ?T => 0})
    end

    defp histogram([], map) do
        map
    end

    defp histogram([h|t], map) do
        case h do
            ?A -> histogram(t, %{map | ?A => map[?A]+1})
            ?C -> histogram(t, %{map | ?C => map[?C]+1})
            ?G -> histogram(t, %{map | ?G => map[?G]+1})
            ?T -> histogram(t, %{map | ?T => map[?T]+1})
        end         
    end


end