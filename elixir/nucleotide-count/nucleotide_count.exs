defmodule NucleotideCount do
    def count(string, nucleotide) do
        count_p(string, nucleotide, 0)
    end

    defp count_p([], _, cnt) do
        cnt
    end

    defp count_p([h|t], nucleotide, cnt) do
        case h do
            ^nucleotide ->
                count_p(t, nucleotide, cnt+1)
            _ ->
                count_p(t, nucleotide, cnt)
        end
    end

    def histogram(string) do
        histogram_p(string, %{?A => 0, ?C => 0, ?G => 0, ?T => 0})
    end

    defp histogram_p([], map) do
        map
    end

    defp histogram_p([h|t], map) do
        case h do
            ?A -> histogram_p(t, %{map | ?A => map[?A]+1})
            ?C -> histogram_p(t, %{map | ?C => map[?C]+1})
            ?G -> histogram_p(t, %{map | ?G => map[?G]+1})
            ?T -> histogram_p(t, %{map | ?T => map[?T]+1})
        end         
    end


end