defmodule NucleotideCount do
    def count(string, nucleotide) do
        count_p(string, nucleotide, 0)
    end

    defp count_p([], _, cnt) do
        cnt
    end

    defp count_p([h|t], nucleotide, cnt) do
        cond do
            h == nucleotide ->
                count_p(t, nucleotide, cnt+1)
            h != nucleotide -> 
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
        cond do
            h == ?A ->
                histogram_p(t, %{map | ?A => map[?A]+1})
            h == ?C -> 
                histogram_p(t, %{map | ?C => map[?C]+1})
            h == ?G ->
                histogram_p(t, %{map | ?G => map[?G]+1})
            h == ?T -> 
                histogram_p(t, %{map | ?T => map[?T]+1})               
        end          
    end


end