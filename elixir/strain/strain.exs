defmodule Strain do
    def keep(lst, f) do
        keep_p(lst, f, [])
    end

    defp keep_p([h|t], f, accum) do
        case f.(h) do
            true  -> keep_p(t, f, accum ++ [h])
            false -> keep_p(t, f, accum)
        end
    end

    defp keep_p([], _, accum) do
        accum
    end

    def discard(lst, f) do
        discard_p(lst, f, [])
    end

    defp discard_p([h|t], f, accum) do
        case f.(h) do
            true  -> discard_p(t, f, accum)
            false -> discard_p(t, f, accum ++ [h])
        end
    end

    defp discard_p([], _, accum) do
        accum
    end    
end