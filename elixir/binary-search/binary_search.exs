defmodule BinarySearch do

    def search({}, _) do
        :not_found
    end

    def search({v}, x) do
        if v == x do
            {:ok, 0}
        else
            :not_found 
        end
    end

    def search({v1, v2}, x) do
        case x do
            ^v1 -> {:ok, 0}
            ^v2 -> {:ok, 1}
            _   -> :not_found
        end
    end

    def search(t, x) do 
        :not_implemented
    end

end