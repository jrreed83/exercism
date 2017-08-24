defmodule BinaryTree do
    def create() do
        {}
    end

    def insert({}, x) do
        {{}, x, {}}
    end

    def insert({left, val, right}, x) do
        cond do
            x < val  -> 
                {insert(left, x), val, right}
            x > val ->
                {left, val, insert(right, x)}
            x == val  ->
                {left, val, right}
        end
    end

    def is_elem?({}, _) do
        false
    end

    def is_elem?({left, val, right}, x) do
        cond do
            x < val  -> 
                is_elem?(left, x)
            x > val ->
                is_elem?(right, x)
            x == val  ->
                true
        end       
    end
end