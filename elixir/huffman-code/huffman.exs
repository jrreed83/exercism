defmodule Huffman do
    
    def merge({t1, p1}, {t2, p2}) do
        if p1 < p2 do
            {[t1, t2], p1+p2}
        else
            {[t2, t1], p1+p2}
        end
    end

end

defmodule PriorityQueue do
    def create() do
        []
    end

    def add(queue, node) do
        add_p(queue, node, [])    
    end

    def priority({_, p}) do
        p
    end

    # We've gone though everything, the proposed node is
    # the lowest priority
    defp add_p([], node, front) do
        front ++ [node]        
    end

    defp add_p([h|t], node, front) do
        
        # node is lower priority, so continue
        if priority(node) > priority(h) do
            add_p(t, node, front ++ [h])
        else
            # node is higher priority than h, so insert
            front ++ [node] ++ [h|t]
        end 
    end

end