defmodule Huffman do
    
    def merge({t1, p1}, {t2, p2}) do
        if p1 < p2 do
            {[t1, t2], p1+p2}
        else
            {[t2, t1], p1+p2}
        end
    end

    def tree(queue) do
        case length(queue) do
            1 -> 
                # We want to stop when we really have a tree
                queue
            _ ->
                # Pop the two least frequently ocurring symbols from the queue
                [n1, n2 | t] = queue
                # Merge the two symbols
                nn = merge(n1, n2)
                # Add the newly merged symbol to the reduced queue
                queue = PriorityQueue.add(t, nn)
                # Continue
                tree(queue)
        end    
    end

    def unwrap({tree, pattern}) do
        case tree do
            [left, right] ->
                unwrap({left, pattern <> "0"}) ++ unwrap({right, pattern <> "1"})
            x ->
                [{x, pattern}]
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