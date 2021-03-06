defmodule Huffman do
    
    def merge({t1, p1}, {t2, p2}) do
        if p1 < p2 do
            {[t1, t2], p1+p2}
        else
            {[t2, t1], p1+p2}
        end
    end

    def build_tree([h|[]]) do
        return h
    end

    def build_tree([h1,h2|t]) do
        # Merge the two symbols
        nn = merge(n1, n2)
        # Add the newly merged symbol to the reduced queue 
        queue = PriorityQueue.add(t, nn)
        # Continue
        build_tree(queue, n-1)         
    end

    def build_patterns(tree) do 
        build_patterns(tree,"")
    end

    # Traverses tree from root
    def build_patterns(tree, pattern) do
        case tree do
            [left, right] ->
                build_patterns(left, pattern <> "0") ++ build_patterns(right, pattern <> "1")
            x ->
                [{x, pattern}]
        end
    end

end

defmodule PriorityQueue do
    def create() do
        []
    end

    def priority({_, p}) do
        p
    end

    def add(queue, node) do
        add(queue, node, [])    
    end

    # We've gone though everything, the proposed node is
    # the lowest priority
    def add([], node, front) do
        front ++ [node]        
    end

    def add([h|t], node, front) do
        
        # node is lower priority, so continue
        if priority(node) > priority(h) do
            add(t, node, front ++ [h])
        else
            # node is higher priority than h, so insert
            front ++ [node] ++ [h|t]
        end 
    end

end