defmodule Zipper do
    def from_list([h|t]) do
        [prev: [], focus: h, next: t]
    end

    def next([prev: p, focus: f, next: n]) do
        [h|t] = n
        [prev: [f|p], focus: h, next: t]    
    end

    def next([prev: p, focus: f, next: []]) do
        [prev: p, focus: f, next: []]    
    end

    def prev([prev: [h|t], focus: f, next: n]) do
        [prev: [t], focus: h, next: [f|n]]    
    end

    def prev([prev: [], focus: f, next: n]) do
        [prev: [], focus: f, next: n]    
    end

    def to_list([prev: p, focus: f, next: n]) do
        Enum.reverse(p) ++ [f] ++ n    
    end

    def set_value([prev: p, focus: _, next: n], val) do
        [prev: p, focus: val, next: n]
    end

    def value([prev: p, focus: f, next: n]) do
        f
    end
end