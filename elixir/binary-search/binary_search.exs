defmodule BinarySearch do

    use Bitwise 

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

    def search(tuple, x) do 
        ptr_l = 0
        ptr_r = tuple_size(tuple)-1
        search(tuple, x, ptr_l, ptr_r )
    end 

    def search(tuple, x, ptr_l, ptr_r) do
        ptr_m = (ptr_l + ptr_r) >>> 1
        val_m = elem(tuple, ptr_m)

        if ptr_l == ptr_r do
            # Only one element left 
            cond do 
                x == val_m  -> 
                    {:ok, ptr_l}
                true -> 
                    :not_found
            end
        else
            # More than two elements left
            cond do
                x == val_m ->
                    {:ok, ptr_m}
                x < val_m  -> 
                    search(tuple, x, ptr_l,   ptr_m-1) 
                x > val_m  -> 
                    search(tuple, x, ptr_m+1, ptr_r)
            end
        end
    end

end