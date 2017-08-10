defmodule RotationalCipher do


    def transform_char(x, shift) do
        case x do
            x when (?a <= x) and (x <= ?z)  ->
                xx = x + shift - ?a 
                xx = rem(xx, 26)
                xx = xx + ?a 
                xx
            x when (?A <= x) and (x <= ?Z) -> 
                xx = x + shift - ?A 
                xx = rem(xx, 26)
                xx = xx + ?A 
                xx
            _ -> 
                x
        end   
    end

    def rotate(text, shift) do
        
        text |> to_charlist   # Convert to character list
             |> Enum.map(fn(x) -> transform_char(x, shift) end) # Rotate each character
             |> to_string # Convert back to string  
    end


end