defmodule RotationalCipher do


    def transform_char(x, shift) do
        case x do
            x when (?a <= x) and (x <= ?z)  ->
                if x + shift > ?z do 
                    ?a + (x + shift - ?z - 1)
                else
                    x + shift 
                end
            x when (?A <= x) and (x <= ?Z) -> 
                if x + shift > ?Z do 
                    ?A + (x + shift - ?Z - 1)
                else
                    x + shift 
                end
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