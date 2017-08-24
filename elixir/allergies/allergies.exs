defmodule Allergies do
    def all_allergies() do
        [{128, "cats"},
         {64, "pollen"}, 
         {32, "chocolate"},  
         {16, "tomatoes"}, 
         {8,  "strawberries"}, 
         {4,  "shellfish"}, 
         {2,  "peanuts"},
         {1,  "eggs"}]
    end

    def list(score) do
        list(score, all_allergies(), [])    
    end

    def list(_, [], allergies) do
        allergies
    end

    def list(n, [{score, type}|t], allergies) do
        # Greedy algorithm
        case n do
            n when n >= score -> 
                list(n-score, t, allergies ++ [type])
            n when n < score -> 
                list(n, t, allergies)
            0 -> 
                allergies
        end
    end


end