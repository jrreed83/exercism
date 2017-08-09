defmodule SecretHandshake do

    use Bitwise

    def table() do    
        %{
            0b0001 => "wink",
            0b0010 => "double blink",
            0b0100 => "close your eyes",
            0b1000 => "jump"
        }
    end

    def commands(cmd) do

        if (cmd &&& 0b10000) == 0b10000 do
            Enum.reverse(command_p(cmd &&& 0b01111, [], 1))
        else
            command_p(cmd &&& 0b01111, [], 1)
        end
    end

    def command_p(rest, accum, ptr) do
        t = table()
        
        if rest == 0 do
            accum
        else
            if (rest &&& 1) == 1 do
                command_p(rest >>> 1, accum ++ [t[ptr]], ptr <<< 1) 
            else 
                command_p(rest >>> 1, accum, ptr <<< 1)
            end
        end
    end

end