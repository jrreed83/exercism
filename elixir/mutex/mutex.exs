defmodule Mutex do
    def create() do
        spawn fn() -> loop() end
    end

    defp loop() do
        receive do
            {:lock,   pid} -> send(pid, {:ok, self()})
            {:unlock, pid} -> send(pid, {:ok, self()})        
        end
        loop()
    end

    def lock(pid) do
        send(pid, {:lock, self()})
        receive do
            {:ok, _} -> "Yay"    
        end
    end

    def unlock(pid) do

        send(pid, {:unlock, self()})
    end
end

