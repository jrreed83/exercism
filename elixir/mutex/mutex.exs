defmodule Mutex do
    def create() do
        spawn(fn() -> loop() end)
    end

    defp loop() do
        receive do
            {pid, :lock} -> send(pid, {self(), :ok})
            receive do
                {^pid, :unlock} -> loop() 
            end
        end
    end

    def lock(pid) do
        send(pid, {self(), :lock})
        receive do
            {^pid, :ok} -> "Yay"    
        end
    end

    def unlock(pid) do
        send(pid, {self(), :unlock})
    end

    def task(pid) do
        lock(pid)        
        :timer.sleep(1000)
        IO.puts("Message from process #{inspect self()}")
        unlock(pid)
    end
end


