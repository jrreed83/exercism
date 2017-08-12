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

defmodule Semaphore do
    def create(n) do 
        spawn(fn() -> loop(n) end)
    end

    def release(sem) do
        send(sem, {self(), :release}) 
    end

    def acquire(sem) do 
        send(sem, {self(), :acquire})
        receive do 
            {_, :ok} -> :ok
        end
    end

    def loop(n) do
        if n == -1 do
            receive do
                {sender, :release} ->
                    send(sender, {self(), :ok}) 
                    loop(0)
            end
        else
            receive do
                {sender, :release} -> 
                    send(sender, {self(), :ok})
                    loop(n+1)
                {sender, :acquire} ->
                    if n == 0 do
                        loop(-1)
                    else
                        send(sender, {self(), :ok})
                        loop(n-1) 
                    end
            end
        end
    end
end


