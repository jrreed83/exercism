defmodule BankAccount do
    def open_account() do
        spawn(fn() -> loop(%{balance: 0, state: :open}) end)
    end

    def loop(%{balance: b, state: s}) do
        receive do
            {sender, :balance} ->
                if s == :closed do 
                    send(sender, {self(), :error, :account_closed})
                else
                    send(sender, {self(), :ok, b}) 
                end
                loop(%{balance: b, state: s})
            {sender, :update, amt} ->
                if s == :closed do 
                    send(sender, {self(), :error, :account_closed})   
                    loop(%{balance: b, state: s})
                else
                    send(sender, {self(), :ok,})                       
                    loop(%{balance: b+amt, state: s})
                end
            {_, :close} -> 
                loop(%{balance: b, state: :closed})
        end
    end

    def balance(account) do
        send(account, {self(), :balance})
        receive do
            {_, :ok,    balance} -> balance
            {_, :error, :account_closed} -> {:error, :account_closed}
        end    
    end

    def update(account, amt) do
        send(account, {self(), :update, amt}) 
        receive do
            {_, :error, :account_closed} -> {:error, :account_closed}
            {_, :ok} -> :ok
        end         
    end

    def close_account(account) do
        send(account, {self(), :close})
    end
end