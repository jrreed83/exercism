defmodule BankAccount do
    def open_account() do
        spawn(fn() -> loop(0) end)
    end

    def loop(balance) do
        receive do
            {sender, :balance} ->
                send(sender,{self(), balance}) 
                loop(balance)
            {_, :update, amt} ->
                new_balance = balance+amt
                loop(new_balance)
        end
    end

    def balance(account) do
        send(account, {self(), :balance})
        receive do
            {_, balance} -> balance
        end    
    end

    def update(account, amt) do
        send(account, {self(), :update, amt}) 
    end
end