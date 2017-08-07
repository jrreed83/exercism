Code.load_file("hello_world.exs", __DIR__)

ExUnit.start

defmodule HelloWorldTest do
    use ExUnit.Case

    test "says hello with no name" do
        assert HelloWorld.hello() == "Hello, World!"
    end

    test "says hello with sample name" do
        assert HelloWorld.hello("Alice") == "Hello, Alice!"
    end    

    test "says hello with other name" do
        assert HelloWorld.hello("Bob") == "Hello, Bob!"
    end      
end
