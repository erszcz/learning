defmodule HelloWorld do

def hello do
  IO.puts "Hello World"
end

def sum(a,b) do
  a + b
end

def sub(a,b) do
  a - b
end

def reverse l do
  alias Enum, as: E
  E.reverse l
end

end # module
