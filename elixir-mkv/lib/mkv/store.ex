defmodule MKV.Store do

  @table __MODULE__

  def init do
    :ets.new(@table, [:named_table, :public])
  end

  def get(key, default) do
    case :ets.lookup(@table, key) do
      [] -> default
      [{^key, value}] -> value
    end
  end

  def get!(key) do
    case get(key, {:not_found, key}) do
      {:not_found, ^key} -> raise "not found: #{key}"
      value -> value
    end
  end

  def put(key, value) do
    :true = :ets.insert(@table, {key, value})
  end

end
