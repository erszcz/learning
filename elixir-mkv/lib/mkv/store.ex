defmodule MKV.Store do

  @table __MODULE__

  def init do
    :ets.new(@table, [:named_table, :public])
  end

  def get(key, default) do
    case :ets.lookup(@table, key) do
      [] -> default
      [{^key, value}] -> {key, value}
    end
  end

  def get!(key) do
    case get(key, {:not_found, key}) do
      {:not_found, ^key} -> raise "not found: #{key}"
      {^key, value} -> value
    end
  end

  def put(%MKV.Entry{key: k, value: v}) do
    :true = :ets.insert(@table, {k, v})
  end

end
