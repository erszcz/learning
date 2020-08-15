defmodule MKV.Protocol.Get do
  defstruct [:key]
end

defmodule MKV.Protocol.Put do
  defstruct [:key, :value]
end

defmodule MKV.Protocol do

  ## Keep these aligned!
  ## For a given key, the number from @encoding should be the index of the same key
  ## in @decoding, i.e. key == elem(@decoding, @encoding[key])
  @encoding %{MKV.Protocol.Get => 0,
              MKV.Protocol.Put => 1}
  @decoding {MKV.Protocol.Get,
             MKV.Protocol.Put}

  def encoding, do: @encoding
  def decoding, do: @decoding

  def encode(%MKV.Protocol.Get{key: k} = command) do
    <<encode_op(command) :: size(8), byte_size(k) :: size(32), k :: bytes>>
  end
  def encode(%MKV.Protocol.Put{key: k, value: v} = command) do
    <<encode_op(command) :: size(8), byte_size(k) :: size(32), k :: bytes, v :: bytes>>
  end

  def encode_op(command), do: @encoding[command.__struct__]

  def decode!(<<op :: size(8), payload :: bytes>>) do
    if is_known_op?(op) do
      decode(decode_op(op), payload)
    else
      raise "unknown op: #{inspect op}"
    end
  end

  def decode(MKV.Protocol.Get, <<keylen :: size(32), key :: binary-size(keylen)>>) do
    %MKV.Protocol.Get{key: key}
  end
  def decode(MKV.Protocol.Put, <<keylen :: size(32), key :: binary-size(keylen), value :: bytes>>) do
    %MKV.Protocol.Put{key: key, value: value}
  end

  def is_known_op?(op), do: op < tuple_size(@decoding)

  def decode_op(i), do: elem(@decoding, i)

end
