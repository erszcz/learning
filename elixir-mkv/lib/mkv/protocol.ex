defmodule MKV.Protocol do

  def decode!(data = <<keylen :: size(32), payload :: bytes>>) do
    <<key :: binary-size(keylen), value :: bytes>> = payload
    %MKV.Entry{key: key, value: value}
  end

end
