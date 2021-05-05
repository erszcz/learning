defmodule S do

  defstruct name: "John", age: 27

  def upgrade(%__MODULE__{} = s) do
    ## If we have to compute some defaults for this struct type,
    ## this is the right place to do it.
    default = %__MODULE__{}
    struct(__MODULE__, Map.from_struct(Map.merge(default, s)))
  end

  ## Just a test fixture
  @old_version %{__struct__: __MODULE__, name: "John"}

  ## Just for IEx
  def old(), do: @old_version

  ## Just for IEx
  def test() do
    upgrade(old())
  end

end

defmodule Generic do

  def upgrade(just_deserialized) do
    s = Map.get(just_deserialized, :__struct__)
    Map.merge(struct(s), just_deserialized)
  end

end
