defmodule CommunityWeb.GraphQL.LinksTest do
  use ExUnit.Case
  use Wormwood.GQLCase

  load_gql(CommunityWeb.Schema, "priv/schema.graphql")

  test "can get all known links" do
    result = query_gql(variables: %{}, context: %{})
    assert {:ok, _query_data} = result
  end
end
