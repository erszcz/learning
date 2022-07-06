defmodule CommunityWeb.GraphQL.LinksTest do
  use Community.DataCase, asnyc: true
  use Wormwood.GQLCase

  load_gql(CommunityWeb.Schema, "test/support/schema/queries/allLinks.gql")

  test "can get all known links" do
    result = query_gql(variables: %{}, context: %{})
    assert {:ok, %{data: %{"query" => links}}} = result
    assert length(links) == 2
    assert links |> Enum.sort() == [%{"id" => "1"}, %{"id" => "2"}]
  end
end
