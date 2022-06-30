defmodule Community.NewsTest do
  use Community.DataCase

  alias Community.News

  describe "links" do
    alias Community.News.Link

    import Community.NewsFixtures

    @invalid_attrs %{description: nil, url: nil}

    test "list_links/0 returns all links" do
      link = link_fixture()
      assert News.list_links() == [link]
    end

    test "get_link!/1 returns the link with given id" do
      link = link_fixture()
      assert News.get_link!(link.id) == link
    end

    test "create_link/1 with valid data creates a link" do
      valid_attrs = %{description: "some description", url: "some url"}

      assert {:ok, %Link{} = link} = News.create_link(valid_attrs)
      assert link.description == "some description"
      assert link.url == "some url"
    end

    test "create_link/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = News.create_link(@invalid_attrs)
    end

    test "update_link/2 with valid data updates the link" do
      link = link_fixture()
      update_attrs = %{description: "some updated description", url: "some updated url"}

      assert {:ok, %Link{} = link} = News.update_link(link, update_attrs)
      assert link.description == "some updated description"
      assert link.url == "some updated url"
    end

    test "update_link/2 with invalid data returns error changeset" do
      link = link_fixture()
      assert {:error, %Ecto.Changeset{}} = News.update_link(link, @invalid_attrs)
      assert link == News.get_link!(link.id)
    end

    test "delete_link/1 deletes the link" do
      link = link_fixture()
      assert {:ok, %Link{}} = News.delete_link(link)
      assert_raise Ecto.NoResultsError, fn -> News.get_link!(link.id) end
    end

    test "change_link/1 returns a link changeset" do
      link = link_fixture()
      assert %Ecto.Changeset{} = News.change_link(link)
    end
  end
end
