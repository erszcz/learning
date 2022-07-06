defmodule Community.Seeds do
  alias Community.Repo

  alias Community.News.Link

  def run! do
    links = [
      %Link{url: "http://graphql.org/", description: "The Best Query Language"},
      %Link{url: "http://dev.apollodata.com/", description: "Awesome GraphQL Client"}
    ]

    Enum.each(links, &Repo.insert!/1)
    :ok
  end
end
