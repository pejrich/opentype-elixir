defmodule Opentype.Mixfile do
  use Mix.Project

  @version "0.5.0"

  def project do
    [
      app: :opentype,
      name: "OpenType",
      version: @version,
      elixir: "~> 1.5",
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      test_coverage: [tool: ExCoveralls],
      preferred_cli_env: [
        coveralls: :test,
        "coveralls.detail": :test,
        "coveralls.post": :test,
        "coveralls.html": :test
      ],
      description: "Provides facilities for working with OpenType fonts.",
      package: package(),
      deps: deps()
    ]
  end

  # Configuration for the OTP application
  #
  # Type "mix help compile.app" for more information
  def application do
    # Specify extra applications you'll use from Erlang/Elixir
    [extra_applications: [:logger]]
  end

  # Dependencies can be Hex packages:
  #
  #   {:my_dep, "~> 0.3.0"}
  #
  # Or git/path repositories:
  #
  #   {:my_dep, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
  #
  # Type "mix help deps" for more examples and options
  defp deps do
    [
      {:unicode_data, "~> 0.6.0"},
      {:excoveralls, "~> 0.8.1", only: :test},
      {:ex_doc, "~> 0.18.1", only: :dev}
    ]
  end

  defp package do
    [
      licenses: ["Apache 2.0"],
      name: "opentype",
      maintainers: ["jbowtie/John C Barstow"],
      links: %{
        "GitHub" => "https://github.com/jbowtie/opentype-elixir"
      }
    ]
  end
end
