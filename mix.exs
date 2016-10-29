defmodule Aircraft.Mixfile do
  use Mix.Project

  def project do
    [app: :aircraft,
     version: "0.1.0",
     elixir: "~> 1.3",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps()]
  end

  # Configuration for the OTP application
  #
  # Type "mix help compile.app" for more information
  def application do
    [applications: applications_for(Mix.env),
     mod: {Aircraft, []}]
  end

  defp applications_for(:dev), do: [:remix | applications_for(:any)]
  defp applications_for(_any), do: [:logger, :ranch]

  # Dependencies can be Hex packages:
  #
  #   {:mydep, "~> 0.3.0"}
  #
  # Or git/path repositories:
  #
  #   {:mydep, git: "https://github.com/elixir-lang/mydep.git", tag: "0.1.0"}
  #
  # Type "mix help deps" for more examples and options
  defp deps do
    [
      {:ranch, "~> 1.2"},
      {:irc_parser, "~> 0.1"},
      {:uuid, "~> 1.1"},
      {:remix, "~> 0.0.2", only: :dev}
    ]
  end
end
