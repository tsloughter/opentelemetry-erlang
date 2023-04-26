defmodule OpenTelemetry.MixProject do
  use Mix.Project

  def project do
    {app, desc} = load_app()

    [
      app: app,
      version: to_string(Keyword.fetch!(desc, :vsn)),
      description: to_string(Keyword.fetch!(desc, :description)),
      elixir: "~> 1.8",
      start_permanent: Mix.env() == :prod,
      deps: [
        {:opentelemetry_semantic_conventions, "~> 0.2"},
        {:dialyxir, "~> 1.0", only: [:dev], runtime: false},
        {:covertool, ">= 0.0.0", only: :test},
        {:gradient, github: "esl/gradient", only: [:dev], runtime: false}
      ],
      name: "OpenTelemetry API",
      test_coverage: [tool: :covertool],
      package: package(),
      aliases: [docs: & &1],
      dialyzer: [ignore_warnings: "dialyzer.ignore-warnings", remove_defaults: [:unknown]]
    ]
  end

  def application, do: []

  defp package() do
    [
      description: "OpenTelemetry API",
      build_tools: ["rebar3", "mix"],
      files: ~w(lib mix.exs README.md LICENSE rebar.config include src),
      licenses: ["Apache-2.0"],
      links: %{
        "GitHub" => "https://github.com/open-telemetry/opentelemetry-erlang",
        "OpenTelemetry.io" => "https://opentelemetry.io"
      }
    ]
  end

  defp load_app do
    {:ok, [{:application, name, desc}]} = :file.consult('src/opentelemetry_api.app.src')

    {name, desc}
  end
end
