import Config

config :iex, default_prompt: ">>>"

config :config_runtime_test,
  opt1: "runtime value 1"

config :config_runtime_test, ConfigRuntime,
  opt1: "ConfigRuntime runtime value 1"
