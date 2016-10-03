defmodule Aircraft.Listener do
  alias Aircraft.User

  def start_link do
    opts = [port: e(:port),
            max_connections: e(:max_connections)]
IO.inspect opts
    {:ok, _} = :ranch.start_listener(:aircraft,
                                     e(:listener_count),
                                     :ranch_tcp,
                                     opts,
                                     User,
                                     [])
    end

  defp e(var_name) do
    Application.get_env(:aircraft, var_name)
  end
end
