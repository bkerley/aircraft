defmodule Aircraft.Message do
  defstruct prefix: "", command: "", params: []

  alias Aircraft.Message

  def parse(message) do
    message
    |> :irc_parser.parse
    |> record_to_struct
    |> normalize_command
  end

  def to_iodata(_message = %Message{prefix: prefix,
                                    command: command,
                                    params: params}) do
    [prefix_to_iodata(prefix),
     command,
     params_to_iodata(params),
     "\r\n"]
  end

  defp prefix_to_iodata(prefix) when is_bitstring(prefix) do
    [":", prefix, " "]
  end
  defp prefix_to_iodata(_no_prefix), do: []

  defp params_to_iodata([last_param]), do: [" :", last_param]
  defp params_to_iodata([param | more_params]) do
    [" ", param, params_to_iodata(more_params)]
  end
  defp params_to_iodata(_no_params), do: []

  defp record_to_struct({:ircmesg,
                        prefix,
                        command,
                        params}) do
    # assume prefix, command, and params are chardata
    %Message{prefix: IO.chardata_to_string(prefix),
             command: IO.chardata_to_string(command),
             params: Enum.map(params, &IO.chardata_to_string(&1))}
  end

  defp normalize_command(mesg = %Message{command: command}) do
    %Message{mesg | command: String.upcase(command)}
  end
end
