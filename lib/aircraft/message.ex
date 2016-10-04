defmodule Aircraft.Message do
  defstruct prefix: "", command: "", params: []

  alias Aircraft.Message

  def parse(message) do
    message
    |> :irc_parser.parse
    |> record_to_struct
  end

  defp record_to_struct({:ircmesg,
                        prefix,
                        command,
                        params}) do
    # assume prefix, command, and params are chardata
    %Message{prefix: IO.chardata_to_string(prefix),
             command: IO.chardata_to_string(command),
             params: Enum.map(params, &IO.chardata_to_string(&1))}
  end
end
