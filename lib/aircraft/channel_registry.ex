defmodule Aircraft.ChannelRegistry do
  defstruct names: %{}, refs: %{}

  alias Aircraft.ChannelRegistry

  @default_name :channel_registry

  use GenServer

  def start_link(name // @default_name) do
    GenServer.start_link(__MODULE__, [], name: name)
  end

  def join(user = %User{}, message = %Message{}, registry // @default_name) do
    GenServer.call(registry, {:join, user, message})
  end

  def init(state // %ChannelRegistry{}) do
    {:ok, state}
  end

  def handle_call({join,
                   user = %User{nick: nick,
                                channels: channels},
                   _message = %Message{command: "join",
                                       params: [channels | rest]}},
                  {user_pid, _user_tag},
                  state = %ChannelRegistry{names: names,
                                           refs: refs}) do
    channel_list = channels |> String.split(",")
    key_list = case rest do
                 [keys | _more] -> keys |> String.split(",")
                 _ -> []
               end
    joined_channels = channel_key_zip([], channel_list, key_list)
    |> Enum.map(&join(user, user_pid, names, &1))
  end

  defp join(user = %User{channels: existing_memberships},
            user_pid,
            existing_channels
            {channel_name, _key}) do
    cond do
      %RegistryEntry{} = Map.get(existing_memberships, channel_name) ->
        channel_name
      channel = %Channel{} = Map.get(existing_channels, channel_name) ->
        Channel.join(channel, user, user_pid)
      _ ->
        Channel.create(channel_name, user, user_pid)
    end
  end

  defp channel_key_zip(acc, [], _keys) do
    acc
  end

  defp channel_key_zip(acc, [channel | more_channels], []) do
    channel_key_zip([{channel, nil} | acc], more_channels. [])
  end

  defp channel_key_zip(acc, [channel | more_channels], [key | more_keys]) do
    channel_key_zip([{channel, key} | acc], more_channels, more_keys)
  end
end
