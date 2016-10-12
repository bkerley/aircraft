defmodule Aircraft.ChannelRegistry do
  defstruct names: %{}, refs: %{}

  alias Aircraft.Channel
  alias Aircraft.ChannelRegistry
  alias Aircraft.Message
  alias Aircraft.RegistryEntry
  alias Aircraft.User

  @default_name :channel_registry

  use GenServer

  def start_link(name \\ @default_name) do
    GenServer.start_link(__MODULE__, %ChannelRegistry{}, name: name)
  end

  def join(user = %User{}, message = %Message{}, registry \\ @default_name) do
    GenServer.call(registry, {:join, user, message})
  end

  def init(state \\ %ChannelRegistry{}) do
    {:ok, state}
  end

  def handle_call({:join,
                   user = %User{},
                   _message = %Message{
                     command: "join",
                            params: [channels | rest]}
                  },
                  {user_pid, _user_tag},
                  state = %ChannelRegistry{names: names,
                                           refs: refs}) do
    channel_list = channels |> String.split(",")
    key_list = case rest do
                 [keys | _more] -> keys |> String.split(",")
                 _ -> []
               end
    new_state = channel_key_zip([], channel_list, key_list)
    |> Enum.reduce(state, &join(user, user_pid, names, &1, &2))

    {:reply,
     :ok,
     # new_state}
     state}
  end

  defp join(user = %User{channels: existing_memberships},
            user_pid,
            existing_channels,
            {channel_name, _key},
            state) do

    cond do
     Map.get(existing_memberships, channel_name, false) ->
        state
      channel = Map.get(existing_channels, channel_name, false) ->
        Channel.join(channel, user, user_pid)
        state
      true ->
        channel_pid = Channel.create(channel_name, user, user_pid)
        ref = Process.monitor(channel_pid)
        rec = %RegistryEntry{name: channel_name,
                             ref: ref,
                             pid: channel_pid}
        struct(state,
               names: Map.put(state.names, channel_name, rec),
               refs: Map.put(state.refs, ref, rec))
    end
  end

  defp channel_key_zip(acc, [], _keys) do
    acc
  end

  defp channel_key_zip(acc, [channel | more_channels], []) do
    channel_key_zip([{channel, nil} | acc], more_channels, [])
  end

  defp channel_key_zip(acc, [channel | more_channels], [key | more_keys]) do
    channel_key_zip([{channel, key} | acc], more_channels, more_keys)
  end
end
