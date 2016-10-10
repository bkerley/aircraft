defmodule Aircraft.Channel do
  defstruct name: "#default", nicks: %{}, pids: %{}, refs: %{}

  use GenServer

  alias Aircraft.Channel
  alias Aircraft.Message

  def start_link do
    GenServer.start_link(__MODULE__, %Channel{})
  end

  def init(state = %Channel{}) do
    {:ok, state}
  end

  def join(channel_pid, nick) do
    GenServer.call(channel_pid, {:join, nick})
  end

  def create(channel_name, owning_user, owning_user_pid) do
    {:ok, channel_pid} = GenServer.start_link(__MODULE__,
                                              %Channel{name: channel_name})
    GenServer.call(channel_pid, {:join, owning_user, owning_user_pid})
    channel_pid
  end

  def nicks(channel_pid) do
    GenServer.call(channel_pid, {:nicks})
  end

  def privmsg(channel_pid, message) do
    GenServer.call(channel_pid, {:privmsg, message})
  end

  def part(channel_pid, message) do
    GenServer.cast(channel_pid, {:part, message, self})
  end

  def handle_call({:join, nick},
                  {user_pid, _tag},
                  state = %Channel{nicks: nicks, pids: pids}) do
    ref = Process.monitor(user_pid)
    rec = %RegistryEntry{name: nick,
                         pid: user_pid,
                         ref: ref}
    {:reply,
     :ok,
     %Channel{state |
              nicks: Map.put(nicks, nick, rec),
              refs: Map.put(refs, ref, rec)
              pids: Map.put(pids, from_pid, rec)}}
  end

  def handle_call({:nicks}, _from, state = %Channel{nicks: nicks}) do
    {:reply, Map.keys(nicks), state}
  end

  def handle_call({:privmsg, message},
                  {from_pid, _tag},
                  state = %Channel{name: channel_name, pids: pids}) do
    from_nick = pids[from_pid]

    pids
    |> Map.keys
    |> Enum.each(&send(&1, {channel_name, from_nick, message}))

    {:reply, :ok, state}
  end

  def handle_cast({:part, message, user_pid},
                  state = %Channel{name: name, pids: pids}) do
    departed = pids[user_pid]

    fanout(pids.keys, [:part, name, departed.name, message])

    new_state = remove(state, departed)

    {:noreply, new_state}
  end

  def fanout(destination_pids, message) do
    destination_pids
    |> Enum.each(&send(&1, message))
  end
end
