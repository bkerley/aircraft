defmodule Aircraft.Channel do
  defstruct name: "#default", nicks: %{}, pids: %{}

  use GenServer

  alias Aircraft.Channel

  def start_link do
    GenServer.start_link(__MODULE__, %Channel{})
  end

  def init(state = %Channel{}) do
    {:ok, state}
  end

  def join(channel_pid, nick) do
    GenServer.call(channel_pid, {:join, nick})
  end

  def nicks(channel_pid) do
    GenServer.call(channel_pid, {:nicks})
  end

  def privmsg(channel_pid, message) do
    GenServer.call(channel_pid, {:privmsg, message})
  end

  def handle_call({:join, nick},
                  {from_pid, _tag},
                  state = %Channel{nicks: nicks, pids: pids}) do
    {:reply,
     :ok,
     %Channel{state |
              nicks: Map.put(nicks, nick, from_pid),
              pids: Map.put(pids, from_pid, nick)}}
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
end
