defmodule Aircraft.Channel do
  defstruct [:owner, name: "#default", nicks: %{}, pids: %{}, refs: %{}]

  use GenServer

  require Logger

  alias Aircraft.Channel
  alias Aircraft.Message
  alias Aircraft.RegistryEntry
  alias Aircraft.User

  def start_link do
    GenServer.start_link(__MODULE__, %Channel{})
  end

  def init(state = %Channel{}) do
    {:ok, state}
  end

  def join(channel_pid, nick) do
    join(channel_pid, nick, self)
  end

  def join(channel_pid, user, user_pid) do
    GenServer.call(channel_pid, {:join, user, user_pid})
  end

  def create(channel_name, owning_user, owning_user_pid) do
    {:ok, channel_pid} = GenServer.start_link(__MODULE__,
                                              %Channel{name: channel_name})
    GenServer.call(channel_pid, {:create, owning_user, owning_user_pid})
    channel_pid
  end

  def nicks(channel_pid) do
    GenServer.call(channel_pid, {:nicks})
  end

  def privmsg(channel_pid, message) do
    GenServer.call(channel_pid, {:privmsg, message})
  end

  def nick(channel_pid, message) do
    GenServer.cast(channel_pid, {:nick, message})
  end

  def part(channel_pid, message) do
    GenServer.cast(channel_pid, {:part, message, self})
  end

  def handle_call({:create, user = %User{}, user_pid},
                  {_registry_pid, _registry_tag},
                  state = %Channel{}) do

    Logger.info "creating channel #{state.name} for #{user.nick}"
    perform_join(state, user, user_pid)
  end

  def handle_call({:join, user, user_pid},
                  {sender_pid, _tag},
                  state = %Channel{}) do
    perform_join(state, user, user_pid)
  end

  def handle_call({:nicks}, _from, state = %Channel{nicks: nicks}) do
    {:reply, Map.keys(nicks), state}
  end

  def handle_call({:privmsg, message},
                  {from_pid, _tag},
                  state = %Channel{name: channel_name, pids: pids}) do
    %RegistryEntry{name: from_nick} = pids[from_pid]

    fanout(Map.keys(pids), %Message{prefix: from_nick,
                                    command: "PRIVMSG",
                                    params: [channel_name, message]})

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
    |> Enum.each(&GenServer.cast(&1, message))
  end

  defp perform_join(state = %Channel{name: name,
                             nicks: nicks,
                             pids: pids,
                             refs: refs},
            user = %User{nick: nick},
            user_pid) do
    ref = Process.monitor(user_pid)
    rec = %RegistryEntry{name: nick,
                         pid: user_pid,
                         ref: ref}

    User.reply(user_pid, {%Message{command: "332",
                                   params: [name, "No topic is set"]},
                          self})
    {:reply,
     :ok,
     %Channel{state |
              nicks: Map.put(nicks, nick, rec),
              refs: Map.put(refs, ref, rec),
              pids: Map.put(pids, user_pid, rec)}}
  end

  defp remove(state = %Channel{nicks: nicks, refs: refs, pids: pids},
              departed = %RegistryEntry{}) do
    struct(state, nicks: Map.delete(nicks, departed.name),
           refs: Map.delete(refs, departed.ref),
           pids: Map.delete(pids, departed.pid))
  end
end
