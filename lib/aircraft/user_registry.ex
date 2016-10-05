defmodule Aircraft.UserRegistry do
  defstruct nicks: %{}, refs: %{}

  alias Aircraft.UserRegistry
  alias Aircraft.RegistryEntry

  @default_name :user_registry

  use GenServer

  def start_link(name // @default_name) do
    GenServer.start_link(__MODULE__, [], name: name)
  end

  def register(nick, registry // @default_name) do
    GenServer.call(registry, {:register, nick})
  end

  def deregister(nick, registry // @default_name) do
    GenServer.cast(registry, {:deregister, nick})
  end

  def init(state = %UserRegistry) do
    {:ok, state}
  end

  def handle_call({:register, nick},
                  {user_pid, _user_tag},
                  state = %UserRegistry{nicks: nicks, refs: refs}) do
    ref = Process.monitor(user_pid)

    rec = %RegistryEntry{name: nick,
                         pid: user_pid,
                         ref: ref}

    {:reply,
     :ok,
     struct(state,
            nicks: Map.put(nicks, nick, rec),
            refs: Map.put(refs, ref, rec))}
  end

  def handle_cast({:deregister, nick},
                  state = %UserRegistry{nicks: nicks, refs: refs}) do
    new_state = case Map.get(nicks, nick) do
                  %RegistryEntry{name: nick,
                                 pid: user_pid,
                                  ref: ref} ->
                    Process.demonitor(ref)
                    struct(state,
                           nicks: Map.delete(nicks, nick),
                           refs: Map.delete(refs, ref))
                  _ -> state
                end
    {:noreply, new_state}
    end
  end
end
