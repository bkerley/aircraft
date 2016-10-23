defmodule Aircraft.UserRegistry do
  defstruct nicks: %{}, refs: %{}

  alias Aircraft.UserRegistry
  alias Aircraft.RegistryEntry

  @default_name :user_registry

  use GenServer

  def start_link(name \\ @default_name) do
    GenServer.start_link(__MODULE__, %UserRegistry{}, name: name)
  end

  def register(nick, registry \\ @default_name) do
    GenServer.call(registry, {:register, nick})
  end

  def nick(old_nickname, new_nickname, registry \\ @default_name) do
    GenServer.call(registry, {:nick, old_nickname, new_nickname})
  end

  def deregister(nick, registry \\ @default_name) do
    GenServer.cast(registry, {:deregister, nick})
  end

  def init(state \\ %UserRegistry{}) do
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

  def handle_call({:nick, old_nickname, new_nickname},
                  {user_pid, _user_tag},
                  state = %UserRegistry{nicks: nicks, refs: refs}) do
    if Map.has_key?(nicks, new_nickname) do
      :in_use
    else
      user = Map.get(nicks, old_nickname)
      modified_user = %RegistryEntry{user | name: new_nickname}

      new_nicks = nicks
      |> Map.delete(old_nickname)
      |> Map.put(new_nickname, modified_user)

      new_refs = refs
      |> Map.put(modified_user.ref,
                 modified_user)

      {:reply,
       :ok,
       struct(state, nicks: new_nicks, refs: new_refs)}
    end
  end

  def handle_cast({:deregister, nick},
                  state = %UserRegistry{nicks: nicks, refs: refs}) do
    new_state = case Map.get(nicks, nick) do
                  %RegistryEntry{name: nick,
                                  ref: ref} ->
                    Process.demonitor(ref)
                    struct(state,
                           nicks: Map.delete(nicks, nick),
                           refs: Map.delete(refs, ref))
                  _ -> state
                end
    {:noreply, new_state}
  end

  def handle_info({:DOWN, ref, :process, _pid, _reason},
                  state = %UserRegistry{nicks: nicks, refs: refs}) do
    new_state = case Map.get(refs, ref) do
                  %RegistryEntry{name: nick,
                                  ref: ^ref} ->
                    struct(state,
                           nicks: Map.delete(nicks, nick),
                           refs: Map.delete(refs, ref))
                  _ -> state
                end
    {:noreply, new_state}
  end
end
