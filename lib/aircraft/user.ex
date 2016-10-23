defmodule Aircraft.User do
  defstruct [:ok, :closed, :error,
             :nick,
             :ref, :socket, :transport, :opts,
             channels: %{}, buf: ""]

  @behaviour :gen_server
  @behaviour :ranch_protocol

  require Logger

  alias Aircraft.Channel
  alias Aircraft.ChannelRegistry
  alias Aircraft.Message
  alias Aircraft.RegistryEntry
  alias Aircraft.User
  alias Aircraft.UserRegistry

  def start_link(ref, socket, transport, opts) do
    :proc_lib.start_link(__MODULE__,
                                 :init,
                                 [%User{nick: "zedo",
                                        ref: ref,
                                        socket: socket,
                                        transport: transport,
                                        opts: opts}])
  end

  def reply(user_pid, message) do
    :gen_server.cast(user_pid, {:reply, message})
  end

  def init(state = %User{ref: ref,
                         socket: socket,
                         transport: transport}) do
    :ok = :proc_lib.init_ack({:ok, self})
    :ok = :ranch.accept_ack(ref)

    {ok, closed, error} = transport.messages

    :ok = transport.setopts(socket, active: :once)

    initial_nick = UUID.uuid4

    UserRegistry.register(initial_nick)

    :gen_server.enter_loop(__MODULE__,
                           [],
                           struct(state,
                                  %{ok: ok, closed: closed, error: error,
                                    nick: initial_nick}))
  end

  def init(other) do
    Logger.info :io_lib.format("~p", [other])
  end

  def handle_info({ok, socket, data},
                  user = %User{ok: ok,
                               socket: socket,
                               transport: transport,
                               buf: buf}) do
    Logger.info data
    {ircmesgs, new_buf} = parse_mesg(buf <> data)

    {responses, updated_user} = ircmesgs
    |> Enum.reduce({[], user}, fn (message, {replies, user}) ->
      {new_reply, update_user} = process(message, user)
      {[new_reply | replies], update_user}
    end)

    responses
    |> Enum.filter(fn(reply) -> reply end)
    |> Enum.each(fn(reply) ->
      :ok = transport.send(socket,
                           Message.to_iodata(reply))
    end)

    :ok = transport.setopts(socket, active: :once)

    {:noreply, %User{updated_user | buf: new_buf}}
  end

  def handle_info(other, state) do
    Logger.info :io_lib.format("~p", [other])
    {:noreply, state}
  end

  def handle_call(_request, _from, state) do
    {:reply, :ok, state}
  end

  def handle_cast({:reply, {message = %Message{command: "332",
                                               params: [channel_name,
                                                        join_message]},
                            channel_pid}},
                  state = %User{transport: transport,
                                socket: socket,
                                channels: channels}) do
    ref = Process.monitor(channel_pid)
    rec = %RegistryEntry{name: channel_name,
                         pid: channel_pid,
                         ref: ref}
    new_state = struct(state, channels: Map.put(channels, channel_name, rec))
    :ok = transport.send(socket,
                         ["332", " ", channel_name, " :", join_message, "\r\n"])
    {:noreply, new_state}
  end

  def handle_cast(message = %Message{},
                  state = %User{transport: transport,
                                socket: socket,
                                channels: channels}) do
    IO.inspect message
    :ok = transport.send(socket, Message.to_iodata(message))
    {:noreply, state}
  end

  def handle_cast(request, state) do
    Logger.info "Unknown request to user #{state.nick}"
    IO.inspect request
    {:noreply, state}
  end

  def code_change(_old_vsn, state, _extra) do
    {:ok, state}
  end

  def terminate(_reason, _state) do
    :ok
  end

  defp process(join_message = %Message{command: "JOIN"},
               user = %User{}) do
    ChannelRegistry.join(user, join_message)
    {false, user}
  end

  defp process(privmsg_message = %Message{command: "PRIVMSG",
                                          params: [destination, message]},
               user = %User{channels: channels}) do
    channel = Map.get(channels, destination, nil)
    case channel do
      %RegistryEntry{pid: channel_pid} ->
        Channel.privmsg(channel_pid, message)
      _ ->
        false
    end
    {false, user}
  end

  defp process(nick_message = %Message{command: "NICK",
                                       params: [new_nickname | _rest]},
               user = %User{nick: old_nickname, channels: channels}) do
    case UserRegistry.nick(old_nickname, new_nickname) do
      :ok ->
        new_nick_message = %Message{prefix: old_nickname,
                                    command: "NICK",
                                    params: [new_nickname]}

        for %RegistryEntry{pid: pid} <- channels do
          Channel.nick(pid, new_nick_message)
        end

        {new_nick_message, %User{user | nick: new_nickname}}
      :in_use ->
        {%Message{command: "436",
                  params: [new_nickname, "Nickname is already in use"]},
         user}
      _other ->
        {%Message{command: "432",
                  params: [new_nickname,
                           "Unexpected response from user registry"]},
         user}
    end
  end

  defp parse_mesg(buf) do
    case String.split(buf, "\r\n") do
      [unfinished_mesg] -> {[], unfinished_mesg}
      multiple_mesgs ->
        finished_message_count = length(multiple_mesgs) - 1
        finished_messages = Enum.take(multiple_mesgs, finished_message_count)
        unfinished_message = List.last(multiple_mesgs)

        {Enum.map(finished_messages, &Message.parse(&1)),
         unfinished_message}
    end
  end
end
