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
    Logger.info "init state #{:io_lib.format("~p", [state])}"
    :ok = :ranch.accept_ack(ref)
    Logger.info "accepted ack"

    {ok, closed, error} = transport.messages

    :ok = transport.setopts(socket, active: :once)
    Logger.info "set active"

    :gen_server.enter_loop(__MODULE__,
                           [],
                           struct(state,
                                  %{ok: ok, closed: closed, error: error}))
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

    for m <- ircmesgs do
      Logger.info :io_lib.format("~p", [m])
      process(m, user)
    end

    :ok = transport.setopts(socket, active: :once)

    {:noreply, %User{user | buf: new_buf}}
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
    transport.send(socket, ["332", " ", channel_name, " :", join_message])
    {:noreply, new_state}
  end

  def handle_cast(_request, state) do
    {:noreply, state}
  end

  def code_change(_old_vsn, state, _extra) do
    {:ok, state}
  end

  def terminate(_reason, _state) do
    :ok
  end

  defp process(join_message = %Message{command: "join"},
               user = %User{}) do
    ChannelRegistry.join(user, join_message)
  end

  defp process(privmsg_message = %Message{command: "privmsg",
                                          params: [destination | message]},
               user = %User{channels: channels}) do
    channel = Map.get(channels, destination, nil)
    case channel do
      %RegistryEntry{pid: channel_pid} ->
        Channel.privmsg(channel_pid,
                        String.join(message, " "))
      _ ->
        false
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
