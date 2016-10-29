defmodule Aircraft.User do
  defstruct [:ok, :closed, :error,
             :nick, :username, :realname,
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

    :ok = UserRegistry.register(initial_nick)

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
    |> List.flatten
    |> Enum.filter(fn(reply) -> reply end)
    |> Enum.reverse
    |> Enum.each(fn(reply) ->
      :ok = transport.send(socket,
                           Message.to_iodata(reply))
    end)

    cond do
      Enum.any?(ircmesgs,
        fn(%Message{command: command}) -> command == "QUIT" end) ->
        {:stop, :normal, user}
      true ->
        :ok = transport.setopts(socket, active: :once)

        {:noreply,
         struct(updated_user, buf: new_buf)}
    end
  end

  def handle_info({closed, _socket},
                  user = %User{nick: nick, closed: closed}) do
    Logger.info "#{nick} closed connection"
    {:stop, :normal, user}
  end

  def handle_info(other, state) do
    {:noreply, state}
  end

  def handle_call(_request, _from, state) do
    {:reply, :ok, state}
  end

  def handle_cast({:reply, {_message = %Message{command: "332",
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

  def handle_cast({:reply, {:names, channel_name, name_list}},
                  state = %User{socket: socket, transport: transport}) do
    messages = name_list
    |> Enum.chunk(10)
    |> Enum.map(fn(name_chunk) ->
      %Message{command: "353", params: [channel_name,
                                        Enum.join(name_chunk, " ")]}
      |> Message.to_iodata
    end)

    :ok = transport.send(socket, messages)

    :ok = transport.send(socket,
                         Message.to_iodata(%Message{command: "366",
                                                    params: [channel_name,
                                                             "End of NAMES list"]}))

    {:noreply, state}
  end

  def handle_cast(message = %Message{},
                  state = %User{transport: transport,
                                socket: socket,
                                channels: channels}) do
    :ok = transport.send(socket, Message.to_iodata(message))
    {:noreply, state}
  end

  def handle_cast(response, state) do
    Logger.info "Unknown response to user #{state.nick}"
    {:noreply, state}
  end

  def code_change(_old_vsn, state, _extra) do
    {:ok, state}
  end

  def terminate(_reason, _state) do
    :ok
  end

  defp process(_message = %Message{command: "PING", params: []},
               user = %User{}) do
    {%Message{command: "PONG"}, user}
  end

  defp process(_message = %Message{command: "QUIT",
                                        params: params},
               user = %User{channels: channels}) do
    quit_message = case params do
                     [missive] -> missive
                     [] -> "deplaned"
                   end

    channels
    |> Map.values
    |> Enum.each(fn(%RegistryEntry{pid: channel_pid}) ->
      Channel.quit(channel_pid, quit_message)
    end)

    {false, user}
  end


  defp process(join_message = %Message{command: "JOIN"},
               user = %User{}) do
    ChannelRegistry.join(user, join_message)
    {false, user}
  end

  defp process(_privmsg_message = %Message{command: "PRIVMSG",
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

  defp process(_nick_message = %Message{command: "NICK",
                                        params: [new_nickname | _rest]},
               user = %User{nick: old_nickname, channels: channels}) do
    Logger.info("nick #{old_nickname} to #{new_nickname}")
    case UserRegistry.nick(old_nickname, new_nickname) do
      :ok ->
        new_nick_message = %Message{prefix: old_nickname,
                                    command: "NICK",
                                    params: [new_nickname]}

        for %RegistryEntry{pid: pid} <- channels do
          Channel.nick(pid, new_nick_message)
        end

        {new_nick_message,
         struct(user, nick: new_nickname)}
      :in_use ->
        {%Message{command: "436",
                  params: [new_nickname, "Nickname is already in use"]},
         user}
      other ->
        Logger.debug "nick #{old_nickname} to #{new_nickname}: #{other}"
        {%Message{command: "432",
                  params: [new_nickname,
                           "Unexpected response from user registry"]},
         user}
    end
  end

  defp process(_user_message = %Message{command: "USER",
                                        params: [username,
                                                 _hostname,
                                                 _servername,
                                                 realname]},
              user = %User{nick: nick}) do
    {[
      %Message{command: "001",
               params: [nick, "now you're flying with aircraft"]},
      %Message{command: "422",
               params: ["MOTD file missing"]}
    ],
     struct(user, username: username, realname: realname)}
  end

  defp process(_other_message = %Message{command: command,
                                        params: params},
               user = %User{nick: nick}) do
    Logger.info """
    unknown #{command} from #{user.nick} with #{params |> Enum.join(" ")}
    """
    {false, user}
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
