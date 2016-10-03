defmodule Aircraft.User do
  defstruct [:ok, :closed, :error,
             :nick, :ref, :socket, :transport, :opts, :buf]

  @behaviour :gen_server
  @behaviour :ranch_protocol

  require Logger

  alias Aircraft.User
  alias Aircraft.Channel

  def start_link(ref, socket, transport, opts) do
    :proc_lib.start_link(__MODULE__,
                                 :init,
                                 [%User{nick: "zedo",
                                       ref: ref,
                                       socket: socket,
                                       transport: transport,
                                       opts: opts}])
  end



  def init(state = %User{ref: ref,
                         socket: socket,
                         transport: transport}) do
    :ok = :proc_lib.init_ack({:ok, ref})
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

    Logger.info "parsed #{length(ircmesgs)} messages"

    :ok = transport.setopts(socket, active: :once)

    {:ok, %User{user | buf: new_buf}}
  end

  def handle_info(other, state) do
    Logger.info :io_lib.format("~p", [other])
    {:ok, state}
  end

  def handle_call(_request, _from, state) do
    {:reply, :ok, state}
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

  defp parse_mesg(buf) do
    case String.split(buf, "\r\n") do
      [unfinished_mesg] -> {[], unfinished_mesg}
      multiple_mesgs ->
        finished_message_count = length(multiple_mesgs) - 1
        finished_messages = Enum.take(multiple_mesgs, finished_message_count)
        unfinished_message = List.last(multiple_mesgs)

        {Enum.map(finished_messages, &:irc_parser.parse(&1)),
         unfinished_message}
    end
  end
end
