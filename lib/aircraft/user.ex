defmodule Aircraft.User do
  defstruct [:nick, :ref, :socket, :transport, :opts]

  use GenServer

  alias Aircraft.User
  alias Aircraft.Channel

  def start_link(ref, socket, transport, opts) do
    GenServer.start_link(__MODULE__,
                         %User{nick: "zedo",
                               ref: ref,
                               socket: socket,
                               transport: transport,
                               opts: opts})
  end

  def init(state = %User{ref: ref,
                         socket: socket,
                         transport: transport}) do
    :ok = :ranch.accept_ack(ref)
    :ok = transport.setopts(socket, active: :once)

    {:ok, state}
  end

  def handle_info({:tcp, socket, data},
                  %User{nick: nick,
                        socket: socket}) do

  end
end
