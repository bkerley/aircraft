defmodule ChannelTest do
  use ExUnit.Case

  alias Aircraft.Channel

  test "chat a bit" do
    nick = "zedo"
    message = "hello"

    {:ok, channel_pid} = Channel.start_link

    :ok = Channel.join(channel_pid, nick)
    assert Channel.nicks(channel_pid) == [nick]

    :ok = Channel.privmsg(channel_pid, message)

    assert_receive({"#default", ^nick, ^message})
  end
end
