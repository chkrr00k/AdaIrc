with Ada.Text_IO, GNAT.Sockets;
use Ada.Text_IO, GNAT.Sockets;
with Ada.Streams, Ada.Strings, Ada.Strings.Unbounded, Ada.Strings.Fixed, Ada.Strings.Equal_Case_Insensitive;
use Ada.Strings, Ada.Strings.Unbounded, Ada.Strings.Fixed;
with Ada.Containers.Doubly_Linked_Lists;
use Ada.Containers;
with Ada.Strings.Unbounded.Text_IO;
use Ada.Strings.Unbounded.Text_IO;
-- "proudly" made by chkrr00k;
-- & released in GPLv3 license;


procedure irc is

	host: string := "irc.mozilla.org";
	port: port_type := 6667;


	type message is record 
		m: unbounded_string;
	end record;

	package Qg is
		procedure Push(e: in message);
		function Pop return message;
		function Size return integer;
		Empty: exception;
	end;
	package body Qg is
		package Msg_c is new Doubly_Linked_Lists(message);
		use Msg_c;
		s: List;
		
		procedure Push(e: in message) is
		begin
			Append(s, e);
		end Push;
		function Pop
			return message
		is 
			result: message;
		begin
			if Is_Empty(s) then raise Empty;
			end if;
			result := First_Element(s);
			Delete_First(s);
			return result;
		end Pop;
		function Size
			return integer
		is begin
			return Integer(Length(s));
		end Size;
	end Qg;

	task type connection;

	task type ircServer is
		entry get(msg: out unbounded_string);
		entry send(msg: string);
	end ircServer;
	
	channel: Stream_Access;

	endl: String := (1=>ASCII.CR, 2=>ASCII.LF);

	procedure recv(ch: in Stream_Access; result: out unbounded_string) 
	is
		c: character;       
		res: unbounded_string;
	begin loop character'read(ch, c);
		if not (c = ASCII.CR or c = ASCII.LF) then
			append(res, c);
		end if;

		exit when c = ASCII.CR;
		end loop;
		result := Trim(res, Both);
	end;			
	procedure send(ch: Stream_Access; m: string)
	is begin
		string'write(ch, m);
	end;
	function init(addr: sock_addr_type)
		return Stream_Access
	is 
		cl: Socket_Type;
	begin
		Create_Socket(cl);
		Connect_Socket(cl, addr);
		return Stream(cl);
	end;
	function resolve(url: string; port: port_type)
		return Sock_Addr_Type
	is
		result: Sock_Addr_Type;
	begin
		result.Addr := Addresses(Get_Host_By_Name(url), 1);
		result.port := port;
		return result;
	end;

	function startsWith(input: string; prefix: string)
		return boolean
	is begin
		return Ada.Strings.Equal_Case_Insensitive(Head(input, prefix'length), prefix);
	end;

	procedure connect(nick: string; ident: string; host: string; realname: string; ch: Stream_Access)
	is begin
		send(ch, "NICK " & nick & endl);
		send(ch, "USER " & ident & " " & host & " ayy :" & realname & endl);
	end;

	procedure pong(msg: string; ch: Stream_Access)
	is begin
		send(ch, "PONG " & msg(msg'First + 6..msg'Last) & endl);
	end;
	
	task body connection is
		r: unbounded_string;
		PING: constant string := "ping";
		msg: message;
	begin
		Put_Line("Started");
		GNAT.Sockets.Initialize;
		channel := init(resolve(host, port));
		
		connect("adasys", "a", "b", "bot", channel);
		loop
			recv(channel, r);
			if startsWith(to_string(r), PING) then
				Put_Line(">pong");
				pong(to_string(r), channel);
			else
				msg := (m => r);
				Qg.Push(msg);

				Put_Line("<" & to_string(r));

			end if;
		end loop;
	end connection;

	task body ircServer is
		cc: connection;
	begin
		loop select when Qg.Size > 0 => accept get(msg: out unbounded_string) do
				msg := Qg.Pop.m;
			end;
			or accept send(msg: string) do
				send(channel, msg);
			end;
		end select;
		end loop;
	end ircServer;

-- client implementation


	i: ircServer;

	task type printer;
	task body printer is
		m: unbounded_string;
	begin
		loop
			i.get(m);
			Put_Line(m);
		end loop;
	end printer;

	input: unbounded_string;
begin
--	Put_Line("Message:");
	loop
		input := Get_Line;
		i.send(to_string(input) & endl);
	end loop;
end irc;
