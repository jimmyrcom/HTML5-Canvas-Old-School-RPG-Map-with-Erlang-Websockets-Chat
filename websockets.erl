-module(websockets).
-export([handshake/1,handshake/2,msg/2,msg/3,alert/2,die/2]).
-record(websock,{key1,key2,allowed,origin,host,request,port,callback,callbackData=[]}).
-define(AllowedOrigin,
        [ <<"rp.eliteskills.com">>
              , <<"jimmyr.com">>
              , <<"localhost">>
              , <<"76.74.253.61.844">>
        ]).

%% Copyright (C) 2010 Jimmy Ruska (www.JimmyR.com,Youtube:JimmyRcom,Gmail:JimmyRuska), under GPL 2.0
%You give it a websockets handshake and it returns a proper response. Accepts a Fun as callback
%in order to parse things like cookies or protocol.

%% This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.
 
%% This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

%% You should have received a copy of the GNU General Public License along with this program; if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

handshake(Bin) -> handshake(Bin,false).
handshake(Bin,Callback) ->
    case binary:split(Bin,<<16#0d0a0d0a:32>>) of
        [HttpRequest|[Data]] -> void;
        [HttpRequest] -> Data = void
    end,
    Fields = binary:split(HttpRequest,<<16#0d0a:16>>,[global]),
    #websock{
                key1=Key1
              , key2=Key2
              , origin=Origin
              , request=Request
              , host=Host
              , port=Port
            } = parseKeys(Fields,#websock{allowed=?AllowedOrigin,callback=Callback}),

     case (Key1=:=undefined orelse Key2=:=undefined) of
         false -> NewWay=true;
         true -> NewWay=false
     end,
    ["HTTP/1.1 101 ",case NewWay of true-> "WebSocket"; false-> "Web Socket" end," Protocol Handshake\r\n",
     "Upgrade: WebSocket\r\n",
     "Connection: Upgrade\r\n",
     case NewWay of
         true ->
             ["Sec-WebSocket-Origin: ",Origin,"\r\n",
              "Sec-WebSocket-Location: ws://",Host,":",integer_to_list(Port),Request,"\r\n",
              "Sec-WebSocket-Protocol: sample\r\n\r\n",
              erlang:md5(<<Key1:32, Key2:32,Data/binary>>)
             ];
         false ->
             ["WebSocket-Origin: ",Origin,"\r\n",
              "WebSocket-Location: ws://",Host,":",integer_to_list(Port),Request,"\r\n\r\n"
             ]
     end
    ].

alert(ClientS,MSG) -> msg(ClientS,"alert",MSG).
msg(ClientS,MSG) -> gen_tcp:send(ClientS,[0,MSG,255]).
msg(ClientS,Type,MSG) -> gen_tcp:send(ClientS,[0,Type,<<" @@@ ">>,MSG,255]).

die(ClientS,MSG) ->
    alert(ClientS,MSG),
    gen_tcp:send(ClientS,[255,0]),
    gen_tcp:send(ClientS,[0,0,0,0,0,0,0,0,0]),
    gen_tcp:close(ClientS),
    u:trace(MSG).

parseKeys([<<"Sec-WebSocket-Key1: ",Key/binary>>|T],Websock) ->
    parseKeys(T,Websock#websock{key1=genKey(Key,[],0)});
parseKeys([<<"Sec-WebSocket-Key2: ",Key/binary>>|T],Websock) ->
    parseKeys(T,Websock#websock{key2=genKey(Key,[],0)});
parseKeys([<<"Origin: ",Origin/binary>>|T],Websock) ->   
    parseKeys(T,Websock#websock{origin=Origin});
parseKeys([<<"Host: ",Host/binary>>|T],Websock) ->
    [Host1,Port] = binary:split(Host,<<$:>>),
    parseKeys(T,Websock#websock{host=Host1,port=list_to_integer(binary_to_list(Port))});
parseKeys([<<"GET ",Request/binary>>|T],Websock) ->
    Size = byte_size(Request)-9,
    <<Request1:Size/binary,_/binary>> = Request,
    parseKeys(T,Websock#websock{request = Request1});
parseKeys([],W) when 
      W#websock.origin=/=undefined andalso
      W#websock.host=/=undefined
      ->

    case  W#websock.allowed of
        any ->
            Test=true;
        Allowed ->
            [_|Origin] = re:replace(W#websock.origin,"http://(www\.)?","",[caseless]),
            Test = lists:any(fun(Host) when Host=:=Origin -> true; (_) -> false end,Allowed)
    end,

    case Test of
        true -> W;
        false ->
            u:trace(W),
            throw("No matching allowed hosts")
    end;
parseKeys([],W) ->
    u:trace(W),
    throw("Missing Information");
parseKeys([_|T],W) when W#websock.callback=/=false ->
    F=W#websock.callback,
    parseKeys(T,W#websock{callbackData=F()});
parseKeys([_|T],Websock) -> parseKeys(T,Websock).

genKey(<<X:8,Rest/binary>>,Numbers,Spaces) when X>47 andalso X<58 ->
    genKey(Rest,[X|Numbers],Spaces);
genKey(<<>>,Numbers,Spaces) ->
%    u:trace("Key: ",Numbers),
    list_to_integer(lists:reverse(Numbers)) div Spaces;
genKey(<<$ :8,Rest/binary>>,Numbers,Spaces) ->
    genKey(Rest,Numbers,Spaces+1);
genKey(<<_:8,Bin/binary>>,Numbers,Spaces) ->
    genKey(Bin,Numbers,Spaces).


