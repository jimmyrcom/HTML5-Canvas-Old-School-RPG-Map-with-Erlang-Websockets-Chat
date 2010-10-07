-module(connect).
-export([accept_connections/1,alert/2]).
-record(state,{sock,user,x,y,lastMessage}).
-record(websock,{key1,key2,allowed,origin,host,request,port}).
%Created by Jimmy Ruska under GPL

accept_connections(S) ->
    {ok, ClientS} = gen_tcp:accept(S),
    u:say("Connection request received"),
    spawn(fun() -> accept_connections(S) end),
    receive {tcp,_,Bin} ->
            [HttpRequest|[Data]] = binary:split(Bin,<<16#0d0a0d0a:32>>),
            Fields = binary:split(HttpRequest,<<16#0d0a:16>>,[global]),
            AllowedOrigin = 
                [ <<"rp.eliteskills.com">>
                      , <<"jimmyr.com">>
                      , <<"localhost">>
                      , <<"76.74.253.61.844">>
                ],
%		            u:trace(HttpRequest,Data),
            #websock{
                        key1=Key1
                      , key2=Key2
                      ,  origin=Origin
                      , request=Request
                      , host=Host
                      , port=Port
                    } = parseKeys(Fields,#websock{allowed=AllowedOrigin}),
            Handshake = 
                [
                 "HTTP/1.1 101 WebSocket Protocol Handshake\r\n",
                 "Upgrade: WebSocket\r\n",
                 "Connection: Upgrade\r\n",
                 "Sec-WebSocket-Origin: ",Origin,"\r\n",
                 "Sec-WebSocket-Location: ws://",Host,":",integer_to_list(Port),Request,"\r\n",
                 "Sec-WebSocket-Protocol: sample\r\n\r\n",
                 erlang:md5(<<Key1:32, Key2:32,Data/binary>>)
                ],
%            u:trace(Handshake),
            gen_tcp:send(ClientS, Handshake),
            step2(ClientS)
    after 30000 ->
            die(ClientS,"Timeout on Handshake")
    end.

step2(ClientS) ->
    receive {tcp,_,Bin1} ->
           ["move",User,X,Y] = string:tokens(binary_to_list(binary:part(Bin1,1,byte_size(Bin1)-2)),"||"),
            case es_websock:checkUser(User,ClientS) of
                fail ->
                    die(ClientS,"Already Connected");
                go ->
                    gen_tcp:send(ClientS,[0,"all @@@ ",es_websock:allUsers(User),255]),
                    es_websock:register(ClientS,User,X,Y),
                    client(#state{user=User,x=X,y=Y,lastMessage=0,sock=ClientS})
            end
    after 30 * 1000 ->
            die(ClientS,"Timeout on Handshake")
    end.

alert(ClientS,MSG) ->    
    gen_tcp:send(ClientS,[0,"Alert @@@ ",MSG,255]).

die(ClientS,MSG) ->
    alert(ClientS,MSG), 
    gen_tcp:send(ClientS,[0,0,0,0,0,0,0,0,0]),
    gen_tcp:close(ClientS).
   
client(State) ->
    receive
        {tcp,_,Bin} -> 
            Bin1 = binary_to_list(binary:part(Bin,1,byte_size(Bin)-2)),
%            gen_tcp:send(State#state.sock,[0,"Sup brohan",255]),
%            u:trace("Received From Client: ",Bin1),
            actions(State,string:tokens(Bin1,"||")),
            client(State);
        Other ->
            other(State,Other)
    after 60*10 * 1000 ->
            es_websock:logout(State#state.user),
            die(State#state.sock,"Disconnected from server: IDLE Timeout")
    end.

actions(State,Data) ->
    case Data of
        ["move",User,X,Y] ->  
            es_websock:register(State#state.sock,User,X,Y);
        ["say",User,Message] ->
            u:trace(User,Message),
            es_websock:say(User,Message);
        _ ->
            u:trace("Unidentified Message",Data)                
    end.
    

%I can't match =:={error,_}?
other(State,What)->
    es_websock:logout(State#state.user),    
    io:format("Received close socket from client ~p~n",[What]),
    gen_tcp:close(State#state.sock).


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
      W#websock.key1=/=undefined andalso
      W#websock.key2=/=undefined andalso
      W#websock.origin=/=undefined andalso
      W#websock.host=/=undefined
      ->
    [_|Origin] = re:replace(W#websock.origin,"http://(www\.)?","",[caseless]),
    u:trace("Origin: ",Origin),
    Test = lists:any(
             fun(Host) when Host=:=Origin -> true; (_) -> false
             end,
             W#websock.allowed),
    case Test of
        true -> W;
        false ->
            u:trace(W),
            throw("No matching allowed hosts")
    end;
parseKeys([],W) ->
    u:trace(W),
    throw("Missing Information");
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
