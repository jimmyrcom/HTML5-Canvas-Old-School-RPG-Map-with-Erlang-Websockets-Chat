-module(es_websock).
-behaviour(gen_server).
-compile(export_all).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,terminate/2, code_change/3]).
-record(state, 
        { users = dict:new()
          , sock
          , increment = 0
        }).
-define(SERVER, ?MODULE).

start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
init([]) ->
    process_flag(trap_exit, true),
    case gen_tcp:listen(844, [binary, {packet, 0}, {active, true}, {reuseaddr, true}]) of
        {ok, S} -> 
            spawn_link(fun() -> connect:accept_connections(S) end),
            {ok,#state{sock=S}};
        Err -> 
            u:trace("Accept connections failed"),
            throw(Err)
    end.


stop() -> gen_server:call(?MODULE,die).
gs() -> gen_server:call(?MODULE,getState).
rs() -> gen_server:call(?MODULE,resetState).
say(User,Message) -> gen_server:call(?MODULE,{say,User,Message}).
register(Sock,User,X,Y) -> gen_server:call(?MODULE,{register,Sock,User,X,Y}).
checkUser(User,Sock) -> gen_server:call(?MODULE,{checkUser,User,Sock}).
allUsers(User) -> gen_server:call(?MODULE,{allUsers,User}).
logout(User) -> gen_server:call(?MODULE,{logout,User}).

handle_call(getState, _From, State) -> {reply,State,State};
handle_call({say,User,Message}, _From, State = #state{users=Users}) when Message=/=[] ->
    case dict:find(User,Users) of
        {ok, {Sock,X,Y,LastMessage}} ->
                                                                       %            u:trace("Flood Test",{Now,LastMessage,(Now-LastMessage)}),
            case (u:unixtime()-LastMessage)>1 of 
                true ->
                    SendToAll = fun(Key,Value) ->
                                        case Key=:=User of
                                            true -> void;
                                            false -> gen_tcp:send(element(1,Value),[0,"say @@@ ",User,"||",Message,255])
                                        end
                                end,
                    dict:map(SendToAll,Users);
                false ->
                    connect:alert(Sock,["Error: Flooding, Message not sent, wait ", integer_to_list(2-(u:unixtime()-LastMessage))," more seconds to post again"])
            end,
            {reply,ok,State#state{users=dict:store(User,{Sock,X,Y,u:unixtime()},Users)}};
        _ ->
            {reply,ok,State}
        end;
handle_call({register,Sock,User,X,Y}, _From, State = #state{users=Users}) ->
    case dict:find(User,Users) of
        {ok, Val} ->
            LastMessage=element(4,Val);
        _ ->
            LastMessage=u:unixtime()-5
    end,        
    SendToAll = fun(Key,Value) ->
                        case Key=:=User of
                            true -> void;
                            false -> gen_tcp:send(element(1,Value),[0,"move @@@ ",User,"||",X,"||",Y,255])
                        end
                end,
    dict:map(SendToAll,Users),
    {reply,ok,State#state{users=dict:store(User,{Sock,X,Y,LastMessage},Users)}};
handle_call({checkUser,User,Sock}, _From, State = #state{users=Users}) ->
    case dict:find(User,Users) of
        {ok,_} -> {reply,fail,State};
        error ->     {reply,go,State#state{users=dict:store(User,{Sock,"0","0",u:unixtime()-5},Users)}}
    end;
handle_call({logout,User}, _From, State=#state{users=Users}) ->
    SendToAll = fun(Key,Value) ->
                        case Key=:=User of
                            true -> void;
                            false -> gen_tcp:send(element(1,Value),[0,"logout @@@ ",User,255])
                        end
                end,
    dict:map(SendToAll,Users),
    {reply,ok,State#state{users=dict:erase(User,Users)}};
handle_call({allUsers,User}, _From, State=#state{users=Users}) ->
    Gather =
        fun(Key,{_,X,Y,_},Acc)->
                if User=/=Key -> [[",[\"",Key,"\",\"",X,"\",\"",Y,"\"]"]|Acc];
                   true -> Acc end                               
        end,
    case lists:flatten(dict:fold(Gather,[],Users)) of
        [_|Out] -> void;
        [] -> Out="[]"
    end,
    {reply,["[",Out,"]"],State};
handle_call(resetState, _From, _State) -> {reply,ok,#state{}};
handle_call(die, _From, State) -> {stop, normal, State};
handle_call(_Request, _From, State) ->
    u:trace("unknown gen_server:handle_call()",_Request),
    {reply, ok, State}.
handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    u:trace("gen_server:handle_info()",_Info),
    {noreply, State}.
terminate(_Reason, #state{sock=Sock} = State) ->
    gen_tcp:close(Sock),
    u:trace("gen_server:terminate()",{_Reason,State}),
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


s(S,Data) ->
    gen_tcp:send(S,Data).

%    case gen_tcp:listen(5555, [binary, {packet, 2}, {active, false}, {packet_size,1024*256}]) of
