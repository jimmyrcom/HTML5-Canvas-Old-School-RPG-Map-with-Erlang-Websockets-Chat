-module(es_websock).
-behaviour(gen_server).
-compile(export_all).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,terminate/2, code_change/3]).
-include("user.hrl").
-define(SERVER, ?MODULE).

%% Copyright (C) 2010 Jimmy Ruska (www.JimmyR.com,Youtube:JimmyRcom,Gmail:JimmyRuska), under GPL 2.0

%This code uses erlang OTP's gen_server to handle incoming data like movements, messages and registration
%All data is centralized in the server state and is lost on exit. At this time there is no centralized database
%though I don't plan on adding features that require one, like a player inventory or permanent statistics.

%There is a performance penalty from using modules, but putting all the code here gets ridiculous pretty fast
%It's hard to debug and read things when everything is nested, stacking functions on one page isn't any prettier


%% This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

%% This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

%% You should have received a copy of the GNU General Public License along with this program; if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA


start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
init([]) ->
    process_flag(trap_exit, true),
    %443
    case gen_tcp:listen(844, [binary, {packet, 0}, {active, true}, {reuseaddr, true}, {packet_size,1024*2}]) of
        {ok, S} -> 
            spawn(fun() -> connect:accept_connections(S) end),
            {ok,#state{sock=S}};
        Err -> 
            u:trace("Accept connections failed"),
            throw(Err)
    end.

debug() -> gen_server:call(?MODULE,debug).
stop() -> gen_server:call(?MODULE,die).
gs() -> gen_server:call(?MODULE,getState).
rs() -> gen_server:call(?MODULE,resetState).

sendToAll(Dict,You,Message) ->
    dict:map(fun(ID,_) when ID=:=You -> void;
                (_,Record) -> gen_tcp:send(Record#user.sock,[0,Message,255])
             end,Dict).

say(Simple,Message) -> gen_server:cast(?MODULE,{say,Simple,Message}).
move(Simple,X,Y) -> gen_server:cast(?MODULE,{move,Simple,X,Y}).
logout(Simple) -> gen_server:cast(?MODULE,{logout,Simple}).

checkUser(State) -> gen_server:call(?MODULE,{checkUser,State}).

handle_call({checkUser,UserState}, _, State) -> checkUser:checkUser(UserState,State);
handle_call(getState, _From, State) -> {reply,State,State};
handle_call(debug, _From, State) ->
    #state{lookupByID=LBID,lookupByName=LBName,lookupByIP=LBIP,maps=Maps} = State,
    u:trace(dict:to_list(array:get(0,Maps))),
    u:trace(gb_trees:to_list(LBName)),
    u:trace(gb_trees:to_list(LBIP)),
    u:trace(dict:to_list(LBID)),
    {reply,ok,State};
handle_call(resetState, _From, _State) -> {reply,ok,#state{}};
handle_call(die, _From, State) -> {stop, normal, State};
handle_call(_Request, _From, State) ->
    u:trace("unknown gen_server:handle_call()",_Request),
    {reply, ok, State}.

handle_cast({say,Simple,Message}, State) when Message=/="" -> say:say(Simple,Message,State);
handle_cast({say,Simple,X,Y}, State)  -> move:move(Simple,X,Y,State);

handle_cast({logout,#simple{id=ID,map=Map}}, State=#state{maps=Maps}) ->
    MapDict=array:get(Map,Maps),
    case dict:find(ID,MapDict) of
        {ok, #user{user=User,pid=Pid}} ->
            Pid ! {die,"Disconnected"},
            sendToAll(MapDict,ID,["logout @@@ ",User]),
            {reply,ok,State#state{maps=map:set(Map,dict:erase(ID,MapDict),Maps)}};
        _ -> {reply,State}
    end;
handle_cast(_Msg, State) ->
    u:trace("gen_server:cast()",_Msg),
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
