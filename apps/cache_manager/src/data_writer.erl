%%%-------------------------------------------------------------------
%%%
%%%
%%%
%%% @author wudeng256@qq.com
%%%-------------------------------------------------------------------

-module(data_writer).

-behaviour(gen_server).

-define(ERROR(FMT, Args), io:format(FMT, Args)).

-export([
    start_link/0,
    update/1,
    insert/1,
    delete/2
]).

-export([
    init/1, 
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2, 
    code_change/3 
]).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

update(Data) ->
    gen_server:cast(?MODULE, {update, Data}).

insert(Data) ->
    gen_server:cast(?MODULE, {insert, Data}).

delete(Table, Id) ->
    gen_server:cast(?MODULE, {delete, Table, Id}).


handle_call(Request, From, State) ->
    try 
        do_handle_call(Request, From, State) 
    catch
        Type:Exception ->
            ?ERROR("type:~p, exception:~p, Request:~p, state:~p, " "~nstacktrace:~p", [Type, Exception, Request, State, erlang:get_stacktrace()]),
            {reply, Type, State}
    end.

handle_cast(Info, State) ->
    try 
        do_handle_cast(Info, State) 
    catch
        Type:Exception ->
            ?ERROR("type:~p, exception:~p, Info:~p, state:~p, " "~nstacktrace:~p", [Type, Exception, Info, State, erlang:get_stacktrace()]),
            {noreply, State}
    end.

handle_info(Info, State) ->
    try 
        do_handle_info(Info, State) 
    catch
        Type:Exception ->
            ?ERROR("type:~p, exception:~p, Info:~p, state:~p, " "~nstacktrace:~p", [Type, Exception, Info, State, erlang:get_stacktrace()]),
            {noreply, State}
    end.

do_handle_call(_Request, _From, State) ->
    {reply, ok, State}.

do_handle_cast({update, Data}, State) ->
    model:update(Data),
    {noreply, State};

do_handle_cast({insert, Data}, State) ->
    model:insert(Data),
    {noreply, State};

do_handle_cast({delete, Table, Id}, State) ->
    model:delete(Table, Id),
    {noreply, State};

do_handle_cast(_Request, State) ->
    {noreply, State}.

do_handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

