%%%-------------------------------------------------------------------
%%%
%%%
%%%
%%% @author wudeng256@qq.com
%%%-------------------------------------------------------------------

-module(data_lock).

-behaviour(gen_server).

%-include("logger.hrl").

-define(ERROR(FMT, Args), io:format(FMT, Args)).

-export([
    start_link/1,
    lock/2,
    unlock/2
]).

-export([
    init/1, 
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2, 
    code_change/3 
]).

start_link(Table) ->
    Atom = spt_atom:atom_prefix(Table, lock),
    gen_server:start_link({local, Atom}, ?MODULE, [Table], []).

lock(Table, Key) ->
    Atom = spt_atom:atom_prefix(Table, lock),
    lock_(Atom, Key, 10).

lock_(Atom, Key, T) ->
    case gen_server:call(Atom, {lock, Key}) of
        ok -> ok;
        error -> 
            receive after T -> ok end,
            lock_(Atom, Key, 2*T)
    end.

unlock(Table, Key) ->
    Atom = spt_atom:atom_prefix(Table, lock),
    gen_server:call(Atom, {unlock, Key}).

init([Table]) ->
    {ok, {Table}}.

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

do_handle_call({lock, Key}, {Pid, _Tag}, State) ->
    Reply = case get({lock, Key}) of
        undefined -> 
            put({lock, Key}, Pid), 
            ok;
        Pid -> ok;
        _ -> error
    end,
    {reply, Reply, State};

do_handle_call({unlock, Key}, {Pid, _Tag}, State) ->
    Reply = case erase({lock, Key}) of
        Pid -> ok;
        undefined -> error;
        Other -> 
            put({lock, Key}, Other), 
            error
    end,
    {reply, Reply, State};


do_handle_call(_Request, _From, State) ->
    {reply, ok, State}.

do_handle_cast(_Request, State) ->
    {noreply, State}.

do_handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

