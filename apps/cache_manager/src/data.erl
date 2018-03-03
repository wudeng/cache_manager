%%%-------------------------------------------------------------------
%%%
%%%
%%%
%%% @author wudeng256@qq.com
%%%-------------------------------------------------------------------

-module(data).

-export([
    lookup/2,
    lookup_/2,
    synchronized/3,
    update/1
]).

%% cache aside 
%% undefined: cache miss, 需要去数据库中加载
%% not_exist: cache hit , 数据库中也没有, 防止缓存穿透
%% Record   : cache hit , 返回缓存中的数据
%%
%% 从ets表里面读数据，如果读不到则从数据库中加载
%% 向ets表更新数据，同时通知持久化进程写入数据库


%% 单线程访问的可以直接读
%% 防止缓存穿透
lookup(Table, Key) ->
    case cache:get(Table, Key) of
        undefined ->
            Value = model:select(Table, Key), %% 返回Record或者not_exist
            cache:put(Table, Key, Value),
            Value;
        Val ->
            Val
    end.

%% 支持并发读，要加锁
%% 防止缓存击穿
lookup_(Table, Key) ->
    case cache:get(Table, Key) of
        undefined ->
            data_lock:lock(Table, Key),
            Value = lookup(Table, Key),
            data_lock:unlock(Table, Key),
            Value;
        Val ->
            Val
    end.


%% 更新
update(Data) ->
    Table = element(1, Data),
    Key = element(2, Data),
    cache:put(Table, Key, Data),
    data_writer:update(Data).

insert(Data) ->
    Table = element(1, Data),
    Key = element(2, Data),
    cache:put(Table, Key, Data),
    data_writer:insert(Data).

delete(Table, Id) ->
    cache:delete(Table, Id),
    data_writer:delete(Table, Id).

%% 加锁
synchronized(Table, Key, Fun) ->
    data_lock:lock(Table, Key),
    try 
        Fun() 
    after
        data_lock:unlock(Table, Key)
    end.
