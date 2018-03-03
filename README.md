cache_manager
=====

Erlang游戏服务器的数据管理。

- 数据库采用mysql，用的驱动是mysql-otp，使用poolboy管理数据连接池。
- 使用开源的cache作为缓存，每个缓存是一组ets表，新旧替换。
- 采用cache aside的方式，所以很容易把cache替换成其他的实现。比如redis等。
- 实现了从数据库接口直接导出record结构，并提供增删查改的接口。
- 游戏中有些数据不仅会被玩家自己修改，还会被其他进程修改，比如好友信息，空间信息等，所以需要支持并发。
- cache miss 的时候，通过加锁二次检查，防止击穿。
- 当数据库中不存在条目的时候，通过写入not_exist，防止穿透。
- 写入数据采用write behind策略，不阻塞当前进程，只保留一条持久化进程，所有写请求排队写入，防止开服导入玩家时发生雪崩。
- 数据发生变动的时候实时发消息到持久化进程。不依赖缓存中的数据。所以缓存evict不会导致数据丢失。
- 锁的粒度为行，降低冲突。

使用方法：
- 需要支持并发修改数据通过synchronized包起来。

```
data:synchronized(Table, Id, fun() ->
    Record = data:lookup(Table, Id),
    NewRecord = do_something_to(Record),
    data:update(NewRecord)
end).
```

TODO：
持久化进程只有一个。可能成为瓶颈。停服的时候只能等他写完才能停服。
所以是不是把持久化进程做成消息队列。不依赖游戏服会好一点。这样停服它照样可以写。


合并写操作。

错误处理


Build
-----
    $ make all
    $ make rel
    $ make run
