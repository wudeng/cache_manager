-module(model_exec).

-export([exec/1]).

%% 统一的超时处理
exec(SQL) ->
  io:format("SQL ~p~n", [SQL]),
  %%Pool = model_config:pool(),
  Pool = pool,
  case catch mysql_poolboy:query(Pool, SQL) of
    {'EXIT', _} -> error_logger:warning_msg("SQL exit ~p", [SQL]), exit;
    {timeout, _}-> error_logger:warning_msg("SQL timeout ~p", [SQL]), timeout;
    {error, R} -> error_logger:warning_msg("SQL error ~p r ~p", [SQL, R]), error;
    _Other -> _Other
  end.
