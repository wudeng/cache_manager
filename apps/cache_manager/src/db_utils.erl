-module(db_utils).

-export([gen_header_file/0]).

gen_header_file() ->
    ok = application:start(poolboy),
    ok = application:start(mysql),
    ok = application:start(mysql_poolboy),
    FileName = "table.hrl",
    {ok, IoDev} = file:open(FileName, [write]),
    Head = "-ifndef(TABLE_H_).\n-define(TABLE_H_, 1).\n\n",
    file:write(IoDev, Head),
    {ok, _Fields, Rows} = mysql_poolboy:query(pool, "show tables", []),
    L = lists:map(fun ([TableName]) -> table_record(TableName) end, Rows),
    file:write(IoDev, L),
    Tail = "\n-endif.",
    file:write(IoDev, Tail),
    file:close(IoDev).

table_record(TableName) ->
    {ok, _, Rows} = mysql_poolboy:query(pool, ["desc ", TableName]),
    FieldInfo = [
        begin
		   F = binary_to_list(Col),
		   case Default of
		     null ->
                 case binary_to_list(Type) of
                   "int" ++ _ -> F ++ " = 0";
                   "bigint" ++ _ -> F ++ " = 0";
                   "varchar" ++ _ -> F ++ " = \"\"";
                   "char" ++ _ -> F ++ " = \"\"";
                   "mediumtext" ++ _ -> F ++ " = \"\"";
                   "timestamp" ++ _ -> F ++ " = \"now()\"";
                   _ -> F
                 end;
		     _ ->
                 Default2 = binary_to_list(Default),
                 case binary_to_list(Type) of
                   "int" ++ _ -> F ++ " = " ++ Default2;
                   "bigint" ++ _ -> F ++ " = " ++ Default2;
                   "varchar" ++ _ -> F ++ " = \"" ++ Default2 ++ "\"";
                   "char" ++ _ -> F ++ " = \"" ++ Default2 ++ "\"";
                   "mediumtext" ++ _ -> F ++ " = \"" ++ Default2 ++ "\"";
                   "timestamp" ++ _ -> F ++ " = \"now()\"";
                   _ -> F
                 end
		   end
		 end
		 || 
         [Col, Type, _Null, _Key, Default, _] <- Rows 
    ],
    lists:append(["-record(", binary_to_list(TableName),", {", string:join(FieldInfo, ","), "}).\n"]).

