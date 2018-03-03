-module(model).

%%-include("table.hrl").
-include_lib("eunit/include/eunit.hrl").

kv_list([K], [V]) ->  [{K, V}];
kv_list([K|Kl], [V|Vl]) -> [{K, V}] ++ kv_list(Kl, Vl).

max(Column, Table) ->
  L = model_sql:max(Table, Column, all),
  {data, Result} = model_exec:exec(L),
  [[Id]] = mysql:get_result_rows(Result),
  Id.

max_id(Table) ->
  case ?MODULE:max(id, Table) of
    undefined -> 0;
    Id ->  Id
  end.

count(Cond, Table) ->
  Sql = model_sql:count(Table, Cond),
  {data, Result} = model_exec:exec(Sql),
  [[Count]] = mysql:get_result_rows(Result),
  Count.

trans(Fun) ->
  Poll = model_config:poll(),
  mysql:transaction(Poll, Fun).


init() ->
    {ok, _Fields, Tables} = mysql_poolboy:query(pool, "show tables", []),
    Fun0 = lists:foldl(
        fun([TableName], Acc) ->
            {ok, _, Fields} = mysql_poolboy:query(pool, ["desc ", TableName]),
            FieldInfo = [binary_to_list(Col) || [Col | _] <- Fields],
            Acc ++ lists:append(["m(", binary_to_list(TableName),") -> [", string:join(FieldInfo, ","), "];\n"])
        end,
        "",
        Tables
    ),
    Fun1 = Fun0 ++ "m(_) -> throw(map_error).",
    M0 = spt_smerl:new(model_record),
    {ok, M1} = spt_smerl:add_func(M0, Fun1),
    M2 = spt_smerl:set_exports(M1, [{m, 1}]),
    ok = spt_smerl:compile(M2),
    lists:foreach(
        fun([TableName]) ->
            Atom = list_to_atom(binary_to_list(TableName)),
            cache:start_link(Atom, [{n, 10}, {ttl, 3600}]),
            data_lock:start_link(Atom)
        end,
        Tables
    ).

select(Table, Id) ->
    PrimaryKey = hd(model_record:m(Table)),
    Sql = model_sql:select(Table, all, [{PrimaryKey, Id}]),
    {ok, _Columns, Rows} = model_exec:exec(Sql),
    case Rows of
        [] -> not_exist;
        [Row] -> list_to_tuple([Table|db2mem(Row)])
    end.

insert(Data) ->
    [Table | Values] = tuple_to_list(Data),
    Fields = model_record:m(Table),
    true = (length(Fields) == length(Values)),
    Kv = kv_list(Fields, mem2db(Values)),
    SQL = model_sql:insert(Table, Kv),
    model_exec:exec(SQL).

update(Data) ->
    [Table, Id | Values] = tuple_to_list(Data),
    [Key | Fields] = model_record:m(Table),
    true = (length(Fields) == length(Values)),
    Kv = kv_list(Fields, mem2db(Values)),
    SQL = model_sql:update(Table, Kv, [{Key, Id}]),
    model_exec:exec(SQL).

delete(Table, Id) ->
    [Key | _] = model_record:m(Table),
    SQL = model_sql:delete(Table, [{Key, Id}]),
    model_exec:exec(SQL).

db2mem(Row) -> 
    [case F of 
         <<"{", _/binary>> -> string_to_term(F);
         <<"[", _/binary>> -> string_to_term(F);
         _ -> F
     end || F <- Row].

mem2db(Row) ->
    [if 
         is_list(F) -> term_to_string(F);
         is_tuple(F) -> term_to_string(F);
         true -> F
     end || F <- Row].


string_to_term(String) when is_binary(String) ->
    string_to_term(binary_to_list(String));
string_to_term(String) when is_list(String) ->
    case erl_scan:string(String ++ ".") of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} -> Term;
                _Err -> undefined
            end;
        _Error -> undefined
    end.

term_to_string(Term) ->
    list_to_binary(io_lib:format("~w", [Term])).
