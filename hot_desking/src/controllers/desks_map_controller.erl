-module(desks_map_controller).
-export([
         index/1
        ]).

-define(CANVAS_WIDTH, 1000).
-define(CANVAS_HEIGHT, 1000).

index(#{req := #{
    path := <<"/desks_map">>,
    qs := BinQueryString}}) ->
    {ok, [
        {query_string, binary_to_list(BinQueryString)},
        {canvas_width, ?CANVAS_WIDTH},
        {canvas_height, ?CANVAS_HEIGHT}
    ]};
index(#{req := #{
    path := <<"/desks">>,
    qs := BinDateAndName}}) ->
    {json, get_desks(BinDateAndName)};
index(#{req := #{path := <<"/walls">>}}) ->
    {json, get_walls()}.

get_desks(BinDateAndName) ->
    % TODO pep string and use a date to check if desks are taken or empty
    BinParamsWithoutAmp = string_prep(BinDateAndName),
    Date = get_date(BinParamsWithoutAmp),
    maybe_start_db_and_get_desk_reservations(Date).
    
% TODO uncomment string_prep if you have issues with
string_prep(BinDateAndName) ->
    ListOfBinParamsWithoutAmp = string:replace(BinDateAndName, "amp;", "", all),
    lists:foldl(
        fun (Elem, Acc) when is_list(Elem) -> BinElem = list_to_binary(Elem), <<Acc/binary, BinElem/binary>>;
            (Elem, Acc) -> <<Acc/binary, Elem/binary>>
        end, <<"">>, ListOfBinParamsWithoutAmp).

get_date(BinDateAndName) ->
    Params = string:split(BinDateAndName, "&", all),
    ProcessedParams = lists:map(fun get_param/1, Params),
    proplists:get_value(visit_date, ProcessedParams).



get_param(<<"name=", Value/binary>>) -> {name, Value};
get_param(<<"visit_date=", Value/binary>>) -> {visit_date, Value};
get_param(<<"desk_id=", Value/binary>>) -> {desk_id, list_to_integer(binary_to_list(Value))}.

get_walls() -> maybe_start_db_and_get_all_the_walls().

maybe_start_db_and_get_all_the_walls() ->
    DbPid = case pgsql_backend:start_link(db) of
        {ok, Pid} -> Pid;
        {error, {already_started, Pid}} -> Pid
    end,
    gen_server:call(DbPid, get_walls).

maybe_start_db_and_get_desk_reservations(Date) ->
    DbPid = case pgsql_backend:start_link(db) of
        {ok, Pid} -> Pid;
        {error, {already_started, Pid}} -> Pid
    end,
    gen_server:call(DbPid, {get_desks, Date}).

