-module(desks_map_controller).
-export([
         index/1, get_walls/0, maybe_start_db_and_get_desk_reservations/1,
         date_str_to_db/1
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
    RawDesks = maybe_start_db_and_get_desk_reservations(Date),
    %lists:map(fun desk_response/1 end, RawDesks).
    % TODO Get Desks from DB
    [
        #{is_taken => false, id => 1, name => <<"">>, x => 20, y => 20}
      , #{is_taken => true, id => 2, name => <<"Aleksander">>, x => 40, y => 40}
    ].

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

date_str_to_db(StringDate) -> 
    Sections = string:split(BinDateAndName, "-", all),
    [YYYY | MM | DD ] = Sections,
    {YYYY, MM, DD}.


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


% desk_response({DeskId, X, Y, null, null, null}) ->
%     #{is_taken => false, id => DeskId, name => <<"">>, x => X, y => Y};
% desk_response({DeskId, X,Y, {_YYYY,_MM,_DD}, _UserId, UserName}) ->
%     #{is_taken => true, id => DeskId, name => UserName, x => X, y => Y}.

