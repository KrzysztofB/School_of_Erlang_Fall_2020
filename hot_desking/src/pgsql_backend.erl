-module(pgsql_backend).

% Inspiration https://github.com/spawnfest/eneo4j/blob/master/src/eneo4j_worker.erl

-behaviour(gen_server).

%% API
-export([start_link/1]).

-export([handle_call/3,
         handle_cast/2,
         handle_info/2,
         init/1,
         terminate/2]).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

init(_Args) ->
    {ok, Conn} = epgsql:connect(#{host => "localhost",
                                  port => 5432, username => "postgres",
                                  password => "postgres",
                                  database => "hot_desking", timeout => 4000}),
    {ok, #{conn => Conn}}.

handle_call(get_all_users, _From, State = #{conn := Conn}) ->
    {ok, _Cols, RawUsernames} = epgsql:squery(Conn, "select username from users"),
    Usernames = lists:map(fun ({Username}) -> Username end, RawUsernames),
    {reply, Usernames, State};
handle_call(get_walls, _From, State = #{conn := Conn}) ->
    {ok, _Cols, RawWalls} = epgsql:equery(Conn,
                                          "select begin_x, begin_y, end_x, end_y "
                                          "from walls"),
    Walls = lists:map(fun ({X1, Y1, X2, Y2}) ->
                    #{begin_x => X1, begin_y => Y1, end_x => X2, end_y => Y2} end,
                    RawWalls),
    {reply, Walls, State};
handle_call({get_desks, DateString}, _From, State = #{conn := Conn}) ->
    DateTuple = date_str_to_db(DateString),
    {ok, _Cols, RawDesks} = epgsql:equery(Conn,                
        "select d.id as desk_id, d.x, d.y, r.date, u.id as user_id, u.username from desks d "
        "left join reservations r on d.id=r.desk_id and r.date=$1::date "
        "left join users u on r.user_id=u.id",
            [DateTuple]),
    Desks = lists:map(fun desk_to_map/1, RawDesks),
    {reply, Desks, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(Cast, State) ->
    logger:warning("Unexpected cast ~p", [Cast]),
    {noreply, State}.

handle_info(Info, State) ->
    logger:warning("Unexpected message ~p", [Info]),
    {noreply, State}.

terminate(Reason, _State) ->
    logger:error("Unexpected termination ~p", [Reason]),
    ok.

date_str_to_db(Date) when is_binary(Date) ->
    DateAsStringList = string:split(Date, "-", all),
    [YYYY, MM, DD] = lists:map(fun binary_to_integer/1, DateAsStringList),
    {YYYY, MM, DD}.

desk_to_map({DeskId, X, Y, null, null, null}) ->
    #{is_taken => false, id => DeskId, name => <<"">>,
      x => X, y => Y, user_id => null};
desk_to_map({DeskId, X, Y, _Date, UserId, UserName}) ->
    #{is_taken => true, id => DeskId, name => UserName,
      x => X, y => Y, user_id => UserId}.
