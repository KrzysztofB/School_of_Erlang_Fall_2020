-module(pgsql_backend).

% Inspiration https://github.com/spawnfest/eneo4j/blob/master/src/eneo4j_worker.erl

-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

init(_Args) ->
    {ok, Conn} = epgsql:connect(#{
        host => "localhost",
        port => 5432,
        username => "postgres",
        password => "postgres",
        database => "hot_desking",
        timeout => 4000
    }),
    {ok, #{conn => Conn}}.

handle_call(get_all_users, _From, State = #{conn := Conn}) ->
    {ok, _Cols, RawUsernames} = epgsql:squery(Conn, "select username from users"),
    Usernames = lists:map(fun({Username}) -> Username end, RawUsernames),
    {reply, Usernames, State};

handle_call(get_walls, _From, State = #{conn := Conn}) ->
    {ok, _Cols, RawWalls} = epgsql:equery(Conn, "select begin_x, begin_y, end_x, end_y from walls"),
    Walls = lists:map(fun({X1,Y1,X2,Y2}) -> #{begin_x=>X1, begin_y=>Y1, end_x=>X2, end_y=>Y2} end, RawWalls),
    {reply, Walls, State};

handle_call({get_desks, Date}, _From, State = #{conn := Conn}) ->
    {ok, _Cols, RawDesks} = epgsql:equery(Conn, 
        "select d.*, r.date, u.id, u.username from desks d left join reservations r on d.id=r.desk_id left join users u on r.user_id=u.id where r.date is null or r.date= $1::date", [{2020,11,18}]),
    {reply, RawDesks, State};

%select d.*, r.date from desks d left join reservations r on d.id=r.desk_id

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

%string2date(StringDate)-> 
