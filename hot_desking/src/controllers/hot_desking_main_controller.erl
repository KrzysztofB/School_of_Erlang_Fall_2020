-module(hot_desking_main_controller).
-export([
         index/1
        ]).

index(#{req := #{path := <<"/">>}}) ->
    {ok, [
        {message, "When would you like to visit the office?"},
        {main_view, "choose_date"},
        {qs, ""}
    ]}.
