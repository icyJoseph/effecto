-module(websocket_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	authorize:init(),
	Dispatch = cowboy_router:compile([
		{'_', [
			%% copypasted example from ws_handler example, to test it.
			{"/", cowboy_static, {priv_file, websocket, "index.html"}},
			{"/websocket", ws_h, []},
			{"/static/[...]", cowboy_static, {priv_dir, websocket, "static"}}
		]}
	]),
	{ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
		env => #{dispatch => Dispatch}
	}),
	websocket_sup:start_link().

stop(_State) ->
	ok.
