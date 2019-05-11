-module(variables).
-export([
		get/1
	]).

-spec get(atom()) -> any().
get(path) -> "../../priv/";
get(ets) -> [user, group];
get(_) -> fail.