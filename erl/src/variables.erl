-module(variables).
-export([
		get/1
	]).

-spec get(atom()) -> any().
get(path) -> "../../priv/";
get(ets) -> [user, group, messages_group];
get(_) -> fail.