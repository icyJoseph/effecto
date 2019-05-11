-module(variables).
-export([
		get/1
	]).

get(path) -> "magic/path/dude/";
get(_) -> fail.