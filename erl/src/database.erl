-module(database).
-export([
		init/0,
		saveloop/0
	]).

-spec init() -> no_return().
init() -> 
    Etses = variables:get(ets),
	lists:foreach(fun(Ets) -> 
		ets:new(Ets, [ordered_set, named_table, public, 
			{write_concurrency, true}, 
			{read_concurrency, true}]),
		{ok, Dets} = dets:open_file(variables:get(path) ++ atom_to_list(Ets), []),
		dets:to_ets(Dets, Ets),
		dets:close(Dets)
	end, Etses),
	spawn(?MODULE, saveloop, []).

-spec saveloop() -> no_return().
saveloop() ->
	receive
	after 1000*60 ->
		lists:foreach(fun(Ets) ->
			{ok, Dets} = dets:open_file(
				variables:get(path) ++ atom_to_list(Ets),
				[]),
			dets:from_ets(Dets, Ets),
			dets:close(Dets)
		end, variables:get(ets))
	end, saveloop().