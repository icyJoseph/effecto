-module(database).
-export([
		init/0,
		insert/2
	]).

init() -> 
    Etses = [online, group],
	lists:foreach(fun(Ets) -> 
		ets:new(Ets, [ordered_set, named_table, public, 
			{write_concurrency, true}, 
			{read_concurrency, true}]),
		{ok, Dets} = dets:open_file("../../priv/" ++ atom_to_list(Ets), []),
		dets:to_ets(Dets, Ets),
		dets:close(Dets)
	end, Etses).

insert(Ets, Data) ->
    ets:insert(Ets, Data),
    {ok, Dets} = dets:open_file("../../priv/" ++ atom_to_list(Ets), []),
    dets:from_ets(Dets, Ets),
    dets:close(Dets).    