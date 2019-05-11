-module(database).
-export([
		init/0,
		save/0,
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
		dets:close(Dets),
		lock(Ets)
	end, Etses),
	spawn(?MODULE, saveloop, []).

-spec saveloop() -> no_return().
saveloop() ->
	receive
	after 1000*60 ->
		save()
	end, saveloop().

save() ->
	lists:foreach(fun(Ets) ->
		unlock(Ets),
		{ok, Dets} = dets:open_file(
			variables:get(path) ++ atom_to_list(Ets),
			[]),
		dets:from_ets(Dets, Ets),
		dets:close(Dets),
		lock(Ets)
	end, variables:get(ets)).

lock(Ets) ->
	os:cmd("gpg --yes --batch --passphrase=SuPeRSecretePassword -c "
			++ variables:get(path) ++ atom_to_list(Ets)),
	os:cmd("rm "++ variables:get(path) ++ atom_to_list(Ets)).

unlock(Ets) ->
	os:cmd("gpg --yes --batch --passphrase=SuPeRSecretePassword "
			++ variables:get(path) ++ atom_to_list(Ets)).