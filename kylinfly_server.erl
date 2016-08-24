-module(kylinfly_server).
-compile(export_all).
start() ->
	inets:stop(),
	ContractList = qiniulib:downloadObj("contractlist"),
	io:format("~p~n", [ContractList]),
	application:ensure_started(inets),
	{ok, Pid}=inets:start(httpd, [
		{modules, [mod_esi,mod_get]},
		{port, 8368},
		{server_name, "kylinfly"},
		{document_root, "www"},
		{server_root, "www"},
		{script_nocache, false},
		{erl_script_alias, {"/api", [healthcheck|get_moduleList(ContractList)]}}
	]),
	qiniulib:uploadObj("Pid", Pid).

reload() ->
	ContractList = qiniulib:downloadObj("contractlist"),
	io:format("~p~n", [ContractList]),
	Res = httpd:reload_config([
		{modules, [mod_esi,mod_get]},
		{port, 8368},
		{server_name, "kylinfly"},
		{document_root, "www"},
		{server_root, "www"},
		{script_nocache, false},
		{erl_script_alias, {"/api", [healthcheck|get_moduleList(ContractList)]}}
	], non_disturbing),
	io:format("~p~n", [Res]).

get_moduleList([]) ->
	[];
get_moduleList([Contract|L])  ->
	[list_to_atom(Contract)|get_moduleList(L)].