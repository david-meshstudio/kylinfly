-module(kylinfly_server).
-compile(export_all).
start() ->
	inets:stop(),
	application:ensure_started(inets),
	{ok, Pid}=inets:start(httpd, [
		{modules, [mod_esi]},
		{port, 8368},
		{server_name, "kylinfly"},
		{document_root, "www"},
		{server_root, "www"},
		{erl_script_alias, {"/api", [contract_BM_TBGameBackendV7_api,contract_BM_TBGameBackendV6_api,contract_BM_TBGameBackendV5_api,contract_BM_TBGameBackendV4_api,contract_BM_TBGameBackendV3_api,contract_BM_TBGameBackendV2_api,kylinfly_controller,contract_BM_TBGameBackendV1_api,contract_BM_TBGameBackendV0_api]}}
	]),
	qiniulib:uploadObj("Pid", Pid).

reload() ->
	httpd:reload_config([
		{modules, [mod_esi]},
		{port, 8368},
		{server_name, "kylinfly"},
		{document_root, "www"},
		{server_root, "www"},
		{erl_script_alias, {"/api", [contract_BM_TBGameBackendV7_api,contract_BM_TBGameBackendV6_api,contract_BM_TBGameBackendV5_api,contract_BM_TBGameBackendV4_api,contract_BM_TBGameBackendV3_api,contract_BM_TBGameBackendV2_api,kylinfly_controller,contract_BM_TBGameBackendV1_api,contract_BM_TBGameBackendV0_api]}}
	], non_disturbing).