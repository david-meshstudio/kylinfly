-module(contract_BM_TBGameBackendV2_api).
-compile(export_all).
-import(etherlib,[eth_getBalance/1,eth_methodCall/3,eth_propertyCall/2,eth_propertyMappingCall/3,string2hexstring/1,hexstring2string/1,hex2de/1,hexstring2de/1]).
-import(rfc4627,[encode/1,decode/1]).
-define(CA, "0x6B015e3c7D407977fa053e577F89A319667d3A21").
-define(ACCOUNT, "0x081ca238d12e46ceb6763c5cfaaf92cbb7f75f60e58b3591ceed5db3f7b5950b").
getBalance() ->
	[_,_|L] = binary_to_list(eth_getBalance(?CA)),
	hex2de(L) / 1000000000000000000.
do(SessionID, _Env, Input) ->
	Data = decode(Input),
	io:format("~p~n", [Data]),
	Header = ["Content-Type: text/plain; charset=utf-8/r/n/r/n"],
	{ok, {obj, [{_, Command}, {_, Params}]}, []} = Data,
	case binary_to_list(Command) of
		"getBalance" ->
			Content = encode(getBalance());
		"SetTreature" ->
			Content = func_SetTreature(Params);
		"UseChance" ->
			Content = func_UseChance(Params);
		"AddTool" ->
			Content = func_AddTool(Params);
		"Checkin" ->
			Content = func_Checkin(Params);
		"ResetChance" ->
			Content = func_ResetChance(Params);
		"MapInitSetTreature" ->
			Content = func_MapInitSetTreature(Params);
		"XRay" ->
			Content = func_XRay(Params);
		"DigConfirm" ->
			Content = func_DigConfirm(Params);
		"Dig" ->
			Content = func_Dig(Params);
		"AddChance" ->
			Content = func_AddChance(Params);
		"UseTool" ->
			Content = func_UseTool(Params);
		"MapInit" ->
			Content = func_MapInit(Params);
		"RegisterMember" ->
			Content = func_RegisterMember(Params);
		Other ->
			Content = {"Unknown Query", Other}
	end,
	mod_esi:deliver(SessionID, [Header, unicode:characters_to_binary(Content), ""]).
func_SetTreature(Params) ->
	[P__type,P__grid|_] = Params,
	eth_methodCall(?CA,"SetTreature",[{"uint256",binary_to_list(P__type),64,0},{"bytes32",binary_to_list(P__grid),64,0}]).
func_UseChance(Params) ->
	[P__id|_] = Params,
	eth_methodCall(?CA,"UseChance",[{"uint256",binary_to_list(P__id),64,0}]).
func_AddTool(Params) ->
	[P__id,P__tool|_] = Params,
	eth_methodCall(?CA,"AddTool",[{"uint256",binary_to_list(P__id),64,0},{"uint256",binary_to_list(P__tool),64,0}]).
func_Checkin(Params) ->
	[P__id,P_timestamp|_] = Params,
	eth_methodCall(?CA,"Checkin",[{"uint256",binary_to_list(P__id),64,0},{"uint256",binary_to_list(P_timestamp),64,0}]).
func_ResetChance(Params) ->
	[P__id,P__chance|_] = Params,
	eth_methodCall(?CA,"ResetChance",[{"uint256",binary_to_list(P__id),64,0},{"uint256",binary_to_list(P__chance),64,0}]).
func_MapInitSetTreature(Params) ->
	[P__type,P__grid|_] = Params,
	eth_methodCall(?CA,"MapInitSetTreature",[{"uint256",binary_to_list(P__type),64,0},{"bytes32",binary_to_list(P__grid),64,0}]).
func_XRay(Params) ->
	[P__grid|_] = Params,
	eth_methodCall(?CA,"XRay",[{"bytes32",binary_to_list(P__grid),64,0}]).
func_DigConfirm(Params) ->
	[P__id,P__grid|_] = Params,
	eth_methodCall(?CA,"DigConfirm",[{"uint256",binary_to_list(P__id),64,0},{"bytes32",binary_to_list(P__grid),64,0}]).
func_Dig(Params) ->
	[P__id,P__grid|_] = Params,
	eth_methodCall(?CA,"Dig",[{"uint256",binary_to_list(P__id),64,0},{"bytes32",binary_to_list(P__grid),64,0}]).
func_AddChance(Params) ->
	[P__id|_] = Params,
	eth_methodCall(?CA,"AddChance",[{"uint256",binary_to_list(P__id),64,0}]).
func_UseTool(Params) ->
	[P__id,P__tool|_] = Params,
	eth_methodCall(?CA,"UseTool",[{"uint256",binary_to_list(P__id),64,0},{"uint256",binary_to_list(P__tool),64,0}]).
func_MapInit(Params) ->
	[P__grid|_] = Params,
	eth_methodCall(?CA,"MapInit",[{"bytes32",binary_to_list(P__grid),64,0}]).
func_RegisterMember(Params) ->
	[P__id,P__chance,P__tcount|_] = Params,
	eth_methodCall(?CA,"RegisterMember",[{"uint256",binary_to_list(P__id),64,0},{"uint256",binary_to_list(P__chance),64,0},{"uint256",binary_to_list(P__tcount),64,0}]).
