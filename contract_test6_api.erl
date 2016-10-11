-module(contract_test6_api).
-compile(export_all).
-import(etherlib,[eth_getBalance/1,eth_methodCall/3,eth_propertyCall/2,eth_propertyMappingCall/3,string2hexstring/1,hexstring2string/1,hex2de/1,hexstring2de/1]).
-import(rfc4627,[encode/1,decode/1]).
-define(CA, "0xae5d318a3e4dc67f465f11fa9eacce5df537702a").
-define(ACCOUNT, "0xf81ee88a5d86b69fa53dde9915ece20ab473bc50").
getBalance() ->
	[_,_|L] = binary_to_list(eth_getBalance(?CA)),
	hex2de(L) / 1000000000000000000.
do(SessionID, _Env, Input) ->
	Data = httpd:parse_query(Input),
	io:format("~p~n", [Data]),
	Header = ["Content-Type: text/plain; charset=utf-8 \r\n Access-Control-Allow-Origin:* \r\n\r\n"],
	[{_, Command},{_, ParamsString}] = Data,
	{ok, Params} = httpd_util:split(ParamsString, ",", 10),
	case Command of
		"getBalance" ->
			Content = encode(getBalance());
		"Test1" ->
			Content = func_Test1(Params);
		"SSOpt" ->
			Content = evt_SSOpt();
		Other ->
			Content = {"Unknown Query", Other}
	end,
	io:format("~p~n", [Content]),
	mod_esi:deliver(SessionID, [Header, unicode:characters_to_binary(Content), ""]).
func_Test1(Params) ->
	[P_cmd,P_c1,P_c2|_] = Params,
	eth_methodCall(?ACCOUNT,"Test1",[{"bytes32",P_cmd,64,0},{"bytes32",P_c1,64,0},{"bytes32",P_c2,64,0}]).
evt_SSOpt() ->
	etherlib:eth_getEventLogs(?ACCOUNT,"0x7e48264165913bb785f1653845f92488b1a5192a9cd3d2447f7bedf269e2885a").
