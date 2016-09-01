-module(contract_kf_test1_api).
-compile(export_all).
-import(etherlib,[eth_getBalance/1,eth_methodCall/3,eth_propertyCall/2,eth_propertyMappingCall/3,string2hexstring/1,hexstring2string/1,hex2de/1,hexstring2de/1]).
-import(rfc4627,[encode/1,decode/1]).
-define(CA, "0xae5d318a3e4dc67f465f11fa9eacce5df537702a").
-define(ACCOUNT, "0xa5310112a1970b886c93203713870b69faeaa423").
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
		"multiply" ->
			Content = func_multiply(Params);
		"subtract" ->
			Content = func_subtract(Params);
		"add" ->
			Content = func_add(Params);
		"getTotal" ->
			Content = func_getTotal(Params);
		"divid" ->
			Content = func_divid(Params);
		Other ->
			Content = {"Unknown Query", Other}
	end,
	io:format("~p~n", [Content]),
	mod_esi:deliver(SessionID, [Header, unicode:characters_to_binary(Content), ""]).
func_multiply(Params) ->
	[P_x,P_y|_] = Params,
	eth_methodCall(?ACCOUNT,"multiply",[{"uint256",P_x,64,0},{"uint256",P_y,64,0}]).
func_subtract(Params) ->
	[P_x,P_y|_] = Params,
	eth_methodCall(?ACCOUNT,"subtract",[{"uint256",P_x,64,0},{"uint256",P_y,64,0}]).
func_add(Params) ->
	[P_x,P_y|_] = Params,
	eth_methodCall(?ACCOUNT,"add",[{"uint256",P_x,64,0},{"uint256",P_y,64,0}]).
func_getTotal(_) ->
	eth_propertyCall(?ACCOUNT,"getTotal").
func_divid(Params) ->
	[P_x,P_y|_] = Params,
	eth_methodCall(?ACCOUNT,"divid",[{"uint256",P_x,64,0},{"uint256",P_y,64,0}]).
