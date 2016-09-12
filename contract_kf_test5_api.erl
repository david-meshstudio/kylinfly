-module(contract_kf_test5_api).
-compile(export_all).
-import(etherlib,[eth_getBalance/1,eth_methodCall/3,eth_propertyCall/2,eth_propertyMappingCall/3,string2hexstring/1,hexstring2string/1,hex2de/1,hexstring2de/1]).
-import(rfc4627,[encode/1,decode/1]).
-define(CA, "0xae5d318a3e4dc67f465f11fa9eacce5df537702a").
-define(ACCOUNT, "0x0e0e64bb75bb8a3b6c66a4c309575a95823cbd8a").
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
		"getMessage" ->
			Content = func_getMessage(Params);
		"call" ->
			Content = func_call(Params);
		"subtract" ->
			Content = func_subtract(Params);
		"add" ->
			Content = func_add(Params);
		"getTotal" ->
			Content = func_getTotal(Params);
		"func" ->
			Content = func_func(Params);
		"callName" ->
			Content = func_callName(Params);
		"deposit" ->
			Content = func_deposit(Params);
		"getListTotal" ->
			Content = func_getListTotal(Params);
		"divid" ->
			Content = func_divid(Params);
		"getListInfo" ->
			Content = func_getListInfo(Params);
		"Deposit" ->
			Content = evt_Deposit();
		Other ->
			Content = {"Unknown Query", Other}
	end,
	io:format("~p~n", [Content]),
	mod_esi:deliver(SessionID, [Header, unicode:characters_to_binary(Content), ""]).
func_multiply(Params) ->
	[P_x,P_y|_] = Params,
	encode(etherlib:hex2de(eth_propertyMappingCall(?ACCOUNT,"multiply",[{"uint256",P_x,64,0},{"uint256",P_y,64,0}]))).
func_getMessage(Params) ->
	[P__id,P__values,P__message|_] = Params,
	eth_methodCall(?ACCOUNT,"getMessage",[{"uint256",P__id,64,0},{"uint256[]",etherlib:genArrayValueInput(P__values,"uint256"),string:len(etherlib:genArrayValueInput(P__values,"uint256")),0},{"string",P__message,string:len(P__message),32}]).
func_call(_) ->
	etherlib:getStringValue(eth_propertyCall(?ACCOUNT,"call")).
func_subtract(Params) ->
	[P_x,P_y|_] = Params,
	encode(etherlib:hex2de(eth_propertyMappingCall(?ACCOUNT,"subtract",[{"uint256",P_x,64,0},{"uint256",P_y,64,0}]))).
func_remarks(Params) ->
	[P_|_] = Params,
	etherlib:getMultiOutputValue(eth_propertyMappingCall(?ACCOUNT,"remarks",[{"uint256",P_,64,0}]),[{obj,[{"name",<<"id">>},{"type",<<"uint256">>}]},{obj,[{"name",<<"message">>},{"type",<<"string">>}]}]).
func_add(Params) ->
	[P_x,P_y|_] = Params,
	encode(etherlib:hex2de(eth_propertyMappingCall(?ACCOUNT,"add",[{"uint256",P_x,64,0},{"uint256",P_y,64,0}]))).
func_getTotal(_) ->
	encode(etherlib:hex2de(eth_propertyCall(?ACCOUNT,"getTotal"))).
func_func(Params) ->
	[P_x|_] = Params,
	eth_methodCall(?ACCOUNT,"func",[{"uint256",P_x,64,0}]).
func_callName(Params) ->
	[P_name|_] = Params,
	etherlib:getStringValue(eth_propertyMappingCall(?ACCOUNT,"callName",[{"string",P_name,string:len(P_name),32}])).
func_deposit(Params) ->
	[P__id|_] = Params,
	eth_methodCall(?ACCOUNT,"deposit",[{"bytes32",P__id,64,0}]).
func_getListTotal(Params) ->
	[P__values|_] = Params,
	encode(etherlib:hex2de(eth_propertyMappingCall(?ACCOUNT,"getListTotal",[{"uint256[]",etherlib:genArrayValueInput(P__values,"uint256"),string:len(etherlib:genArrayValueInput(P__values,"uint256")),0}]))).
func_divid(Params) ->
	[P_x,P_y|_] = Params,
	encode(etherlib:hex2de(eth_propertyMappingCall(?ACCOUNT,"divid",[{"uint256",P_x,64,0},{"uint256",P_y,64,0}]))).
func_getListInfo(Params) ->
	[P__values|_] = Params,
	encode(etherlib:getArrayValue(eth_propertyMappingCall(?ACCOUNT,"getListInfo",[{"uint256[]",etherlib:genArrayValueInput(P__values,"uint256"),string:len(etherlib:genArrayValueInput(P__values,"uint256")),0}]),"uint")).
evt_Deposit() ->
	etherlib:eth_getEventLogs(?ACCOUNT,"0x19dacbf83c5de6658e14cbf7bcae5c15eca2eedecf1c66fbca928e4d351bea0f").
