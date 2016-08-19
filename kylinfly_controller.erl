-module(kylinfly_controller).
-import(rfc4627,[encode/1,decode/1]).
-export([do/3]).
-define(CA, "0xae5d318a3e4dc67f465f11fa9eacce5df537702a").

do(SessionID, _Env, Input) ->
	Data = decode(Input),
	io:format("~p~n", [Data]),
	Header = ["Content-Type: text/plain; charset=utf-8\r\n\r\n"],
	{ok, {obj, [{_, Command}, {_, Params}]}, []} = Data,
	case binary_to_list(Command) of
		"getBalance" when Params =:= [] ->
			Content = encode(etherlib:eth_getBalance(?CA));
		"getBalance" ->
			[Account|_] = Params,
			Content = encode(etherlib:eth_getBalance(binary_to_list(Account)));
		"compileContract" ->
			[File|_] = Params,
			{ContractNames, _, AbiDef} = etherlib:eth_compileSolidityQiniuFile(binary_to_list(File)),
			Content = ContractNames++"|"++encode(AbiDef);
		"publishContract" ->
			[File, Gas, Value|_] = Params,
			{[ContractName|_], [BinCodes|_], AbiDef} = etherlib:eth_compileSolidityQiniuFile(binary_to_list(File)),
			{ok, {obj, [_, _, {_, Txid}]}, _} = decode(etherlib:eth_sendTransaction(?CA, binary_to_list(Gas), binary_to_list(Value), binary_to_list(BinCodes))),
			Content = ContractName++"|"++encode(AbiDef)++"|"++binary_to_list(Txid);
		"deployContractAPI" ->
			[File, Txid|_] = Params,
			{[ContractName|_], _, AbiDef} = etherlib:eth_compileSolidityQiniuFile(binary_to_list(File)),
			{ok, {obj, [_, _, {_, {obj, [_, _, {_, Account}, _, _, _, _, _, _, _, _]}}]}, _} = decode(etherlib:eth_getTransactionReceipt(binary_to_list(Txid))),
			apigenerator:gen_api_sourcefile(ContractName, binary_to_list(Account), AbiDef),
			apigenerator:update_contract_api(ContractName),
			timer:sleep(3000),
			Content = ContractName++"|"++encode(AbiDef)++"|"++binary_to_list(Account),
			mod_esi:deliver(SessionID, [Header, unicode:characters_to_binary(Content), ""]),
			apigenerator:update_server();
		Other ->
			Content = encode({"Unknown Query", Other})
	end,
	mod_esi:deliver(SessionID, [Header, unicode:characters_to_binary(Content), ""]).