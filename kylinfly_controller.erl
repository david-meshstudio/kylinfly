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
			{ContractName, BinCodes, AbiDef} = etherlib:eth_compileSolidityQiniuFile(binary_to_list(File)),
			{ok, {obj, [_, _, {_, {[_, _, {_, ACCOUNT}]}}]}, _} = decode(etherlib:eth_sendTransaction(?CA, Gas, Value, BinCodes)),
			apigenerator:gen_api_sourcefile(ContractName, ACCOUNT, AbiDef),
			apigenerator:update_contract_api(ContractName),
			apigenerator:update_server(),
			Content = "ok";
		Other ->
			Content = encode({"Unknown Query", Other})
	end,
	mod_esi:deliver(SessionID, [Header, unicode:characters_to_binary(Content), ""]).