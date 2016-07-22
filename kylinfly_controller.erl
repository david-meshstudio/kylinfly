-module(kylinfly_controller).
-import(rfc4627,[encode/1,decode/1]).
-export([do/3]).
-define(CA, "0x6B015e3c7D407977fa053e577F89A319667d3A21").

do(SessionID, _Env, Input) ->
	Data = decode(Input),
	io:format("~p~n", [Data]),
	Header = ["Content-Type: text/plain; charset=utf-8\r\n\r\n"],
	{ok, {obj, [{_, Command}, {_, Params}]}, []} = Data,
	Content = "",
	case binary_to_list(Command) of
		"getBalance" when Params =:= <<>> ->
			Content = encode(etherlib:getBalance());
		"getBalance" ->
			Content = encode(etherlib:getBalance(binary_to_list(Params)));
		"publishContract" ->
			[File, Gas, Value|_] = Params,
			{ContractName, BinCodes, AbiDef} = etherlib:eth_compileSolidityQiniuFile(File),
			{ok, {obj, [_, _, {_, {[_, _, {_, ACCOUNT}]}}]}, _} = decode(etherlib:eth_sendTransaction(?CA, Gas, Value, BinCodes)),
			apigenerator:gen_api_sourcefile(ContractName, ACCOUNT, AbiDef),
			apigenerator:update_contract_api(ContractName),
			apigenerator:update_server();
		Other ->
			Content = {"Unknown Query", Other}
	end,
	mod_esi:deliver(SessionID, [Header, unicode:characters_to_binary(Content), ""]).