-module(etherlib).
-export([call/2,eth_getBalance/1,eth_getCompilers/0,eth_compileSolidity/1,eth_compileSolidityFile/1,eth_compileSolidityQiniuFile/1,eth_sendTransaction/4,eth_getTransactionReceipt/1,web3_sha3/1,padleft/2,get_methodCallData/2,get_methodSignHash/1,eth_methodCall/3,get_methodSign/2,eth_propertyCall/2,eth_propertyMappingCall/3,string2hexstring/1,hexstring2string/1,hex2de/1,de2Hex/1,eth_blockNumber/0,get_tranBlockGap/1,hexstring2de/1,eth_getStorageAt/2,eth_getStorageAt/3,eth_gasPrice/0,eth_propertyMappingCallWithTransaction/3]).
-import(rfc4627,[encode/1,decode/1]).
-compile(export_all).
-define(CA, "0xae5d318a3e4dc67f465f11fa9eacce5df537702a").

call(Method, Params) ->
	inets:start(),
	case httpc:request(post,{"http://localhost:8545",[],"application/x-www-form-urlencoded","{\"jsonrpc\":\"2.0\",\"method\":\""++Method++"\",\"params\":"++Params++",\"id\":1}"},[],[]) of
		{ok, {_, _, Body}} -> Body;
		{error, Reason} -> io:format("error cause ~p~n", [Reason])
	end.

eth_getBalance(Account) ->
	{ok, {obj, [_, _, {_, Result}]}, _} = decode(call("eth_getBalance","[\""++Account++"\",\"latest\"]")),
	Result.

eth_getStorageAt(Address, Offset) ->
	eth_getStorageAt(Address, Offset, "latest").

eth_getStorageAt(Address, Offset, Tag) ->
	{ok, {obj, [_, _, {_, Result}]}, _} = decode(call("eth_getStorageAt","[\""++Address++"\",\"0x"++Offset++"\",\""++Tag++"\"]")),
	Result.

eth_gasPrice() ->
	{ok, {obj, [_, _, {_, Result}]}, _} = decode(call("eth_gasPrice","[]")),
	hexstring2de(Result) / 100000.

eth_getCompilers() ->
	{ok, {obj, [_, _, {_, Result}]}, _} = decode(call("eth_getCompilers","[]")),
	Result.

eth_compileSolidityCodelist(Codelist) ->
	Code = string:join(Codelist, " "),
	Codelist2 = string:tokens(Code, "\""),
	Code2 = string:join(Codelist2, "\\\""),
	eth_compileSolidity(Code2).

eth_compileSolidityQiniuFile(File) ->
	Source = qiniulib:download(File),
	% Codelist = string:tokens(Source, "\r\n\t"),
	% eth_compileSolidityCodelist(Codelist).
	kylinfly_compiler:kfc_compileSolidityCodeWithKylinflyMark(Source).

eth_compileSolidityFile(File) ->
	{ok, Source} = file:read_file(File),
	% Codelist = string:tokens(binary_to_list(Source), "\r\n\t"),
	% eth_compileSolidityCodelist(Codelist).
	kylinfly_compiler:kfc_compileSolidityCodeWithKylinflyMark(Source).

eth_compileSolidity(Code) ->
	{ok, {obj, [_, _, {_, {obj, ContractCodes}}]}, _} = decode(call("eth_compileSolidity","[\""++Code++"\"]")),
	{ContractNames, BinCodes, [{_, [_, _, _, _, _, {_, AbiDef}, _, _]}]} = get_contractCodeInfo(ContractCodes),
	{ContractNames, BinCodes, AbiDef}.

get_contractCodeInfo([H|CL]) ->
	{ContractName, {obj, [{_, BinCode}, {_, Info}]}} = H,
	{C, B, I} = get_contractCodeInfo(CL),
	{[ContractName|C], [BinCode|B], [Info|I]};
get_contractCodeInfo([]) ->
	{[], [], []}.

eth_sendTransaction(From, Gas, Value, Data) ->
	Params = "[{\"from\":\""++From++"\",\"gas\":\""++Gas++"\",\"value\":\""++Value++"\",\"data\":\""++Data++"\"}]",
	call("eth_sendTransaction", Params).

eth_getTransactionReceipt(Txid) ->
	call("eth_getTransactionReceipt","[\""++Txid++"\"]").

eth_blockNumber() ->
	{ok, {obj, [_, _, {_, Result}]}, _} = decode(call("eth_blockNumber","[\"\"]")),
	Result.

get_tranBlockGap(Txid) ->
	{ok, {obj, [_, _, {_, Result}]}, _} = decode(eth_getTransactionReceipt(Txid)),
	{obj, [_, _, {_, TBN}, _, _, _, _, _]} = Result,
	hexstring2de(eth_blockNumber()) - hexstring2de(TBN).

eth_propertyCall(To, Property) ->
	Data = get_methodSignHash(Property++"()"),
	{ok, {obj, [_, _, {_, Result}]}, _} = decode(call("eth_call","[{\"to\":\""++To++"\",\"data\":\""++Data++"\"},\"latest\"]")),
	[_,_|RL] = binary_to_list(Result),
	RL.

eth_propertyMappingCall(To, Property, []) ->
	eth_propertyCall(To, Property);
eth_propertyMappingCall(To, Property, Params) ->
	Data = get_methodSignHash(Property++"("++get_ParamsTypeString(Params)++")") ++ get_ParamsValueString(Params),
	{ok, {obj, [_, _, {_, Result}]}, _} = decode(call("eth_call","[{\"to\":\""++To++"\",\"data\":\""++Data++"\"},\"latest\"]")),
	[_,_|RL] = binary_to_list(Result),
	RL.

eth_propertyMappingCallWithTransaction(To, Property, Params) ->
	Data = get_methodSignHash(Property++"("++get_ParamsTypeString(Params)++")") ++ get_ParamsValueString(Params),
	{ok, {obj, [_, _, {_, Result}]}, _} = decode(call("eth_call","[{\"from\":\""++?CA++"\",\"to\":\""++To++"\",\"gas\":\"400000\",\"data\":\""++Data++"\"},\"latest\"]")),
	[_,_|RL] = binary_to_list(Result),
	RL.

eth_methodCall(To, Method, Params) ->
	Data = get_methodSignHash(Method++"("++get_ParamsTypeString(Params)++")") ++ get_ParamsValueString(Params),
	% {ok, {obj, [_, _, {_, Result}]}, _} = decode(call("eth_sendTransaction","[{\"from\":\"0x01E4Cb51Ec4768B9430b06A6EC2284C7977cCa48\",\"to\":\""++To++"\",\"data\":\""++Data++"\"}]")),
	{ok, {obj, [_, _, {_, Result}]}, _} = decode(call("eth_sendTransaction","[{\"from\":\""++?CA++"\",\"to\":\""++To++"\",\"data\":\""++Data++"\"}]")),
	[_,_|RL] = binary_to_list(Result),
	RL.

get_methodCallData(Method, Params) ->
	get_methodSignHash(get_methodSign(Method,Params)) ++ get_ParamsValueString(Params).

get_methodSign(Method, Params) ->
	Method++"("++get_ParamsTypeString(Params)++")".

get_methodSignHash(Sign) ->
	{ok, {obj, [_, _, {_, Result}]}, _} = decode(web3_sha3(Sign)),
	[H1,H2,H3,H4,H5,H6,H7,H8,H9,H10|_] = binary_to_list(Result),
	[H1,H2,H3,H4,H5,H6,H7,H8,H9,H10].

get_ParamsTypeString([P|[]]) ->
	case P of
		{Type, _, _, _} ->
			Type;
		{} ->
			""
	end;
get_ParamsTypeString([P|PL]) ->
	case P of
		{Type, _, _, _} ->
			Type++","++get_ParamsTypeString(PL);
		{} ->
			""
	end.

get_ParamsValueString([P|PL]) ->
	case P of
		{Type, Value, Length, Offset} ->
			if
				Type =:= "uint256" ->
					{Value1,_} = string:to_integer(Value),
					padleft(de2Hex(Value1), Length) ++ get_ParamsValueString(PL);
				Type =:= "uint" ->
					{Value1,_} = string:to_integer(Value),
					padleft(de2Hex(Value1), Length) ++ get_ParamsValueString(PL);
				Type =:= "bytes32" ->
					padright(Value, Length) ++ get_ParamsValueString(PL);
				Type =:= "string" ->
					padleft(de2Hex(Offset), 64) ++ padleft(de2Hex(length(Value)), 64) ++ padright(string2hexstring(Value), Length) ++ get_ParamsValueString(PL);
				true ->
					padleft(Value, Length) ++ get_ParamsValueString(PL)
			end;
		{} ->
			""
	end;
get_ParamsValueString([]) ->
	"".

web3_sha3(Content) ->
	call("web3_sha3","[\"0x"++string2hexstring(Content)++"\"]").

string2hexstring([H|T]) ->
	integer_to_list(H,16)++string2hexstring(T);
string2hexstring([]) ->
	"".

hexstring2string([H1,H2|T]) ->
	[list_to_integer([H1,H2],16)|hexstring2string(T)];
hexstring2string([]) ->
	[].

padleft(S, L) ->
	if
		length(S) < L ->
			padleft("0"++S, L);
		true ->
			S
	end.

padright(S, L) ->
	if
		length(S) < L ->
			padright(S++"0", L);
		true ->
			S
	end.

tempData([0])-> [];  
tempData([Num]) ->
    Temp = Num band 15,  
    if  
        Temp >= 0,Temp < 10 -> Result = Temp + 48;  
        Temp >= 10,Temp < 16 ->  Result = Temp + 55          
    end,  
    [Result | tempData([Num bsr 4])].  
 
de2Hex(Num)->  
    lists:reverse(tempData([Num])).

hex2de(Hex) ->
	list_to_integer(Hex,16).

hexstring2de(Hex) ->
	[_,_|L] = binary_to_list(Hex),
	hex2de(L).