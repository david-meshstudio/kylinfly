-module(apigenerator).
-compile(export_all).
-import(rfc4627,[encode/1,decode/1]).
-define(CA, "0xae5d318a3e4dc67f465f11fa9eacce5df537702a").

update_server() ->
	test_handler:test("reload").

update_contract_api(ContractName) ->
	Model = "contract_" ++ ContractName ++ "_api",
	% delete_module(Model),
	compile:file(Model),
	ContractList = qiniulib:downloadObj("contractlist"),
	case sets:is_element(Model, sets:from_list(ContractList)) of
		true ->
			false;
		false ->
			qiniulib:uploadObj("contractlist", [Model|ContractList])
	end.

gen_api_sourcefile(ContractName, Account, AbiDefs) ->
	gen_api_sourcefile(ContractName, ?CA, Account, AbiDefs).

gen_api_sourcefile(ContractName, CA, Account, AbiDefs) ->
	File = "contract_" ++ ContractName ++ "_api.erl",
	{ok, S} = file:open(File, write),
	file:write_file(File, "-module(contract_" ++ ContractName ++ "_api).\r\n-compile(export_all).\r\n-import(etherlib,[eth_getBalance/1,eth_methodCall/3,eth_propertyCall/2,eth_propertyMappingCall/3,string2hexstring/1,hexstring2string/1,hex2de/1,hexstring2de/1]).\r\n-import(rfc4627,[encode/1,decode/1]).\r\n-define(CA, \"" ++ CA ++ "\").\r\n-define(ACCOUNT, \"" ++ Account ++ "\").\r\ngetBalance() ->\r\n\t[_,_|L] = binary_to_list(eth_getBalance(?CA)),\r\n\thex2de(L) / 1000000000000000000.\r\ndo(SessionID, _Env, Input) ->\r\n\tData = httpd:parse_query(Input),\r\n\tio:format(\"~p~n\", [Data]),\r\n\tHeader = [\"Content-Type: text/plain; charset=utf-8 \\r\\n Access-Control-Allow-Origin:* \\r\\n\\r\\n\"],\r\n\t[{_, Command},{_, ParamsString}] = Data,\r\n\t{ok, Params} = httpd_util:split(ParamsString, \",\", 10),\r\n\tcase Command of\r\n\t\t\"getBalance\" ->\r\n\t\t\tContent = encode(getBalance());\r\n\t\t" ++ get_control_code(AbiDefs) ++ "Other ->\r\n\t\t\tContent = {\"Unknown Query\", Other}\r\n\tend,\r\n\tio:format(\"~p~n\", [Content]),\r\n\tmod_esi:deliver(SessionID, [Header, unicode:characters_to_binary(Content), \"\"]).\r\n", [write]),
	Source = get_api_code(AbiDefs, ContractName),
	file:write_file(File, Source, [append]),
	file:close(S).

get_control_code([]) ->
	"";
get_control_code([AbiDef|L]) ->
	case AbiDef of
		{obj, [{"constant", Constant},{"inputs",_},{"name",Name},{"outputs",_},{"type",Type}]} ->
			if
				Constant ->
					get_control_code(L);
				true ->
					case Type of
						<<"function">> ->
							"\"" ++ binary_to_list(Name) ++ "\" ->\r\n\t\t\tContent = func_" ++ binary_to_list(Name) ++ "(Params);\r\n\t\t" ++ get_control_code(L);
						_ ->
							""
					end
			end;
		{obj, [{"anonymous", Anonymous},{"inputs", _},{"name", Name},{"type", Type}]} ->
			if
				Anonymous ->
					"";
				true ->
					case Type of
						<<"event">> ->
							"\"" ++ binary_to_list(Name) ++ "\" ->\r\n\t\t\tContent = evt_" ++ binary_to_list(Name) ++ "();\r\n\t\t" ++ get_control_code(L);
						_ ->
							""
					end
			end
	end.

get_api_code([], _) ->
	"";
get_api_code([AbiDef|L], ContractName) ->
	case AbiDef of
		{obj, [{"constant", Constant},{"inputs",InputList},{"name",Name},{"outputs",OutputList},{"type",Type}]} ->
			if
				Constant ->
					[{obj, [_, {"type", OutputType}]}|_] = OutputList,
					io:format("~p~n", [OutputList]),
					if
						is_list(OutputList) ->
							case InputList of
								[] ->
									"func_" ++ binary_to_list(Name) ++ "(_) ->\r\n\tetherlib:getMultiOutputValue(eth_propertyCall(?ACCOUNT,\"" ++ binary_to_list(Name) ++ "\"),[" ++ get_OutputTypeList(OutputList, 0) ++ "]).\r\n" ++ get_api_code(L, ContractName);
								[_] ->
									ParaNameString = string:join(get_InputNameList(InputList),","),
									FuncParaString = string:join(get_FunctionParaList(InputList),","),
									"func_" ++ binary_to_list(Name) ++ "(Params) ->\r\n\t[" ++ ParaNameString ++ "|_] = Params,\r\n\tetherlib:getMultiOutputValue(eth_propertyMappingCall(?ACCOUNT,\"" ++ binary_to_list(Name) ++ "\",[" ++ FuncParaString ++ "]),[" ++ get_OutputTypeList(OutputList, 0) ++ "]).\r\n" ++ get_api_code(L, ContractName)
							end;
						true ->
							IsArray = kylinfly_tool:str_endwith("[]", binary_to_list(OutputType)),
							IsBytesM = kylinfly_tool:str_beginwith("bytes", binary_to_list(OutputType)) and (not IsArray),
							case InputList of
								[] when OutputType =:= <<"int">>; OutputType =:= <<"int256">>; OutputType =:= <<"uint">>; OutputType =:= <<"uint256">> ->
									"func_" ++ binary_to_list(Name) ++ "(_) ->\r\n\tencode(etherlib:hex2de(eth_propertyCall(?ACCOUNT,\"" ++ binary_to_list(Name) ++ "\"))).\r\n" ++ get_api_code(L, ContractName);
								[] when OutputType =:= <<"string">>; OutputType =:= <<"bytes">> ->
									"func_" ++ binary_to_list(Name) ++ "(_) ->\r\n\tetherlib:getStringValue(eth_propertyCall(?ACCOUNT,\"" ++ binary_to_list(Name) ++ "\")).\r\n" ++ get_api_code(L, ContractName);
								[] when IsArray ->
									ArrayType = kylinfly_tool:str_replace("[]", "", binary_to_list(OutputType)),
									"func_" ++ binary_to_list(Name) ++ "(_) ->\r\n\tetherlib:getArrayValue(eth_propertyCall(?ACCOUNT,\"" ++ binary_to_list(Name) ++ "\"),\"" ++ ArrayType ++ "\").\r\n" ++ get_api_code(L, ContractName);
								[] when IsBytesM ->
									M = kylinfly_tool:str_replace("bytes", "", binary_to_list(OutputType)),
									"func_" ++ binary_to_list(Name) ++ "(_) ->\r\n\tetherlib:getByteValueList(eth_propertyCall(?ACCOUNT,\"" ++ binary_to_list(Name) ++ "\"), " ++ M ++ ", 0).\r\n" ++ get_api_code(L, ContractName);
								[] ->
									"func_" ++ binary_to_list(Name) ++ "(_) ->\r\n\teth_propertyCall(?ACCOUNT,\"" ++ binary_to_list(Name) ++ "\").\r\n" ++ get_api_code(L, ContractName);
								[_] when IsBytesM ->
									M = kylinfly_tool:str_replace("bytes", "", binary_to_list(OutputType)),
									ParaNameString = string:join(get_InputNameList(InputList),","),
									FuncParaString = string:join(get_FunctionParaList(InputList),","),
									"func_" ++ binary_to_list(Name) ++ "(Params) ->\r\n\t[" ++ ParaNameString ++ "|_] = Params,\r\n\tetherlib:getByteValueList(eth_propertyMappingCall(?ACCOUNT,\"" ++ binary_to_list(Name) ++ "\",[" ++ FuncParaString ++ "]), " ++ M ++ ", 0).\r\n" ++ get_api_code(L, ContractName);
								[_] ->
									ParaNameString = string:join(get_InputNameList(InputList),","),
									FuncParaString = string:join(get_FunctionParaList(InputList),","),
									"func_" ++ binary_to_list(Name) ++ "(Params) ->\r\n\t[" ++ ParaNameString ++ "|_] = Params,\r\n\teth_propertyMappingCall(?ACCOUNT,\"" ++ binary_to_list(Name) ++ "\",[" ++ FuncParaString ++ "]).\r\n" ++ get_api_code(L, ContractName)
							end
					end;
				true ->
					case Type of
						<<"function">> when OutputList =:= [] ->
							case InputList of
								[] ->
									"func_" ++ binary_to_list(Name) ++ "(_) ->\r\n\teth_methodCall(?ACCOUNT,\"" ++ binary_to_list(Name) ++ "\").\r\n" ++ get_api_code(L, ContractName);
								_ ->
									ParaNameString = string:join(get_InputNameList(InputList),","),
									FuncParaString = string:join(get_FunctionParaList(InputList),","),
									"func_" ++ binary_to_list(Name) ++ "(Params) ->\r\n\t[" ++ ParaNameString ++ "|_] = Params,\r\n\teth_methodCall(?ACCOUNT,\"" ++ binary_to_list(Name) ++ "\",[" ++ FuncParaString ++ "]).\r\n" ++ get_api_code(L, ContractName)
							end;		
						<<"function">> ->
							[{obj, [_, {"type", OutputType}]}|_] = OutputList,
							io:format("~p~n", [OutputList]),
							IsArray = kylinfly_tool:str_endwith("[]", binary_to_list(OutputType)),
							IsBytesM = kylinfly_tool:str_beginwith("bytes", binary_to_list(OutputType)) and (not IsArray),
							case InputList of
								[] when OutputType =:= <<"int">>; OutputType =:= <<"int256">>; OutputType =:= <<"uint">>; OutputType =:= <<"uint256">> ->
									"func_" ++ binary_to_list(Name) ++ "(_) ->\r\n\tencode(etherlib:hex2de(eth_propertyCall(?ACCOUNT,\"" ++ binary_to_list(Name) ++ "\"))).\r\n" ++ get_api_code(L, ContractName);
								[] when OutputType =:= <<"string">>; OutputType =:= <<"bytes">> ->
									"func_" ++ binary_to_list(Name) ++ "(_) ->\r\n\tetherlib:getStringValue(eth_propertyCall(?ACCOUNT,\"" ++ binary_to_list(Name) ++ "\")).\r\n" ++ get_api_code(L, ContractName);
								[] when IsArray ->
									ArrayType = kylinfly_tool:str_replace("[]", "", binary_to_list(OutputType)),
									"func_" ++ binary_to_list(Name) ++ "(_) ->\r\n\tetherlib:getArrayValue(eth_propertyCall(?ACCOUNT,\"" ++ binary_to_list(Name) ++ "\"),\"" ++ ArrayType ++ "\").\r\n" ++ get_api_code(L, ContractName);
								[] when IsBytesM ->
									M = kylinfly_tool:str_replace("bytes", "", binary_to_list(OutputType)),
									"func_" ++ binary_to_list(Name) ++ "(_) ->\r\n\tetherlib:getByteValueList(eth_propertyCall(?ACCOUNT,\"" ++ binary_to_list(Name) ++ "\"), " ++ M ++ ", 0).\r\n" ++ get_api_code(L, ContractName);
								[] ->
									"func_" ++ binary_to_list(Name) ++ "(_) ->\r\n\teth_propertyCall(?ACCOUNT,\"" ++ binary_to_list(Name) ++ "\").\r\n" ++ get_api_code(L, ContractName);
								_ ->
									ParaNameString = string:join(get_InputNameList(InputList),","),
									FuncParaString = string:join(get_FunctionParaList(InputList),","),
									if
										IsBytesM ->
											M = kylinfly_tool:str_replace("bytes", "", binary_to_list(OutputType)),
											"func_" ++ binary_to_list(Name) ++ "(Params) ->\r\n\t[" ++ ParaNameString ++ "|_] = Params,\r\n\tetherlib:getByteValueList(eth_propertyMappingCall(?ACCOUNT,\"" ++ binary_to_list(Name) ++ "\",[" ++ FuncParaString ++ "]), " ++ M ++ ", 0).\r\n" ++ get_api_code(L, ContractName);
										OutputType =:= <<"int[]">>; OutputType =:= <<"int256[]">>; OutputType =:= <<"uint[]">>; OutputType =:= <<"uint256[]">> ->
											"func_" ++ binary_to_list(Name) ++ "(Params) ->\r\n\t[" ++ ParaNameString ++ "|_] = Params,\r\n\tencode(etherlib:getArrayValue(eth_propertyMappingCall(?ACCOUNT,\"" ++ binary_to_list(Name) ++ "\",[" ++ FuncParaString ++ "]),\"uint\")).\r\n" ++ get_api_code(L, ContractName);
										OutputType =:= <<"byte[]">> ->
											"func_" ++ binary_to_list(Name) ++ "(Params) ->\r\n\t[" ++ ParaNameString ++ "|_] = Params,\r\n\tencode(etherlib:getArrayValue(eth_propertyMappingCall(?ACCOUNT,\"" ++ binary_to_list(Name) ++ "\",[" ++ FuncParaString ++ "]),\"bytes\")).\r\n" ++ get_api_code(L, ContractName);
										OutputType =:= <<"int">>; OutputType =:= <<"int256">>; OutputType =:= <<"uint">>; OutputType =:= <<"uint256">> ->
											"func_" ++ binary_to_list(Name) ++ "(Params) ->\r\n\t[" ++ ParaNameString ++ "|_] = Params,\r\n\tencode(etherlib:hex2de(eth_propertyMappingCall(?ACCOUNT,\"" ++ binary_to_list(Name) ++ "\",[" ++ FuncParaString ++ "]))).\r\n" ++ get_api_code(L, ContractName);
										OutputType =:= <<"string">>; OutputType =:= <<"bytes">> ->
											"func_" ++ binary_to_list(Name) ++ "(Params) ->\r\n\t[" ++ ParaNameString ++ "|_] = Params,\r\n\tetherlib:getStringValue(eth_propertyMappingCall(?ACCOUNT,\"" ++ binary_to_list(Name) ++ "\",[" ++ FuncParaString ++ "])).\r\n" ++ get_api_code(L, ContractName);
										true ->
											"func_" ++ binary_to_list(Name) ++ "(Params) ->\r\n\t[" ++ ParaNameString ++ "|_] = Params,\r\n\teth_propertyMappingCall(?ACCOUNT,\"" ++ binary_to_list(Name) ++ "\",[" ++ FuncParaString ++ "]).\r\n" ++ get_api_code(L, ContractName)
									end
							end;
						_ ->
							""
					end
			end;
		{obj, [{"anonymous", Anonymous},{"inputs", InputList},{"name", Name},{"type", Type}]} ->
			if
				Anonymous ->
					"";
				true ->
					case Type of
						<<"event">> ->
							kylinfly_monitor:register_event("contract_" ++ ContractName ++ "_api", "evt_" ++ binary_to_list(Name) ++ "()"),
							Data = etherlib:get_eventSignHash(binary_to_list(Name)++"("++etherlib:get_ParamsTypeStringEvent(InputList)++")"),
							"evt_" ++ binary_to_list(Name) ++ "() ->\r\n\tetherlib:eth_getEventLogs(?ACCOUNT,\"" ++ Data ++ "\").\r\n" ++ get_api_code(L, ContractName);
						_ ->
							""
					end
			end
	end.

set_Names([]) ->
	[];
set_Names([Input|L]) ->
	{obj, [{"name", _}, {"type", Type}]} = Input,
	Name = string:left(string:join(string:tokens(base64:encode_to_string(crypto:hash(md5,term_to_binary(erlang:unique_integer()))),"/+="),""),8),
	[{obj, [{"name", list_to_binary(Name)}, {"type", Type}]}|set_Names(L)].

get_ReadDataCode([Output|L]) ->
	[Output|L].

get_InputNameList([]) ->
	[];
get_InputNameList([Input|L]) ->
	{obj, [{"name", Name}, {"type", _}]} = Input,
	["P_" ++ binary_to_list(Name)|get_InputNameList(L)].

get_FunctionParaList([]) ->
	[];
get_FunctionParaList([Input|L]) ->
	{obj, [{"name", Name}, {"type", Type}]} = Input,
	IsArray = kylinfly_tool:str_endwith("[]", binary_to_list(Type)),
	if
		Type =:= <<"string">>; Type =:= <<"bytes">> ->
			["{\"" ++ binary_to_list(Type) ++ "\",P_" ++ binary_to_list(Name) ++ ",string:len(P_" ++ binary_to_list(Name) ++ "),32}"|get_FunctionParaList(L)];
		IsArray ->
			ArrayType = kylinfly_tool:str_replace("[]", "", binary_to_list(Type)),
			["{\"" ++ binary_to_list(Type) ++ "\",etherlib:genArrayValueInput(P_" ++ binary_to_list(Name) ++ ",\"" ++ ArrayType ++ "\"),string:len(etherlib:genArrayValueInput(P_" ++ binary_to_list(Name) ++ ",\"" ++ ArrayType ++ "\")),0}"|get_FunctionParaList(L)];
		true ->
			["{\"" ++ binary_to_list(Type) ++ "\",P_" ++ binary_to_list(Name) ++ ",64,0}"|get_FunctionParaList(L)]
	end.
	
get_OutputTypeList([], _) ->
	"";
get_OutputTypeList([Output|L], Index) ->
	{obj, [{_, Name},{_, Type}]} = Output,
	if
		Index =:= 0 ->
			"{obj,[{\"name\",<<\"" ++ binary_to_list(Name) ++ "\">>},{\"type\",<<\"" ++ binary_to_list(Type) ++ "\">>}]}" ++ get_OutputTypeList(L, Index + 1);
		true ->
			",{obj,[{\"name\",<<\"" ++ binary_to_list(Name) ++ "\">>},{\"type\",<<\"" ++ binary_to_list(Type) ++ "\">>}]}" ++ get_OutputTypeList(L, Index + 1)
	end.	
