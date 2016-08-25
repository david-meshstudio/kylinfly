-module(kylinfly_compiler).
-compile(export_all).

kfc_isMark(Part) ->
	case string:len(Part) < 3 of
		true ->
			false;
		false ->
			[C1, C2, C3|_] = Part,
			case [C1, C2, C3] of
				"KF_" -> true;
				_ -> false
			end
	end.

kfc_findFunctionName([]) ->
	"no";
kfc_findFunctionName([FLP|L]) ->
	case FLP of
		"function" ->
			[Name|_] = L,
			Name;
		_ ->
			kfc_findFunctionName(L)
	end.

kfc_getFunctionName(FL) ->
	FLs = string:tokens(FL, " ()"),
	kfc_findFunctionName(FLs).

kfc_getMarklist([]) ->
	[];
kfc_getMarklist([Part|L]) ->
	case kfc_isMark(Part) of
		true ->
			[FL|_] = L,
			Name = kfc_getFunctionName(FL),
			[Part++Name|kfc_getMarklist(L)];
		false -> 
			kfc_getMarklist(L)
	end.

kfc_getSoliditylist([]) ->
	[];
kfc_getSoliditylist([Part|L]) ->
	case kfc_isMark(Part) of
		false -> [Part|kfc_getSoliditylist(L)];
		true -> kfc_getSoliditylist(L)
	end.

kfc_compileMarklist([]) ->
	[];
kfc_compileMarklist([Mark|L]) ->
	Mark1 = tool_removeString(" ", Mark),
	[Function, ParamString, Name|_] = string:tokens(Mark1, "()"),
	[_, _, _|FuncName] = Function,
	Params = string:tokens(ParamString, ","),
	[{FuncName, Params, Name}|kfc_compileMarklist(L)].

kfc_compileSolidityCodeWithKylinflyMark(Source) ->
	Codelist = string:tokens(Source, "@\r\n\t"),
	Marklist = kfc_getMarklist(Codelist),
	Soliditylist = kfc_getSoliditylist(Codelist),
	SolidityCompileResult = etherlib:eth_compileSolidityCodelist(Soliditylist),
	MarkCompileResult = kfc_compileMarklist(Marklist),
	{SolidityCompileResult, MarkCompileResult}.

kfc_compileSolidityCodeWithKylinflyMarkFromQiniuFile(File) ->
	Source = qiniulib:download(File),
	kfc_compileSolidityCodeWithKylinflyMark(Source).

tool_removeString(Needle, Haystack) ->
	List = string:tokens(Haystack, Needle),
	string:join(List, "").