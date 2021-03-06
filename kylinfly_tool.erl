-module(kylinfly_tool).
-compile(export_all).

str_replace(Find, Replace, String) ->
	Start = string:str(String, Find),
	if
		Start > 0 ->
			Res = string:sub_string(String, 1, Start - 1) ++ Replace ++ string:sub_string(String, Start + string:len(Find)),
			Res2 = str_replace(Find, Replace, Res),
			if
				Res =:= Res2 ->
					Res;
				true ->
					Res2
			end;
		true ->
			String
	end.

str_replace_list([], _, String) ->
	String;
str_replace_list([Find|L], Replace, String) ->
	str_replace_list(L, Replace, str_replace(Find, Replace, String)).

str_beginwith(Find, String) ->
	Start = string:str(String, Find),
	if
		Start =:= 1 ->
			true;
		true ->
			false
	end.

str_endwith(Find, String) ->
	Start = string:str(String, Find),
	Len1 = string:len(String),
	Len2 = string:len(Find),
	if
		Start =:= Len1 - Len2 + 1 ->
			true;
		true ->
			false
	end.