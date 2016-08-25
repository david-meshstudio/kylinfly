-module(kylinfly_service).
-compile(export_all).

kfs_getCommandCode(FuncCall) ->
	{Command, [Mode|Params], Name} = FuncCall,
	case Command of
		"Record" ->
			[Key, Value|_] = Params,
			case Mode of
				"add" ->
					"kfs_func_record_add(\""++Name++"\",\""++Key++"\",\""++Value++"\"),";
				"remove" ->
					"kfs_func_record_remove(\""++Name++"\",\""++Key++"\",\""++Value++"\"),"
			end;
		"Crypto" ->
			[Key, Algorithm|_] = Params,
			case Algorithm of
				"AES" ->
					Key;
				"DES" ->
					Key
			end
	end.