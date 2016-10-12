-module(kylinfly_monitor).
-compile(export_all).

event_check() ->
	EventList = qiniulib:downloadObj("eventlist"),
	check_contract(EventList).

check_contract([]) ->
	0;
check_contract([{Module, Func}|L]) ->
	Info = apply(Module, Func, []),
	apply_service(Info, Module),
	check_contract(L).

apply_service(Info, Module) ->
	case Info of
		[] ->
			body;
		[State|L1] ->
			case State of
				"f100000000000000000000000000000000000000000000000000000000000000" ->
					case L1 of
						[Cmd, P1, P2, C1, C2, UID, Callback|_] when Cmd =:= "ss_add" ->
							Res = keyxxapi:add(P1, P2, C1, C2, UID),
							callback(Res, Module, Callback);
						[Cmd, P, C1, C2, UID, Callback|_] when Cmd =:= "ss_multiply" ->
							Res = keyxxapi:multiply(P, C1, C2, UID),
							callback(Res, Module, Callback)
					end;
				"f200000000000000000000000000000000000000000000000000000000000000" ->
				    none
			end
	end.

callback(Res, Module, Callback) ->
	apply(Module, Callback, [Res]).