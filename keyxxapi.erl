-module(keyxxapi).
-compile(export_all).
-import(rfc4627,[encode/1,decode/1]).
-define(ServiceURL, "http://localhost:8369/api/keyxx_controller:base").

call(Params) ->
	inets:start(),
	case httpc:request(post,{?ServiceURL,[],"application/x-www-form-urlencoded",encode(Params)},[],[]) of
		{ok, {_, _, Body}} -> Body;
		{error, Reason} -> io:format("error cause ~p~n", [Reason])
	end.	

add(P1, P2, C1, C2, UID) ->
	Params = ["add", [P1, P2, C1, C2, UID]],
	call(Params).

multiply(P, C1, C2, UID) ->
	Params = ["multiply", [P, C1, C2, UID]],
	call(Params).
