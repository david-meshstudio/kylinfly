-module(kylinfly_timer).
-compile(export_all).

start() ->
	timer:apply_interval(1000, kylinfly_monitor, event_check, []).