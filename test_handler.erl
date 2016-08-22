-module(test_handler).  
-export([test/1, stop/0]).  
-define(Server, ?MODULE).  
  
test(Name) ->  
  io:format("test handler test~n"),  
  gen_server:call(?Server, {?MODULE, test, Name}).
  
stop() ->  
  io:format("test handler stop~n"),  
  exit(whereis(?Server), kill),  
  io:format("test handler is killed~n").  