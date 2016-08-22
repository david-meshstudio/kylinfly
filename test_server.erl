-module(test_server).  
-behaviour(gen_server).  
-export([start/1, init/1, handle_call/3, handle_cast/2, handle_info/2,  
  terminate/2, code_change/3]).  
  
start(Module) ->  
  io:format("test server start ~p~n", [Module]),  
  gen_server:start_link({local, Module}, ?MODULE, [], []).  
  
init([]) ->  
  io:format("test server init~n"),
  kylinfly_server:start(),
  {ok, []}.  
  
handle_call({test_handler, test, Name}, _From, State) ->  
  io:format("test server test ~p~n", [Name]),
  kylinfly_server:reload(),
  {reply, ok, State};  
  
handle_call(_Request, _From, State) ->  
  io:format("test server call nothing~n"),  
  Reply = ok,  
  {reply, Reply, State}.  
  
handle_cast(_Msg, State) ->  
  {noreply, State}.  
  
handle_info(_Info, State) ->  
  {noreply, State}.  
  
terminate(_Reason, _State) ->  
  ok.  
  
code_change(_OldVsn, State, _Extra) ->  
  {ok, State}.