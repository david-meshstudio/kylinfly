-module(test_sup).  
-behaviour(supervisor).  
-export([start_link/1, init/1]).  
  
start_link(_) ->  
  io:format("test sup start link~n"),  
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).  
  
  
init([]) ->  
  io:format("test sup init~n"),  
  {ok,{  
    {one_for_all, 1, 60},  
    [{  test_handler_worker,    
      {  test_server, start, [test_handler] },   
      permanent,   
      2000,  
      worker,   
      [test_server,test_handler]  
      }]}  
  }. 