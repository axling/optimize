%%%-------------------------------------------------------------------
%%% File    : optimize.erl
%%% Author  :  <ecka@ECKAX>
%%% Description : General optimizaton algorithms
%%%
%%% Created : 30 May 2009 by  <ecka@ECKAX>
%%%-------------------------------------------------------------------
-module(optimize).

-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    case optimize_sup:start_link() of
	{ok, Pid} -> 
	    {ok, Pid};
	Error ->
	    Error
    end.

stop(_State) ->
    ok.    




    


