%%%-------------------------------------------------------------------
%%% File    : optimize.erl
%%% Author  :  <ecka@ECKAX>
%%% Description : General optimizaton algorithms
%%%
%%% Created : 30 May 2009 by  <ecka@ECKAX>
%%%-------------------------------------------------------------------
-module(optimize_hill_climb).

-export([hill_climb/2, 
	 hill_climb/3, 
	 climb_until/3, 
	 climb_until/4]).

-define(MAX, 100).

hill_climb(OptimizeFun, Dimension) when is_function(OptimizeFun),
					is_integer(Dimension) ->
    Start = optimize_lib:create_point(Dimension, ?MAX),
    hill_climb(OptimizeFun, Start);
hill_climb(OptimizeFun, StartPoint) when is_function(OptimizeFun),
					 is_tuple(StartPoint)->
    Value = apply(OptimizeFun, [StartPoint]),
    hill_climb(OptimizeFun, StartPoint, {Value, StartPoint}).
    
hill_climb(OptimizeFun, Dimension, Max) when is_function(OptimizeFun),
					     is_integer(Dimension),
					     is_integer(Max) ->
    Start = optimize_lib:create_point(Dimension, Max),
    hill_climb(OptimizeFun, Start);    
hill_climb(OptimizeFun, Point, CurrentSolution) ->
    Neighbours = optimize_lib:get_all_neighbours(Point),
    Solutions = optimize_lib:calculate_neighbours(OptimizeFun, Neighbours),
    case optimize_lib:calculate_result(Solutions, CurrentSolution) of
	CurrentSolution ->
	    CurrentSolution;
	{_MaxSolution, MaxPoint} = Max -> 
	    hill_climb(OptimizeFun, MaxPoint, Max)
    end.

climb_until(OptimizeFun, Dimension, Sum) ->
    climb_until(OptimizeFun, Dimension, Sum, ?MAX).
climb_until(OptimizeFun, Dimension, Sum, Max) -> 
    R = hill_climb(OptimizeFun, Dimension, Max), 
    case element(1,R) > Sum of 
	true -> 
	    R; 
	false ->
	    climb_until(OptimizeFun, Dimension, Sum)
    end.
