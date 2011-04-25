%%%-------------------------------------------------------------------
%%% File    : optimize.erl
%%% Author  :  <ecka@ECKAX>
%%% Description : General optimizaton algorithms
%%%
%%% Created : 30 May 2009 by  <ecka@ECKAX>
%%%-------------------------------------------------------------------
-module(optimize).

-export([hill_climb/2, hill_climb/3, climb_until/3, climb_until/4, 
	 simulated_annealing/5]).

-define(MAX, 100).

-include("mnesia_defs.hrl").

hill_climb(OptimizeFun, Dimension) when is_function(OptimizeFun),
					is_integer(Dimension) ->
    Start = create_point(Dimension, ?MAX),
    hill_climb(OptimizeFun, Start);
hill_climb(OptimizeFun, StartPoint) when is_function(OptimizeFun),
					 is_tuple(StartPoint)->
    Value = apply(OptimizeFun, [StartPoint]),
    hill_climb(OptimizeFun, StartPoint, {Value, StartPoint}).
    
hill_climb(OptimizeFun, Dimension, Max) when is_function(OptimizeFun),
					     is_integer(Dimension),
					     is_integer(Max) ->
    Start = create_point(Dimension, Max),
    hill_climb(OptimizeFun, Start);    
hill_climb(OptimizeFun, Point, CurrentSolution) ->
    Neighbours = get_all_neighbours(Point),
    Solutions = calculate_neighbours(OptimizeFun, Neighbours),
    case calculate_result(Solutions, CurrentSolution) of
	CurrentSolution ->
	    CurrentSolution;
	{_MaxSolution, MaxPoint} = Max -> 
	    hill_climb(OptimizeFun, MaxPoint, Max)
    end.

simulated_annealing(OptimizeFun, StartPoint, Temperature, Reduction, 
		    Iterations) when is_tuple(StartPoint) ->
    Value = apply(OptimizeFun, [StartPoint]),
    random:seed(now()),
    simulated_annealing(OptimizeFun, {Value, StartPoint}, {Value, StartPoint}, 
			Temperature, Reduction, Iterations);
simulated_annealing(OptimizeFun, Dimension, Temperature, Reduction, 
		    Iterations) ->
    Start = create_point(Dimension, ?MAX),
    Value = apply(OptimizeFun, [Start]),
    random:seed(now()),
    simulated_annealing(OptimizeFun, {Value, Start}, {Value, Start}, 
			Temperature, Reduction, Iterations).

simulated_annealing(_, _, Best, _, _, 0) ->
    Best;
simulated_annealing(OptimizeFun, {PreviousValue, Point}=Previous, 
		    {BestSolution, _BestPoint}=Best,Temperature, Reduction, 
		    Iterations) ->
    Neighbours = get_all_neighbours(Point),
    Selected = lists:nth(random:uniform(length(Neighbours)), Neighbours),
    Value = apply(OptimizeFun, [Selected]),
    case Value - PreviousValue of
	Delta when Delta >= 0 ->
	    NewBest = 
		if
		    Value > BestSolution ->
			{Value, Selected};
		    true ->
			Best
		end,
	    simulated_annealing(OptimizeFun, {Value, Selected}, NewBest,
				Reduction*Temperature, Reduction, Iterations-1);
	Delta when Delta < 0 -> 
	    case math:exp(Delta/Temperature) > random:uniform() of
		true ->
		    simulated_annealing(OptimizeFun, {Value, Selected},
					Best, Reduction*Temperature, Reduction,
					Iterations-1);
		false ->
		    simulated_annealing(OptimizeFun, Previous,
					Best, Reduction*Temperature, Reduction,
					Iterations-1)
	    end
    end.

calculate_result(Result, {MaxSolution, _MaxPoint}=Max) ->
    NewMax = lists:max(Result),
    NewMaxSolution = element(1, NewMax),
    if 
	NewMaxSolution > MaxSolution ->
	    NewMax;
	true ->
	    Max
    end.

calculate_neighbours(OptimizeFun, Neighbours) ->
    ParentPid = self(),
    Pids = [{spawn(fun() -> 
			   Money = apply(OptimizeFun, [Point]),
			   ParentPid ! {self(), Money}
		   end), Point} 
	    || Point <- Neighbours],
    [receive {Pid, Money} -> {Money, Point} end || {Pid, Point} <- Pids].
    
climb_until(OptimizeFun, Dimension, Sum) ->
    climb_until(OptimizeFun, Dimension, Sum, ?MAX).
climb_until(OptimizeFun, Dimension, Sum, Max) -> 
    R = optimize:hill_climb(OptimizeFun, Dimension, Max), 
    case element(1,R) > Sum of 
	true -> 
	    R; 
	false ->
	    climb_until(OptimizeFun, Dimension, Sum)
    end.

get_all_neighbours({P1}) ->
    [{P1-1}, {P1+1}];
get_all_neighbours({P1, P2} = P) ->
    L = [-1, 0, 1],
    AllPoints = [{P1+X, P2+Y} || X <- L, Y <- L],
    lists:delete(P, AllPoints);
get_all_neighbours({P1, P2, P3} = P) ->
    L = [-1, 0, 1],
    AllPoints = [{P1+X, P2+Y, P3+Z} || X <- L, Y <- L, Z <- L],
    lists:delete(P, AllPoints);
get_all_neighbours({P1, P2, P3, P4} = P) ->
    L = [-1, 0, 1],
    AllPoints = [{P1+X, P2+Y, P3+Z, P4+A} || X <- L, Y <- L, Z <- L, A <- L],
    lists:delete(P, AllPoints);
get_all_neighbours({P1, P2, P3, P4, P5} = P) ->
    L = [-1, 0, 1],
    AllPoints = [{P1+X, P2+Y, P3+Z, P4+A, P5+B} 
		 || X <- L, Y <- L, Z <- L, A <- L, B <- L],
    lists:delete(P, AllPoints);
get_all_neighbours({P1, P2, P3, P4, P5, P6} = P) ->
    L = [-1, 0, 1],
    AllPoints = [{P1+X, P2+Y, P3+Z, P4+A, P5+B, P6+C} 
		 || X <- L, Y <- L, Z <- L, A <- L, B <- L, C <- L],
    lists:delete(P, AllPoints);
get_all_neighbours({P1, P2, P3, P4, P5, P6, P7} = P) ->
    L = [-1, 0, 1],
    AllPoints = [{P1+X, P2+Y, P3+Z, P4+A, P5+B, P6+C, P7+D} 
		 || X <- L, Y <- L, Z <- L, A <- L, B <- L, C <- L, D <- L],
    lists:delete(P, AllPoints);
get_all_neighbours({P1, P2, P3, P4, P5, P6, P7, P8} = P) ->
    L = [-1, 0, 1],
    AllPoints = [{P1+X, P2+Y, P3+Z, P4+A, P5+B, P6+C, P7+D, P8+E} 
		 || X <- L, Y <- L, Z <- L, A <- L, B <- L, C <- L, D <- L,
		    E <- L],
    lists:delete(P, AllPoints);
get_all_neighbours({P1, P2, P3, P4, P5, P6, P7, P8, P9} = P) ->
    L = [-1, 0, 1],
    AllPoints = [{P1+X, P2+Y, P3+Z, P4+A, P5+B, P6+C, P7+D, P8+E, P9+F} 
		 || X <- L, Y <- L, Z <- L, A <- L, B <- L, C <- L, D <- L,
		    E <- L, F <- L],
    lists:delete(P, AllPoints);
get_all_neighbours({P1, P2, P3, P4, P5, P6, P7, P8, P9, P10} = P) ->
    L = [-1, 0, 1],
    AllPoints = [{P1+X, P2+Y, P3+Z, P4+A, P5+B, P6+C, P7+D, P8+E, P9+F, P10+H} 
		 || X <- L, Y <- L, Z <- L, A <- L, B <- L, C <- L, D <- L,
		    E <- L, F <- L, H <- L],
    lists:delete(P, AllPoints).

create_point(Dimension, Max) ->
    erlang:make_tuple(Dimension, 0,
		      [{X, random:uniform(Max)}
		       || X <- lists:seq(1, Dimension)]).
