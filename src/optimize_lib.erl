%%%-------------------------------------------------------------------
%%% File    : optimize.erl
%%% Author  :  <ecka@ECKAX>
%%% Description : General library functions
%%%-------------------------------------------------------------------
-module(optimize_lib).

-export([calculate_result/2,
	calculate_neighbours/2,
	create_point/2,
	get_all_neighbours/1]).

create_point(Dimension, Max) ->
    erlang:make_tuple(Dimension, 0,
		      [{X, random:uniform(Max)}
		       || X <- lists:seq(1, Dimension)]).

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
