%%%-------------------------------------------------------------------
%%% File    : optimize_sa.erl
%%% Author  :  <ecka@ECKAX>
%%% Description : Simulated Annealing
%%%-------------------------------------------------------------------
-module(optimize_sa).

-export([simulated_annealing/5]).

-define(MAX, 100).

simulated_annealing(OptimizeFun, StartPoint, Temperature, Reduction, 
		    Iterations) when is_tuple(StartPoint) ->
    Value = apply(OptimizeFun, [StartPoint]),
    random:seed(now()),
    simulated_annealing(OptimizeFun, {Value, StartPoint}, {Value, StartPoint}, 
			Temperature, Reduction, Iterations);
simulated_annealing(OptimizeFun, Dimension, Temperature, Reduction, 
		    Iterations) ->
    Start = optimize_lib:create_point(Dimension, ?MAX),
    Value = apply(OptimizeFun, [Start]),
    random:seed(now()),
    simulated_annealing(OptimizeFun, {Value, Start}, {Value, Start}, 
			Temperature, Reduction, Iterations).

simulated_annealing(_, _, Best, _, _, 0) ->
    Best;
simulated_annealing(OptimizeFun, {PreviousValue, Point}=Previous, 
		    {BestSolution, _BestPoint}=Best,Temperature, Reduction, 
		    Iterations) ->
    Neighbours = optimize_lib:get_all_neighbours(Point),
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
