%%%-------------------------------------------------------------------
%%% File    : ga.erl
%%% Author  :  <eeriaxl@EV001A4B76217E>
%%% Description : This module will optimize according to a Genetic Algorithms
%%% a stock scenario using the esim module. 
%%% 
%%%
%%% Created :  8 May 2009 by  <eeriaxl@EV001A4B76217E>
%%%-------------------------------------------------------------------
-module(ga).

-export([run/4]).

-define(MAX, 100).

run(OptimizeFun, Dimension, Mutation, Generations) ->
    random:seed(now()),
    StartList = [create_point(Dimension, ?MAX) || _X <- lists:seq(1,5)],
    ChromosomeList = encode_params(StartList),
    loop(OptimizeFun, ChromosomeList, Mutation, Generations).

loop(OptimizeFun, ChromosomeList, _Mutation, 0) ->
    Pid = self(),
    Pids = lists:map(
	     fun(Chromosome) ->
		     Point = 
			 decode_chromosome(Chromosome),
		     Fun = 
			 fun() ->
				 Return = apply(OptimizeFun, [Point]),
				 Pid ! {end_of_sim, self(), Return}
			 end,
		     {spawn(Fun), Point}
	     end, ChromosomeList),
    ValueList = 
	[receive {end_of_sim, LPid, Return} -> {Return, LPoint} end ||
						{LPid, LPoint} <- Pids],        
    lists:max(ValueList);

loop(OptimizeFun, ChromosomeList, Mutation, Generations) ->
    Pid = self(),
    Pids = 
	lists:map(fun(Chromosome) ->
			  Point = 
			      decode_chromosome(Chromosome),
			  Fun = 
			      fun() ->
				      Return = apply(OptimizeFun, [Point]),
				      Pid ! {end_of_sim, self(), Return}
			      end,
			  {spawn(Fun), Chromosome}
		  end, ChromosomeList),
    ValueList = 
	[receive {end_of_sim, LPid, Return} -> {Return, LChromosome} end ||
						{LPid, LChromosome} <- Pids],
    SortedVals = lists:reverse(lists:keysort(1, ValueList)),
    NewChromosomeList = do_crossover(SortedVals, Mutation),
    loop(OptimizeFun, NewChromosomeList, Mutation, Generations-1).

do_crossover(SortedVals, Mutation) ->
    {Parents, _} = lists:split(trunc(length(SortedVals)/2), SortedVals),
    ParentChromosomes = [C || {_, C}<- Parents],
    Offspring = cross_parents(ParentChromosomes, [], Mutation),
    MutatedParents = lists:map(fun(X) ->
				       mutate(X, [], Mutation)
			       end, ParentChromosomes),
    lists:append([MutatedParents, Offspring]).

cross_parents([], AccOffspring, _Mutation) ->
    AccOffspring;
cross_parents([First, Second | Rest], AccOffspring, Mutation) ->
    SplitPoint = trunc(length(binary_to_list(First))/2),
    {First1, First2} = lists:split(SplitPoint, binary_to_list(First)),
    {Second1, Second2} = lists:split(SplitPoint, binary_to_list(Second)),
    NewOffspring1 =  mutate(list_to_binary(lists:append([First1, Second2])), 
			    [], Mutation),
    NewOffspring2 =  mutate(list_to_binary(lists:append([Second1, First2])), 
			    [], Mutation),

    cross_parents(Rest, [NewOffspring1, NewOffspring2 | AccOffspring], 
		  Mutation).

mutate(<<>>, BitList, _Mutation) ->
    << <<Bit:1>> || Bit <- BitList>>;
mutate(<<X:1, BitString/bitstring>>, AccList, Mutation) ->
    Rand = random:uniform(100),
    if
	Rand > Mutation ->
	    case X of
		1 ->
		    mutate(BitString, [0 | AccList], Mutation);
		0 ->
		    mutate(BitString, [1 | AccList], Mutation)
	    end;
	true ->
	    mutate(BitString, [X | AccList], Mutation)
    end.

encode_params(AllParams) ->
    lists:map(fun(ParamList) when is_tuple(ParamList) ->
		      list_to_binary(tuple_to_list(ParamList))
	      end, AllParams).

decode_chromosome(BinaryList) ->
    list_to_tuple(binary_to_list(BinaryList)).

create_point(Dimension, Max) ->
    erlang:make_tuple(Dimension, 0,
		      [{X, random:uniform(Max)}
		       || X <- lists:seq(1, Dimension)]).
