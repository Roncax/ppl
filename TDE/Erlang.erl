====2020.09.03====

Erlang, Ex 6 (6 points)

Consider a binary tree encoded e.g. in this way

{node 1 {node 2 nil nil} {node 3 nil nil}}.

Write a procedure which takes a binary tree containing function objects, then
launches a process executing each function, and returns a list of their PIDs.
Write a procedure which takes a list of PIDs and send a 'stop' signal to each of
them, waiting for an acknowledgment from them.

btree2proc(nil) ->
    [];

btree2proc({node, F, L, R}) ->
    P = spawn(?MODULE, F, [[]]),
    btree2proc(L) ++ [P] ++ btree2proc(R).

stopList([]) -> ok;
stopList([P|Ps]) -> P!stop,
                    receive
                        {ack, P} -> ok
                    end,
                    stopList(Ps).


====2020.07.17====

Define a "broadcaster" process which answers to the following
commands:

- {spawn, L, V} creates a process for each element of L, passing its
initial parameter in V, where L is a list of names of functions
defined in the current module and V is their respective parameters (of
course it must be |L| = |V|);

- {send, V}, with V a list of values, sends to each respective process
created with the previous spawn command a message in V; e.g. {spawn,
[1,2,3]} will send 1 to the first process, 2 to the second, and 3 to
the third;

- stop is used to end the broadcaster, and to also stop every process
spawned by it.

SOLUTION
broadcaster(Pids) ->
    receive
        {spawn, Fs, Vs} ->
            FDs = lists:zip(Fs, Vs),
            io:format("~p~n", [FDs]),
            broadcaster([spawn_link(?MODULE, F, V) || {F,V} <- FDs]);
        {send, Vs} ->
            FDs = lists:zip(Pids, Vs),
            io:format("~p~n", [FDs]),
            [ Pid ! V || {Pid, V} <- FDs];
        stop ->
            ok
    end.
    
    

====2020.06.29====
ERLANG
Define a "functional" process buffer, called fuffer, that stores only one 
value and may receive messages only from its creator. fuffer can receive the following commands:
'set' to store a new value
'get' to obtain the current value
'apply F' to apply the function F to the stored value
'die' to end
'duplicate' to create (and return) an exact copy of itself

fuffer(Data, PID) ->
    receive
        {set, PID, V} ->
            fuffer(V, PID);
        {get, PID} ->
            PID!Data, fuffer(Data, PID);
        {apply, PID, F} ->
            fuffer(F(Data), PID);
        {die, PID} -> ok;
        {duplicate, PID} ->
            PID ! spawn(?MODULE, fuffer, [Data, PID]),
            fuffer(Data, PID)
    end.


====2020.02.07====
We want to create a simplified implementation of the “Reduce” part of the MapReduce paradigm. To this
end, define a process “reduce_manager” that keeps track of a pool of reducers. When it is created, it
stores a user-defined associative binary function ReduceF. It receives messages of the form {reduce,
Key, Value}, and forwards them to a different “reducer” process for each key, which is created lazily
(i.e. only when needed). Each reducer serves requests for a unique key.
Reducers keep into an accumulator variable the result of the application of ReduceF to the values they
receive. When they receive a new value, they apply ReduceF to the accumulator and the new value,
updating the former. When the reduce_manager receives the message print_results, it makes all its
reducers print their key and incremental result.

For example, the following code (where the meaning of string:split should be clear from the context):

word_count(Text) ->
	RMPid = start_reduce_mgr(fun (X, Y) -> X + Y end),
	lists:foreach(fun (Word) -> RMPid ! {reduce, Word, 1} end, string:split(Text, " ", all)),
	RMPid ! print_results,
	ok.

causes the following print:
mapreduce:word_count("sopra la panca la capra campa sotto la panca la capra crepa").
sopra: 1
la: 4
panca: 2
capra: 2
campa: 1
sotto: 1
crepa: 1
ok

SOLUTION
start_reduce_mgr(ReduceF) ->
	spawn(?MODULE, reduce_mgr, [ReduceF, #{}]).

reduce_mgr(ReduceF, Reducers) ->
	receive
		print_results ->
			lists:foreach(fun ({_, RPid}) -> RPid ! print_results end, maps:to_list(Reducers));
		{reduce, Key, Value} ->
				case Reducers of
					#{Key := RPid} ->
						RPid ! {Key, Value},
						reduce_mgr(ReduceF, Reducers);
					_ ->
						NewReducer = spawn(?MODULE, reducer, [ReduceF, Key, Value]),
						reduce_mgr(ReduceF, Reducers#{Key => NewReducer})
						end
				end.

reducer(ReduceF, Key, Result) ->
	receive
		print_results ->
			io:format("~s: ~w~n", [Key, Result]);
		{Key, Value} ->
			reducer(ReduceF, Key, ReduceF(Result, Value))
	end.



====2020.01.15====
We want to implement something like Python’s range in Erlang, using processes.

E.g.
 R = range(1,5,1) % starting value, end value, step
 next(R) % is 1
 next(R) % is 2
 …
 next(R) % is 5
 next(R) % is the atom stop_iteration

Define range and next, where range creates a process that manages the iteration, 
and next a function that talks with it, asking the current value

range(start, end, step) ->
	curr = start,
	receive
		{next, PID} ->
			curr = curr + step
			if
				curr >= end -> PID! stop_iteration;
				true -> PID!{curr, self()},
						range(curr, end, step) 
	end.

next(R) ->
	R!{next, self()}
	receive ->
		{curr, R} -> io:format("~s", curr);
	end.



====2019.09.03====
1) Define a split function, which takes a list and a number n and returns a pair of lists, where the first one
is the prefix of the given list, and the second one is the suffix of the list of length n.

E.g. split([1,2,3,4,5], 2) is {[1,2,3],[4,5]}.

2) Using split of 1), define a splitmap function which takes a function f, a list L, and a value n, and splits
L with parameter n, then launches two process to map f on each one of the two lists resulting from the
split. The function splitmap must return a pair with the two mapped lists.

SOLUTION

helper(E, {0, L}) ->
	{-1, [[E]|L]};

helper(E, {V, [X|Xs]}) ->
	{V-1, [[E|X]|Xs]}.

split(L, N) ->
	{_, R} = lists:foldr(fun helper/2, {N, [[]]}, L),
	R.

mapper(F, List, Who) ->
	Who ! {self(), lists:map(F, List)}.

splitmap(F, L, N) ->
	[L1, L2] = split(L, N),
	P1 = spawn(?MODULE, mapper, [F, L1, self()]),
	P2 = spawn(?MODULE, mapper, [F, L2, self()]),
	receive
		{P1, V1} ->
			receive {P2, V2} ->
				{V1, V2}
			end
	end.




====2019.07.24====
Define a master process which takes a list of nullary (or 0-arity) functions, and starts a worker process for
each of them. The master must monitor all the workers and, if one fails for some reason, must re-start it to
run the same code as before. The master ends when all the workers are done.
Note: for simplicity, you can use the library function spawn_link/1, which takes a lambda function, and
spawns and links a process running it

SOLUTION
listlink([], Pids) -> Pids;
listlink([F|Fs], Pids) ->
	Pid = spawn_link(F),
	listlink(Fs, Pids#{Pid => F}).

master(Functions) ->
	process_flag(trap_exit, true),
	Workers = listlink(Functions, #{}),
	master_loop(Workers, length(Functions)).

master_loop(Workers, Count) ->
	receive
		{'EXIT', Child, normal} ->
			if
				Count =:= 1 -> ok;
				true -> master_loop(Workers, Count-1)
			end;
		{'EXIT', Child, _} ->
			#{Child := Fun} = Workers,     <---- recupero cosi 'Fun' dalla map!
			Pid = spawn_link(Fun),
			master_loop(Workers#{Pid => Fun}, Count)
	end.
	





















