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




====2019.06.28====
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
	

====2019.01.16====
Define a process P, having a local behavior (a function), that answer to three commands:
- load is used to load a new function f on P: the previous behavior is composed with f;
- run is used to send some data D to P: P returns its behavior applied to D;
- stop is used to stop P.
For security reasons, the process must only work with messages coming from its creator: other messages
must be discarded.

SOLUTION
cam(Beh, Who) ->
	receive
		{run, Who, What} ->
			Who ! Beh(What),
			cam(Beh, Who);
		{load, Who, Code} ->
			cam(fun (X) -> Code(Beh(X)) end, Who);
		{stop, Who} ->
			ok;
		_ -> cam(Beh, Who)
	end



====2018.09.05====
Define a function create_pipe, which takes a list of names and creates a process of each element of the list,
each process registered as its name in the list; e.g. with [one, two], it creates two processes called ‘one’ and
‘two’. The processes are “connected” (like in a list, there is the concept of “next process”) from the last to
the first, e.g. with [one, two, three], the process structure is the following:
three → two → one → self,
this means that the next process of ‘three’ is ‘two’, and so on; self is the process that called create_pipe.
Each process is a simple repeater, showing on the screen its name and the received message, then sends it to
the next process.
Each process ends after receiving the ‘kill’ message, unregistering itself.


SOLUTION
repeater(Next, Name) ->
	receive
		kill ->
			unregister(Name),
			Next ! kill;
		V ->
			io:format("~p got ~p~n", [Name, V]),
			Next ! V,
			repeater(Next, Name)
	end.

create_pipe([], End) -> End;
create_pipe([X|Xs], Next) ->
	P = spawn(?MODULE, repeater, [Next, X]),
	register(X, P),
	create_pipe(Xs, X).

Exercise 2, Erlang (10 pts)
The fixed-point of a function f and a starting value x is the value v = fk(x), with k > 0, such that fk(x) = fk+1(x). We want to
implement a fixed-point code using two communicating actors:
1) Define the function for an applier actor, which has a state S, holding a value, and receives a function f from other actors: if S =
f(S), it sends back the result S and ends it computation; otherwise sends back a message to state that the condition S = f(S) has not
been reached.
2) Define a function called fix, which takes as input a function and a starting value, and creates and uses an applier actor to
implement the fixed-point.

SOLUTION

applier(State) ->
	receive
		{Sender, F} -> NewState = F(State),
					if
						NewState =:= State -> Sender!{self(), State};
						true -> Sender!{self(), no}, applier(NewState)
					end
	end.

loop(P, F) ->
	P!{self(), F},
		receive
			{P, V} -> 
			if
				V =:= no -> loop(P, F);
				true -> V
			end
	end.

fix(F, V) ->
	A = spawn(?MODULE, applier, [V]),
	loop(A, F)



====2021.01.20====
Define a function for a proxy used to avoid to send PIDs; the proxy must react to the following messages:

- {remember, PID, Name}: associate the value Name with PID.

- {question, Name, Data}: send a question message containing Data to the PID corresponding to the value Name (e.g. an atom), like in PID ! {question, Data}

- {answer, Name, Data}: send an answer message containing Data to the PID corresponding to the value Name (e.g. an atom), like in PID ! {answer, Data}



SOLUTION
proxy(Table) ->
    receive
        {question, Name, Data} ->
            #{Name := Id} = Table,
            Id ! {question, Data},
            proxy(Table);
        {answer, Name, Data} ->
            #{Name := Id} = Table,
            Id ! {answer, Data},
            proxy(Table);
        {remember, PID, Name} ->
            proxy(Table#{Name => PID})
    end.



====2017.07.20====
We want to define a “dynamic list” data structure, where each element of the list is an actor storing a value. Such value can be of
course read and set, and each get/set operation on the list can be performed in parallel.
1) Define create_dlist, which takes a number n and returns a dynamic list of length n. You can assume that each element store the
value 0 at start.
2) Define the function dlist_to_list, which takes a dynamic list and returns a list of the contained values.
2) Define a map for dynamic list. Of course this operation has side effects, since it changes the content of the list.


cell(Value) ->
 receive
  {set, V} ->
   cell(V);
  {get, Pid} ->
   Pid ! Value,
   cell(Value)
 end.

delement_get(Element) ->
 Element ! {get, self()},
  receive
   V -> V
  end.

delement_set(Element, New) ->
 Element ! {set, New}.

create_dlist(0) -> [];
create_dlist(Size) -> [spawn(?MODULE, cell, [0]) | create_dlist(Size-1)].

dlist_map([], _) -> ok;
dlist_map([X|Xs], Fun) ->
 delement_set(X, Fun(delement_get(X))),
 dlist_map(Xs, Fun).


dlist_to_list([]) -> ok;
dlist_to_list([X|Xs]) ->
	X ! {get, self()}
	receive ->
		{V} -> V
		dlist_to_list(Xs)
	end.
	

====2021.02.08====
Consider the apply operation (i.e.<*>) in Haskell's Applicative class.
Define a parallel <*> for Erlang's lists

concatMap(F:FS, LS) ->
	if 
	   F == [] -> ok;
	 
	else
	   res = maps:F(LS),
	   res = [res|concat(FS, LS)],
	   res;


mapList(F, L:LS) ->
	if
	 L == [] -> ok;
	else
	 res = [F(L)|mapList(F, LS)],
	 res;
