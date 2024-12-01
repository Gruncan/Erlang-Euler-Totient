-module(totientrange).
-export([hcf/2, relprime/2, sumTotient/3, eularWorker/0, euler/1, start_workers/1, assign_work/5, collect_results/3, workerName/1, workerChaos/3, supervisor/2, start/2]).


workerName(N) ->
  list_to_atom("worker" ++ integer_to_list(N)).


workerChaos(NVictims, NWorkers, WorkerIds) ->
  lists:map(
    fun( _ ) ->
      timer:sleep(500), %% Sleep for .5s
      %% Choose a random victim
      WorkerNum = rand:uniform(NWorkers-1),
      WorkerPid = lists:nth(WorkerNum, WorkerIds),
      io:format("workerChaos killing ~p~n", [WorkerPid]),
      if
      WorkerPid == undefined ->
        io:format("workerChaos already dead: ~p~n", [workerName(WorkerNum)]);
      true -> %% Kill Kill Kill
        exit(WorkerPid, chaos)
      end
    end,
  lists:seq( 1, NVictims ) ).




hcf(X,0) -> X;
hcf(X,Y) -> hcf(Y,X rem Y).

relprime(X,Y) -> hcf(X,Y) == 1.

euler(N) ->
    RelprimeN = fun(Y) -> relprime(N,Y) end,
    length(lists:filter(RelprimeN, (lists:seq(1,N)))).


eularWorker() ->
  receive
    {work, CollectorID, WorkList} ->
      CollectorID ! {done, self(), lists:sum(lists:map(fun euler/1, WorkList))}
  end.



%% Take completion timestamp, and print elapsed time
printElapsed(S,US) ->
    {_, S2, US2} = os:timestamp(),
    %% Adjust Seconds if completion Microsecs > start Microsecs
    if
        US2-US < 0 ->
            S3 = S2-1,
            US3 = US2+1000000;
        true ->
            S3 = S2,
            US3 = US2
    end,
    io:format("Time taken in Secs, MicroSecs ~p ~p~n",[S3-S,US3-US]).



start_workers(0) -> [];
start_workers(Workers) ->
    WorkerName = list_to_atom("worker" ++ integer_to_list(Workers)),
    Pid = spawn_link(totientrange, eularWorker, []),
    register(WorkerName, Pid),
    [WorkerName | start_workers(Workers - 1)].



supervisor([], _) -> stop;
supervisor(Workers, Collector) ->
  io:format("Supervising ~p workers ~n", [length(Workers)]),
  receive
    {'EXIT', ProcessName, normal} ->
      % Process is finished we can remove it from the list.
      ets:update_counter(work_assignment, worker_count, -1),
      NewWorkers = lists:delete(ProcessName, Workers),
      % io:format("Process killed correctly ~p, remaining: ~p~n", [ProcessName, NewWorkers]),
      supervisor(NewWorkers, Collector);
    {'EXIT', ProcessName, _} ->
      io:format("Process killed unexpectedly ~p~n", [ProcessName]),

      {_, {Index, Chunk}} = hd(ets:lookup(work_assignment, ProcessName)),
      ets:delete(work_assignment, ProcessName),

      io:format("Process ~p: [~p, ~p]~n", [ProcessName, Index, Chunk]),

      process_flag(trap_exit, true),
      Pid = spawn_link(totientrange, eularWorker, []),
      ets:insert(work_assignment, {Pid, {Index, Chunk}}),
      % ets:update_counter(work_assignment, worker_count, 1),
      
      {_, Work} = hd(ets:lookup(work_assignment, assignedWork)),

      NewWork = lists:sublist(Work, Index, Chunk),


      Pid ! {work, Collector, NewWork},

      % We register under the same name, so do not need to update the list.
      % register(ProcessName, Pid),
      NewWorkers = [Pid | Workers],
      % io:format("New workers ~p~n", [NewWorkers]),
      supervisor(NewWorkers, Collector);
    finished ->
      io:format("Supervision complete~n")
  end.
  

start(MasterId, MaxWorkers) ->
  process_flag(trap_exit, true),
  Workers = start_workers(MaxWorkers),

  WorkerIds = lists:map(fun(Name) -> whereis(Name) end, Workers),
  io:format("Workers ~p~n", [WorkerIds]),

  Collector = spawn(totientrange, collect_results, [MasterId, MaxWorkers, 0]),

  MasterId ! {ids, WorkerIds, Collector},

  supervisor(WorkerIds, Collector).



assign_work([], _, _, _, _) -> ok;
assign_work(Work, [Worker], CollectorID, _, Index) ->
  Chunk = length(Work),
  io:format("Assigned work ~p: [~p, ~p]~n", [Worker, Index, Chunk]),
  ets:insert(work_assignment, {Worker, {Index, Chunk}}),
  Worker ! {work, CollectorID, Work};

assign_work(Work, [Worker | Workers], CollectorID, Chunk, Index) ->
  {AsgWork, RemWork} = lists:split(Chunk, Work),

  io:format("Assigned work ~p: [~p, ~p]~n", [Worker, Index, Chunk]),
  ets:insert(work_assignment, {Worker, {Index, Chunk}}),

  Worker ! {work, CollectorID, AsgWork},
  assign_work(RemWork, Workers, CollectorID, Chunk, Index + Chunk).


collect_results(MasterId, 0, FinalResult) ->
  MasterId ! {done, FinalResult};

collect_results(MasterId, _, FinalResult) ->  
  receive
    {done, FromId, Result} ->
        io:format("Recieved from ~p: value ~p~n", [FromId, Result]),
        {_, MaxWorkers} = hd(ets:lookup(work_assignment, worker_count)),
        io:format("Workers remaining ~p~n", [MaxWorkers]),

        collect_results(MasterId, MaxWorkers, FinalResult + Result)
    end.

%%sumTotient lower upper = sum (map euler [lower, lower+1 .. upper])
sumTotient(Lower, Upper, MaxWorkers) ->
    {_, S, US} = os:timestamp(),

    work_assignment = ets:new(work_assignment, [named_table, public, set]),


    Work = lists:seq(Lower, Upper),

    ets:insert(work_assignment, {assignedWork, Work}),
    ets:insert(work_assignment, {worker_count, MaxWorkers}),
  
    Chunk = length(Work) div MaxWorkers,

    spawn(totientrange, start, [self(), MaxWorkers]),

    receive
      {ids, WorkerIds, Collector} ->
        spawn(totientrange, workerChaos, [2, MaxWorkers, WorkerIds]),
        assign_work(Work, WorkerIds, Collector, Chunk, 1)
    end,


    receive
      {done, Res} ->
        io:format("Sum of totients: ~p~n", [Res]),
        printElapsed(S,US)
    end,

    ets:delete(work_assignment).

    