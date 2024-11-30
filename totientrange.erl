-module(totientrange).
-export([hcf/2,
         relprime/2,
         sumTotient/3,
        eularWorker/1,
        euler/1,
        stop_workers/1,
        start_workers/2,
        assign_work/3,
        collect_results/2
        ]).


hcf(X,0) -> X;
hcf(X,Y) -> hcf(Y,X rem Y).

relprime(X,Y) -> hcf(X,Y) == 1.

euler(N) ->
    RelprimeN = fun(Y) -> relprime(N,Y) end,
    length(lists:filter(RelprimeN, (lists:seq(1,N)))).


eularWorker(MasterId) ->
  % io:format("MasterId: ~p ~n",[MasterId]),
  receive
    {work, WorkList} ->
      MasterId ! {done, self(), lists:sum(lists:map(fun euler/1, WorkList))};
    stop ->
      done
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


stop_workers([]) -> ok;
stop_workers([Worker | RemWorkers]) ->
  Worker ! stop,
  stop_workers(RemWorkers).


start_workers(0, _) -> [];
start_workers(Workers, MasterId) ->
    [spawn(totientrange, eularWorker, [MasterId]) | start_workers(Workers - 1, MasterId)].


assign_work([], _, _) -> ok;
assign_work(Work, [Worker], _) ->
  Worker ! {work, Work};

assign_work(Work, [Worker | Workers], Chunk ) ->
  {AsgWork, RemWork} = lists:split(Chunk, Work),

  Worker ! {work, AsgWork},
  assign_work(RemWork, Workers, Chunk).

collect_results([], FinalResult) ->
  FinalResult;
collect_results([Worker | Workers], FinalResult) ->
  receive
    {done, Worker, Result} ->
      collect_results(Workers, FinalResult + Result)
  end.


%%sumTotient lower upper = sum (map euler [lower, lower+1 .. upper])
sumTotient(Lower, Upper, MaxWorkers) ->
    {_, S, US} = os:timestamp(),
    Workers = start_workers(MaxWorkers, self()),
    Work = lists:seq(Lower, Upper),

    assign_work(Work, Workers, length(Work) div length(Workers)),
    stop_workers(Workers),

    Res = collect_results(Workers, 0),

    io:format("Sum of totients: ~p~n", [Res]),
    printElapsed(S,US).