-module(totientrange).
-export([hcf/2,
         relprime/2,
         sumTotient/3,
        eularWorker/1,
        stop_workers/1,
        start_workers/2,
        assign_work/3
        ]).


%% hcf x 0 = x
%% hcf x y = hcf y (rem x y)
hcf(X,0) -> X;
hcf(X,Y) -> hcf(Y,X rem Y).

%% relprime x y = hcf x y == 1
relprime(X,Y) -> hcf(X,Y) == 1.


eularWorker(masterId) ->
  receive
    {work, N} ->
      RelprimeN = fun(Y) -> relprime(N,Y) end,
      masterId ! {done, self/0(), length/1(lists:filter/2(RelprimeN,(lists:seq/2(1,N))))};
    stop ->
      done
  end.


%%euler n = length (filter (relprime n) (mkList n))

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
    [spawn/3(totientrange, eularWorker, [MasterId]) | start_workers(Workers - 1)].


assign_work([], _, _) -> ok;
assign_work([Work | RemWork], Workers, Counter) ->
  Index = Counter rem length/1(Workers),
  lists:nth/2(Index, Workers) ! {work, self/1(), Work},
  assign_work(RemWork, Workers, Counter+1).


collect_results

%%sumTotient lower upper = sum (map euler [lower, lower+1 .. upper])
sumTotient(Lower,Upper, MaxWorkers) ->
    {_, S, US} = os:timestamp(),
    Workers = start_workers(MaxWorkers, self/0()),
    assign_work(lists:seq/2(Lower, Upper), Workers, 0),
    stop_workers(Workers),


    io:format("Sum of totients: ~p~n", [Res]),
    printElapsed(S,US).