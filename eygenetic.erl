

-module(eygenetic).
-behavior(gen_server).

-export([start/0,stop/0,terminate/2]).
-export([init/1, handle_call/3, handle_cast/2,handle_info/2]).

-export([read_dic/0, shuffle/2, permute/1, join/1, attempt/3, attempt/1, swap/1, factorial/1]).
-export([attempt_init/1, attempt_init_cluster/1, attempt_loop/1]).

%%% gen_server callbacks
init(_Arg) ->
    % process_flag(trap_exit, true),
    % io:format("Starting ~n"),
    % init_mnesia(),
    crypto:start(),
    {ok, []}.
    
start() ->
    gen_server:start_link({global, eygenetic}, eygenetic, [], []).

stop() ->
    gen_server:cast({global, eygenetic}, stop).
    
terminate(Reason, State) ->
   io:format("Terminating~n").
   
handle_call({calc, N}, From, State) ->
   spawn(eygenetic, attempt, [1000]),
   {reply, ok, State};
   
handle_call({all,N}, From, State) ->
    lists:foldl(fun(Elem, Acc) -> 
        spawn(Elem, fun() -> eygenetic:attempt(N) end )
        end, {}, nodes()),
    {reply, ok, State};
   
handle_call({min, Distance, List}, {FromPid,_}=From, State) -> 
    io:format("Minimized: ~p ~p ~n", [Distance, List]),
    FromPid ! {again},
    {reply, min, State};
    
handle_call({pid, Pid}, From, State) ->
    io:format("Registered a PID ~p, ~n", [Pid]),
    Pid ! {again},
    io:format("Started Pid! ~p ~n", [Pid]),
    {reply, pid, State};

handle_call({set, Key, Value}, From, State) ->
   % Rec = do_set(Key, Value),
   Rec = "w00t1",
   {reply, Rec, State}.

handle_cast(stop, State) ->
   io:format("Stopping~n"),
   {stop, normal, State}.

handle_info(Info, State) ->
   {noreply, State}.

factorial(0) -> 1;
factorial(N) when N > 0 -> N * factorial(N-1).
    
read_dic() ->
  {ok, Content} = file:read_file("dictionary1"),
  WordList = binary_to_list(Content),
  Tokens = string:tokens(WordList,"\n"),
  {List1,_List2} = lists:split(12,shuffle(Tokens, [])),
  List1.
  
attempt(N) -> {It,Distance,Vector} = eygenetic:attempt(eygenetic:read_dic(),"1249C4B7F578204F10798C0269F8488280FB9981", N),
    gen_server:call({global,eygenetic}, {min, Distance, Vector}),
    {It,Distance,Vector}.

attempt_init_cluster(N) ->
    lists:foldl(fun(Elem,Acc) -> 
        Pid = spawn(Elem, fun() -> eygenetic:attempt_init(N) end ),
        Acc ++ [Pid]
        end, [], nodes()),
    
    %Don't forget the local node :-)
    eygenetic:attempt_init(N).

attempt_init(N) ->
    lists:foldl(fun(Elem, Acc) -> 
%        Pid = spawn(eygenetic, attempt_loop, [N]),
        Pid = spawn(fun() -> attempt_loop(N) end),
        gen_server:call({global,eygenetic}, {pid, Pid}),
        Acc ++ [Pid]
        end, [], lists:seq(1,8)).

attempt_loop(N) ->
    receive
        {again} ->
            attempt(N),
            attempt_loop(N);
        {stop} ->
            attempt_loop(N)
    end.

% attempt(N, Pid) -> {It,Distance,Vector} = eygenetic:attempt(eygenetic:read_dic(),"A7D10B0CA7D0DF3C6FD57645053D549EB7C0E0A3", N),
%     Pid ! {min, Distance, Vector},
%     {It,Distance,Vector}.
  
attempt(List, Target, N) ->
    lists:foldl(fun(Elem,{InheritedList,MinDistance,MinVector} = Acc) -> 
        NewList = iteration(InheritedList),
        NewSha = sha1:hexstring(join(NewList)),
        Distance = hamming:hamming(NewSha,Target),
        case Distance < MinDistance of
            true ->
                % io:format("Iteration: ~p Minimum: ~p ~n", [Elem, Distance]),
                {NewList, Distance, NewList};
            _ ->
                {NewList, MinDistance, MinVector}
                % Acc
            end
        end, {List, infinity, []}, lists:seq(1,N)). 
  
iteration(List) ->
    % io:format("~p ~n", [List]),
    % case random:uniform(3) of
    case crypto:rand_uniform(1,3) of
        1 ->
            %io:format("Shuffle ~n", []),
            shuffle(List,[]);
        2 ->
            %io:format("Permute ~n", []),
            permute(List);
        3 ->
            %io:format("swap ~n", []),
            swap(List)
    end.

join([]) -> [];
join([H|T]) ->
    case T == [] of
        true ->
            H ++ join(T);
        _ ->
            H ++ [" "] ++ join(T)
    end.
        
shuffle([],Acc) ->
    Acc;
shuffle(List, Acc) -> 
    % {Leading, [H | T]} = lists:split(random:uniform(length(List)) - 1, List),
    {Leading, [H | T]} = lists:split(crypto:rand_uniform(0, length(List)), List),
    shuffle(Leading ++ T, [H | Acc]).


permute(List) ->
    % Element = random:uniform(length(List)),
    Element = crypto:rand_uniform(0, length(List)),
    Term = lists:nth( Element + 1, List),
    % ElementIndex = random:uniform(length(Term)),
    ElementIndex = crypto:rand_uniform(0, length(Term)),
    SpecificLetter = lists:nth( ElementIndex + 1, Term ),
    SpecificLetterUpper = string:to_upper(SpecificLetter),
    case SpecificLetter == SpecificLetterUpper of
        true ->
            PermutedLetter = string:to_lower(SpecificLetter);
        false ->
            PermutedLetter = SpecificLetterUpper
    end,
    {LeadingLetters,[_Hl|Tl]} = lists:split(ElementIndex, Term),
    
    {Leading,[_H|T]} = lists:split(Element, List),
    Leading ++ [LeadingLetters ++ [PermutedLetter] ++ Tl] ++ T.
    
swap(List) ->
    A = read_dic(),
    % Element = random:uniform(length(List)),
    Element = crypto:rand_uniform(0, length(List)),
    {Leading,[_H|T]} = lists:split(Element, List),
    Leading ++ [lists:nth(1, A)] ++ T.
