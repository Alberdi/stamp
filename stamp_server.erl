-module(stamp_server).

-behaviour(gen_server).

%% API
-export([start_link/0, post/2, get/2, get_tags/1, normalize/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, server}, ?MODULE, [], []).

post(Tag, Msg) ->
  gen_server:call(server, {post, Tag, Msg}).
  
get(Tag, N) ->
  gen_server:call(server, {get, Tag, N}).

get_tags(N) ->
  gen_server:call(server, {gettags, N}).

normalize(tag, Tag) -> 
  normalize(string:to_lower(Tag), 20, "[^a-z]");
normalize(msg, Msg) ->
  normalize(string:strip(Msg), 140, "[<>]").

normalize(Str, N, Pat) ->
  lists:sublist(re:replace(Str, Pat, "", [global, {return, list}]), N).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
  {ok, {dict:new(), []}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({post, Tag, Msg}, _From, {Dict, Tags}) ->
  M = normalize(msg, Msg),
  T = normalize(tag, Tag),
  case M == [] orelse T == [] of
    true -> {reply, nook, {Dict, Tags}};
    false -> {reply, ok, {dict:update(T, fun(Old) -> [M|Old] end, [M], Dict), [T|lists:delete(T, Tags)]}}
  end;

handle_call({get, Tag, N}, _From, {Dict, Tags}) ->
  case dict:find(normalize(tag, Tag), Dict) of
    {ok, L} -> {reply, lists:sublist(L, N), {Dict, Tags}};
    error -> {reply, [], {Dict, Tags}}
  end;

handle_call({gettags, N}, _From, {Dict, Tags}) ->
  {reply, lists:sublist(Tags, N), {Dict, Tags}}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

