-module(stamp_server).

-behaviour(gen_server).

%% API
-export([start_link/0, post/3, get/2, get_tags/1, normalize/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(MAX_TAGS, 100).
-define(MAX_MSGS, 100).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, server}, ?MODULE, [], []).

post(Tag, Msg, ID) ->
  gen_server:call(server, {post, Tag, Msg, ID}).
  
get(Tag, N) ->
  gen_server:call(server, {get, Tag, N}).

get_tags(N) ->
  gen_server:call(server, {gettags, N}).

normalize(tag, Tag) -> 
  normalize(string:to_lower(Tag), 20, "[^a-z]");
normalize(msg, Msg) ->
  string:strip(normalize(Msg, 140, "[<>\t\r\n]")).

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
handle_call({post, Tag, Msg, ID}, _From, {Dict, Tags}) ->
  M = preprocess(normalize(msg, Msg), ID),
  T = normalize(tag, Tag),
  case M == [] orelse T == [] of
    true -> {reply, nook, {Dict, Tags}};
    false -> {reply, ok, {dict:update(T, fun(Old) -> lists:sublist([M|Old], ?MAX_MSGS) end, [M], Dict),
                          lists:sublist([T|lists:delete(T, Tags)], ?MAX_TAGS)}}
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
normalize(Str, N, Pat) ->
  lists:sublist(re:replace(Str, Pat, "", [global, {return, list}]), N).

serverfun_date(_) ->
  io_lib:format("<span>~w-~2..0w-~2..0w</span>", tuple_to_list(date())).

serverfun_dice_int(0, _) -> 0;
serverfun_dice_int(Dices, Sides) ->
  random:uniform(Sides) + serverfun_dice_int(Dices-1, Sides).

serverfun_dice([_, Dices, Sides]) ->
  {D, _} = string:to_integer(Dices),
  {S, _} = string:to_integer(Sides),
  if S > 20 -> "<span class='error'>dice overflow</span>";
     S =< 20 andalso D >= 0 -> io_lib:format("<span>~sd~s=~B</span>", [Dices, Sides, serverfun_dice_int(D, S)])
  end.

serverfun_fortune(_) ->
  os:cmd("fortune").

serverfun_name([{{_, _, A1, A2}, A3}]) ->
  Names = ["Alpha", "Beta", "Gamma", "Delta", "Epsilon", "Zeta", "Eta", "Theta", "Iota", "Kappa", "Lambda",
           "Mu", "Nu", "Xi", "Omicron", "Pi", "Rho", "Sigma", "Tau", "Upsilon", "Phi", "Chi", "Psi", "Omega"],
  random:seed(A1, A2, A3),
  io_lib:format("<span>~s</span>", [lists:nth(random:uniform(length(Names)), Names)]).

serverfun_time(_) ->
  io_lib:format("<span>~2..0w:~2..0w:~2..0w</span>", tuple_to_list(time())).

get_substrs_aux(String, [{Start, Length} | T], Substrs) ->
  get_substrs_aux(String, T, [string:substr(String, Start+1, Length) | Substrs]);
get_substrs_aux(_, [], Substrs) -> Substrs.

get_substrs(String, L) ->
  get_substrs_aux(String, L, []).

get_replacement(SubMsg, [{Pat, Fun} | T], ID) ->
  case re:run(SubMsg, string:concat("^", Pat)) of
    nomatch -> get_replacement(SubMsg, T, ID);
    {match, [TotalMatch | SubMatches]} -> {get_substrs(SubMsg, [TotalMatch]),
                                           Fun([ID | lists:reverse(get_substrs(SubMsg, SubMatches))])}
  end;
get_replacement(_, [], _) -> false.

replace_matches(Msg, [[{Start, _Length}] | T], ID) ->
  Subs = [{"date", fun serverfun_date/1},
          {"(\\d)dice(\\d\\d?)", fun serverfun_dice/1},
          {"name", fun serverfun_name/1},
          {"time", fun serverfun_time/1}],
  case get_replacement(string:substr(Msg, Start+2), Subs, ID) of
    false -> replace_matches(Msg, T, ID);
    {Match, Replacement} -> replace_matches(
                              re:replace(Msg, string:concat("\\$", Match), Replacement, [{offset, Start}, {return, list}]),
                              T, ID)
  end;
replace_matches(Msg, [], _) -> Msg.

replace_full(Msg, _ID) ->
  Subs = [{"€fortune", fun serverfun_fortune/1}],
  [Key | Params] = string:tokens(Msg, " "),
  case lists:keyfind(Key, 1, Subs) of
    false -> Msg;
    {Key, Fun} -> Fun(Params)
  end.

preprocess(Msg, ID) ->
  NewMsg = replace_full(Msg, ID),
  case re:run(NewMsg, "\\$", [global]) of
    nomatch -> NewMsg;
    {match, Matches} -> replace_matches(NewMsg, lists:reverse(Matches), ID)
  end.

