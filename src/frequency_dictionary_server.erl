-module(frequency_dictionary_server).

-behaviour(gen_server).

%% API
-export([
  start_link/1, add_text/2, frequency_for/2,
  unique_word_count/1, unique_words/1
]).


-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
   terminate/2, code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================


start_link(Language) ->
  gen_server:start_link({local, Language}, ?MODULE, [], []).

add_text(Language, Text) when is_list(Text) ->
  add_text(Language, list_to_binary(Text));
add_text(Language, Text) when is_binary(Text) ->
  gen_server:cast(Language, {text, Text}).

frequency_for(Language, Word) when  is_list(Word) ->
  frequency_for(Language, list_to_binary(Word));
frequency_for(Language, Word) when is_binary(Word) ->
  gen_server:call(Language, {frequency_for, Word}).

unique_word_count(Language) ->
  gen_server:call(Language, unique_word_count).


unique_words(Language) ->
  gen_server:call(Language, unique_words).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, dict:new()}.


% exception exit      in function  gen_server:call/2 (gen_server.erl, line 180)

handle_call({frequency_for, Word}, _From, Dictionary) ->
  Frequency = case dict:find(Word, Dictionary) of
     {ok, Value} -> Value;
     error       -> 0
  end,
  {reply, Frequency, Dictionary};


handle_call(unique_word_count, _From, Dictionary) ->
  {reply, dict:size(Dictionary), Dictionary};


handle_call(unique_words, _From, Dictionary) ->
  {reply, dict:fetch_keys(Dictionary), Dictionary}.


handle_cast({text, Text}, State) ->
    Words = binary_to_words(Text),

    NewDict = lists:foldl(
      fun (Word, Dictionary) ->
        dict:update(
          Word,
          fun(OldVal) -> OldVal + 1  end,
          1,
          Dictionary
        )
      end,
      State,
      Words
    ),

    {noreply, NewDict}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


% You gotta be kidding me:
% http://www.erlang.org/doc/apps/stdlib/unicode_usage.html :
% There is at the time of writing no Unicode to_upper/to_lower functionality in Erlang/OTP, but there are publicly available libraries that address these issues.

% Elixir can do it for utf8 binaries: http://elixir-lang.org/docs/master/String.html#upcase/1   !!!

binary_to_words(Text) ->
  % re:replace(<< " éabcd" /utf8 >> ," +"," ",[{return,binary}]).
  CleanedUp = re:replace(Text, " +", " ",[{return,binary}, unicode]),

  % re:split(<<"Erlang ff ça va привет ée"/utf8>>," +",[{return,list}, unicode]).
  re:split(CleanedUp, " +",[{return, binary}, unicode]).


