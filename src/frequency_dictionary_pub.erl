-module(frequency_dictionary_pub).


%% API
-export([
  add_text/2, frequency_for/2,
  unique_word_count/1, unique_words/1
]).


add_text(Language, Text) ->
  start_dictionary_if_needed(Language),
  frequency_dictionary_server:add_text(Language, Text).

frequency_for(Language, Word) ->
  start_dictionary_if_needed(Language),
  frequency_dictionary_server:frequency_for(Language, Word).

unique_word_count(Language) ->
  start_dictionary_if_needed(Language),
  frequency_dictionary_server:unique_word_count(Language).


unique_words(Language) ->
  start_dictionary_if_needed(Language),
  frequency_dictionary_server:unique_words(Language).

%  private

start_dictionary_if_needed(Language) ->
  case is_language_present(Language) of
    false -> frequency_dictionary_sup:start_dictionary_dynamically(Language);
    _     -> whatever
  end.

is_language_present(Language) ->
  lists:member(Language, list_of_languages()).


list_of_languages() ->
   [Language || {Language, _, _, _} <- frequency_dictionary_sup:children() ].


