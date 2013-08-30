-module(frequency_dictionary_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

% [en, fr, nl]
start(_StartType, Languages) ->
    frequency_dictionary:start_link(Languages).

stop(_State) ->
    ok.
