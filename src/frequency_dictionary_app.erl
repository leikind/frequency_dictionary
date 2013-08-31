-module(frequency_dictionary_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

% [en, fr, nl]
start(_StartType, Languages) ->
    % io:format("~p ~n", [Languages]),
    frequency_dictionary_sup:start_link(Languages).

stop(_State) ->
    ok.
