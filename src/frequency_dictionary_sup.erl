-module(frequency_dictionary_sup).

-behaviour(supervisor).

%% API
-export([start_link/1, start_dictionary_dynamically/1, children/0]).

%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link(Languages) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Languages).

start_dictionary_dynamically(Lang) ->
  supervisor:start_child(?MODULE, child_spec_for_language(Lang)).

children() -> supervisor:which_children(?MODULE).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Languages) ->

  Children = lists:map(
    fun(Lang) -> % there must be a shorter way
      child_spec_for_language(Lang)
    end,
    Languages
  ),

  RestartStrategy = {one_for_one, 5, 10},

  {ok, {RestartStrategy, Children}}.

%% ===================================================================
%% Helpers
%% ===================================================================

child_spec_for_language(Lang) ->
      {
        Lang,                           % term that the supervisor uses to identify the specification internally
        {                               % how to start the process
          frequency_dictionary_server,
          start_link,
          [Lang]
        },

        permanent,                    % temporary: processes that should never be restarted
                                      % permanent a long-lived service that should always be restarted if it terminates for any reason
                                      % transient:  processes that should be restarted only if they terminate abnormally but not upon normal termination
        brutal_kill,                  % milliseconds for a soft shutdown
        worker,                       % this is a worker
        [frequency_dictionary_server] % modules that the process depends on
      }.
