-module(ro2erl_demo_sup).

-moduledoc """
ro2erl_demo top level supervisor.
""".

-behavior(supervisor).


%=== EXPORTS ===================================================================

% API functions
-export([start_link/0]).

% Behaviour supervisor callback functions
-export([init/1]).


%=== API FUNCTIONS =============================================================

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%=== BEHAVIOUR supervisor CALLBACK FUNCTIONS ===================================

init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 0,
        period => 1
    },
    Sensors = #{
        id => ro2erl_demo_sensors,
        start => {ro2erl_demo_sensors, start_link, []}
    },
    Demo = #{
        id => ro2erl_demo,
        start => {ro2erl_demo, start_link, []}
    },
    ChildSpecs = [Sensors, Demo],
    {ok, {SupFlags, ChildSpecs}}.
