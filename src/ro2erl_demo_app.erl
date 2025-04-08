-module(ro2erl_demo_app).

-behavior(application).


%=== EXPORTS ===================================================================

% Behaviour application callback functions
-export([start/2]).
-export([stop/1]).


%=== BEHAVIOUR application CALLBACK FUNCTIONS ===================================

start(_Type, _Args) ->
    ro2erl_demo_sup:start_link().

stop(_State) ->
    ok.
