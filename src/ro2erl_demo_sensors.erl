-module(ro2erl_demo_sensors).

-behaviour(gen_server).


%=== EXPORTS ===================================================================

% API functions
-export([start_link/0]).
-export([range/0, acc/0, gyro/0, temp/0]).

% Behaviour gen_server callback functions
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

%=== TYPES =====================================================================

-record(state, {
}).


%=== API FUNCTIONS =============================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, #state{}, []).

range() ->
    gen_server:call(?MODULE, ?FUNCTION_NAME).

acc() ->
    gen_server:call(?MODULE, ?FUNCTION_NAME).

gyro() ->
    gen_server:call(?MODULE, ?FUNCTION_NAME).

temp() ->
    gen_server:call(?MODULE, ?FUNCTION_NAME).


%=== BEHAVIOUR gen_server CALLBACK FUNCTIONS ===================================

init(_) ->
    {ok, #state{}}.

handle_call( range, _, S) ->
    try pmod_maxsonar:get() of
        undefined -> {reply, undefined, S};
        Range -> {reply, float(Range), S}
    catch
        _:{noproc, _} ->
            {reply, undefined, S}
    end;
handle_call( acc, _, S) ->
    Acc = pmod_nav:read(acc, [out_x_xl, out_y_xl, out_z_xl], #{xl_unit => mg}),
    {reply, Acc, S};
handle_call( gyro, _, S) ->
    G = pmod_nav:read( acc, [out_x_g, out_y_g, out_z_g]),
    {reply, G, S};
handle_call( temp, _, S) ->
    T = pmod_nav:read( alt, [temp_out]),
    {reply, T, S}.

handle_cast( _, S) ->
    {noreply, S}.

handle_info( _, S) ->
    {noreply, S}.
