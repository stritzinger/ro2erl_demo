-module(ro2erl_demo).

-behaviour(gen_server).

%=== INCLUDES ====================================================================

-include_lib("kernel/include/logger.hrl").

-include_lib("rosie_dds/include/dds_types.hrl").
-include_lib("rosie_dds/include/rtps_structure.hrl").

-include_lib("sensor_msgs/src/_rosie/sensor_msgs_temperature_msg.hrl").
-include_lib("sensor_msgs/src/_rosie/sensor_msgs_range_msg.hrl").


%=== EXPORTS ===================================================================

% API functions
-export([start_link/0]).

% Bridge callback functions
-export([msg_processor/1]).
-export([dispatch_callback/1]).

% Behaviour gen_server callback functions
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).


%=== TYPES =====================================================================

-record(state, {
    ros_node,
    % PUBLISHERS
    % SENSORS
    acc_pub,
    sonar_pub,
    temp_pub
}).


%=== MACROS ====================================================================

-define(PUB_PERIOD, 50).


%=== API FUNCTIONS =============================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, #state{}, []).


%=== BRIDGE CALLBACK FUNCTIONS ==================================================

msg_processor({Msg, #{dender := Guid}}) ->
    #dds_user_topic{name = TopicName} = dds_data_w:get_topic({data_w_of, Guid}),
    {topic, list_to_binary(TopicName), false, size(Msg), Msg}.

dispatch_callback(Msg) ->
    ?LOG_DEBUG("Received message: ~p", [Msg]),
    P = rtps_participant:get_info(participant),
    ProcID = {receiver_of, P#participant.guid#guId.prefix},
    rtps_receiver:receive_packet(ProcID, Msg).


%=== BEHAVIOUR gen_server CALLBACK FUNCTIONS ===================================

init(_) ->
    Node = ros_context:create_node(atom_to_list(?MODULE)),

    TempPub = ros_node:create_publisher(Node, sensor_msgs_temperature_msg, "temp"),
    SonarPub = ros_node:create_publisher(Node, sensor_msgs_range_msg, "sonar"),

    self() ! update_loop,

    {ok, #state{ ros_node = Node,
        sonar_pub = SonarPub,
        temp_pub = TempPub}}.

handle_call( _, _, S) ->
    {reply, ok, S}.

handle_cast( _, S) ->
    {noreply, S}.

handle_info( update_loop, #state{
    sonar_pub = SonarPub,
    temp_pub = TempPub}=S
) ->
    Range = ro2erl_demo_sensors:range(),
    % [Ax,Ay,Az] = ro2erl_demo_sensors:acc(),
    % [GRx,GRy,GRz] = ro2erl_demo_sensors:gyro(),
    [Temp] = ro2erl_demo_sensors:temp(),
    % io:format("range is ~p\n",[Range]),
    % io:format("acc is ~p ~p ~p\n",[Ax,Ay,Az]),
    % io:format("gyro is ~p ~p ~p\n",[GRx,GRy,GRz] ),
    % io:format("temp is ~p\n",[Temp]),
    RangeMsg = #sensor_msgs_range{
        header = #std_msgs_header{
            stamp = #builtin_interfaces_time{},
            frame_id = "robot_head"
        },
        radiation_type=0,
        field_of_view = 0.40,
        min_range = 6.0,
        max_range = 255.0,
        range = Range
    },
    case Range of
        undefined -> nothing;
        _ -> ros_publisher:publish(SonarPub, RangeMsg)
    end,

    TempMsg = #sensor_msgs_temperature{
        header = #std_msgs_header{
            stamp = #builtin_interfaces_time{},
            frame_id = "robot_frame"
        },
        temperature = Temp
    },
    ros_publisher:publish(TempPub, TempMsg),

    erlang:send_after(?PUB_PERIOD, self(), update_loop),
    {noreply, S}.

code_change(_OldVsn, State, _Extra) ->
    grisp_led:off(1),
    grisp_led:off(2),
    LEDs = [1, 2],
    [grisp_led:flash(L, red, 500) || L <- LEDs],
    % robo_grisp_wheels:rotate(left),
    timer:sleep(1000),
    % robo_grisp_wheels:rotate(right),
    timer:sleep(1000),
    % robo_grisp_wheels:stop(),
    grisp_led:off(2),
    Random = fun() ->
        {rand:uniform(2) - 1, rand:uniform(2) -1, rand:uniform(2) - 1}
    end,
    grisp_led:pattern(1, [{100, Random}]),
    {ok, State}.
