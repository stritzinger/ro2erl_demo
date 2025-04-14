-module(ro2erl_demo).

-behaviour(gen_server).

%=== INCLUDES ====================================================================

-include_lib("kernel/include/logger.hrl").

-include_lib("rosie_dds/include/dds_types.hrl").
-include_lib("rosie_dds/include/rtps_constants.hrl").
-include_lib("rosie_dds/include/rtps_structure.hrl").

-include("_rosie/ro2erl_demo_multi_range_msg.hrl").
-include("_rosie/ro2erl_demo_multi_temp_msg.hrl").
-include_lib("sensor_msgs/src/_rosie/sensor_msgs_temperature_msg.hrl").
-include_lib("sensor_msgs/src/_rosie/sensor_msgs_range_msg.hrl").


-include("_rosie/ro2erl_demo_multi_accel_msg.hrl").
-include_lib("geometry_msgs/src/_rosie/geometry_msgs_accel_msg.hrl").

%=== EXPORTS ===================================================================

% API functions
-export([start_link/0]).

% Bridge callback functions
-export([msg_processor/1]).
-export([dispatch_callback/1]).

% Behaviour gen_server callback functions
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

% RCLERL callback
-export([on_topic_msg/2]).

%=== TYPES =====================================================================

-record(state, {
    ros_node,
    % PUBLISHERS
    % SENSORS
    accel_pub,
    sonar_pub,
    temp_pub,
    sonar_sub,
    temp_sub,
    accel_sub,
    % Cache
    temp_measures = [],
    range_measures = [],
    accel_measures = []
}).


%=== MACROS ====================================================================

-define(POLLING_PERIOD, 10).
-define(PUB_PERIOD, 50).

%=== API FUNCTIONS =============================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, #state{}, []).

on_topic_msg(Pid, Msg) ->
    gen_server:cast(Pid, {on_topic_msg, Msg}).

%=== BRIDGE CALLBACK FUNCTIONS ==================================================

msg_processor({Msg, #{sender := #guId{entityId = EntityID} = Guid}}) ->
    #entityId{key = _, kind = Kind} = EntityID,
    Topic = case lists:member(Kind, dds_builtin_entity_kinds()) of
        true -> "builtin";
        false ->
            Handler = find_entity_handler_module(Kind),
            #dds_user_topic{name = Name} = dds_data_w:get_topic({Handler, Guid}),
            Name
    end,
    SafeTopics = ["builtin"],
    Filterable = not lists:member(Topic, SafeTopics),
    {topic, list_to_binary(Topic), Filterable, size(Msg), Msg}.

dispatch_callback(Msg) ->
    ?LOG_DEBUG("Received message: ~p", [Msg]),
    P = rtps_participant:get_info(participant),
    ProcID = {receiver_of, P#participant.guid#guId.prefix},
    rtps_receiver:receive_packet(ProcID, Msg).


%=== BEHAVIOUR gen_server CALLBACK FUNCTIONS ===================================

init(_) ->
    Node = ros_context:create_node(atom_to_list(?MODULE)),
    QoS = #qos_profile{durability = ?VOLATILE_DURABILITY_QOS,
                       reliability = ?RELIABLE_RELIABILITY_QOS,
                       history = {?KEEP_LAST_HISTORY_QOS, 10}},
    TempPub = ros_node:create_publisher(Node, ro2erl_demo_multi_temp_msg, "temp", QoS),
    SonarPub = ros_node:create_publisher(Node, ro2erl_demo_multi_range_msg, "range", QoS),
    AccelPub = ros_node:create_publisher(Node, ro2erl_demo_multi_accel_msg, "acceleration", QoS),
    TempSub = ros_node:create_subscription(Node, ro2erl_demo_multi_temp_msg, "temp", {?MODULE, self()}),
    SonarSub = ros_node:create_subscription(Node, ro2erl_demo_multi_range_msg, "range", {?MODULE, self()}),
    AccelSub = ros_node:create_subscription(Node, ro2erl_demo_multi_accel_msg, "acceleration", {?MODULE, self()}),

    self() ! pub_loop,

    self() ! polling_loop,

    {ok, #state{ ros_node = Node,
        sonar_pub = SonarPub,
        temp_pub = TempPub,
        accel_pub = AccelPub,
        sonar_sub = SonarSub,
        temp_sub = TempSub,
        accel_sub = AccelSub}}.

handle_call( _, _, S) ->
    {reply, ok, S}.

handle_cast({on_topic_msg, #ro2erl_demo_multi_temp{temperatures = Temperatures}}, S) ->
    io:format("ROSIE: [ro2erl_demo]: I received ~p temp measurements~n", [length(Temperatures)]),
    {noreply, S};
handle_cast({on_topic_msg, #ro2erl_demo_multi_range{ranges = Ranges}}, S) ->
    io:format("ROSIE: [ro2erl_demo]: I receivd ~p range measurements~n", [length(Ranges)]),
    {noreply, S};
handle_cast({on_topic_msg, #ro2erl_demo_multi_accel{accelerations = Accelerations}}, S) ->
    io:format("ROSIE: [ro2erl_demo]: I receivd ~p accel measurements~n", [length(Accelerations)]),
    {noreply, S}.

handle_info(polling_loop, #state{temp_measures = TempMeasures,
                                 range_measures = RangeMeasures,
                                 accel_measures = AccelMeasures} = S) ->

    erlang:send_after(?POLLING_PERIOD, self(), polling_loop),
    AccelMsg = get_accel(),
    RangeMsg = get_range(),
    NewAccel = [AccelMsg|AccelMeasures],
    NewRanges = case RangeMsg of
        undefined -> RangeMeasures;
        _ -> [RangeMsg | RangeMeasures]
    end,
    TempMsg = get_temp(),
    NewTemps = [TempMsg | TempMeasures],
    {noreply, S#state{temp_measures = NewTemps,
                      range_measures = NewRanges,
                      accel_measures = NewAccel}};
handle_info(pub_loop, #state{sonar_pub = SonarPub,
                             temp_pub = TempPub,
                             accel_pub = AccelPub,
                             temp_measures = TempMeasures,
                             range_measures = RangeMeasures,
                             accel_measures = AccelMeasures} = S) ->
    TempMsg = #ro2erl_demo_multi_temp{temperatures = lists:reverse(TempMeasures)},
    RangeMsg = #ro2erl_demo_multi_range{ranges = lists:reverse(RangeMeasures)},
    AccelMsg = #ro2erl_demo_multi_accel{accelerations = lists:reverse(AccelMeasures)},
    ros_publisher:publish(TempPub, TempMsg),
    ros_publisher:publish(SonarPub, RangeMsg),
    ros_publisher:publish(AccelPub, AccelMsg),
    erlang:send_after(?PUB_PERIOD, self(), pub_loop),
    {noreply, S#state{range_measures = [], temp_measures = [], accel_measures = []}}.

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

dds_builtin_entity_kinds() ->
    [
        ?EKIND_BUILTIN_unknown,
        ?EKIND_BUILTIN_Participant,
        ?EKIND_BUILTIN_Writer_WITH_Key,
        ?EKIND_BUILTIN_Writer_NO_Key,
        ?EKIND_BUILTIN_Reader_NO_Key,
        ?EKIND_BUILTIN_Reader_WITH_Key,
        ?EKIND_BUILTIN_Writer_Group,
        ?EKIND_BUILTIN_Reader_Group
    ].

find_entity_handler_module(?EKIND_USER_Writer_WITH_Key) ->  data_w_of;
find_entity_handler_module(?EKIND_USER_Writer_NO_Key) ->    data_w_of;
find_entity_handler_module(?EKIND_USER_Reader_NO_Key) ->    data_r_of;
find_entity_handler_module(?EKIND_USER_Reader_WITH_Key) ->  data_r_of;
find_entity_handler_module(?EKIND_USER_Writer_Group) ->     data_w_of;
find_entity_handler_module(?EKIND_USER_Reader_Group) ->     data_r_of.

get_temp() ->
    [Temp] = [90],%ro2erl_demo_sensors:temp(),
    #sensor_msgs_temperature{
        header = #std_msgs_header{
            stamp = #builtin_interfaces_time{},
            frame_id = "robot_frame"
        },
        temperature = Temp
    }.

get_range() ->
    Range = 80, %ro2erl_demo_sensors:range(),
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
        undefined -> undefined;
        _ -> RangeMsg
    end.

get_accel() ->
    [Ax,Ay,Az] = [0,0,0],%ro2erl_demo_sensors:acc(),
    [GRx,GRy,GRz] = [0,0,0],%ro2erl_demo_sensors:gyro(),
    Vector3acc = #geometry_msgs_vector3{
        x = Ax,
        y = Ay,
        z = Az
    },
    Vector3gyro = #geometry_msgs_vector3{
        x = GRx,
        y = GRy,
        z = GRz
    },
    #geometry_msgs_accel{
        linear = Vector3acc,
        angular = Vector3gyro
    }.
