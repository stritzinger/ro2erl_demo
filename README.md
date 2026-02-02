# RO2ERL Demo

A demonstration application for the RO2ERL Bridge of the Target-X system.


## Overview

This demonstration project showcases the integration between ROS2/DDS networks and Erlang-based distributed systems using the `ro2erl_bridge` and `ro2erl_hub` components. The demo provides practical examples of:

- Bridge configuration and initialization
- Message passing between ROS2 and Erlang
- Topic filtering and bandwidth management
- Custom message processing


## Dependencies

- Erlang/OTP 27 or later
- `grisp` - GRiSP 2 library
- `ro2erl_bridge` - Bridge component for connecting to ROS2/DDS networks
- `rosie_rclerl` - Erlang client for ROS2/DDS communications
- `grisp_connect` - grisp.io client
- `grisp_updater_grisp2` - Support for GRiSP 2 software updates


## Building

### Fetching

```bash
# Clone the repository
git clone https://github.com/stritzinger/ro2erl_demo.git
cd ro2erl_demo
```

### Running locally

> [!WARNING]
> !!! REMEMBER TO DELETE `_build` IF YOU PREVIOUSLY USED A PROFILE DIFFERENT FROM `local` !!!

Remember that you may need to setup your grisp.io server to use the fake
device CA, In this example it would be `local/peers.CA.pem` from ro2erl_hub.

Add your grisp.io test server CA into `priv/certificates/servers` with the
proper name, e.g. `www.seawater.local.pem`.

### Without TLS

```bash
ERL_FLAGS='-connect_all false' rebar3 as local shell --name demo --setcookie targetx
```

### With TLS

First generate the reuired certificates:

```bash
./local/setup.sh ../ro2erl_hub
```

Then you can start the demo shell with TLS:

```bash
ERL_FLAGS='-proto_dist inet_tls -ssl_dist_optfile local/ssl_dist_opts.rel -connect_all false' rebar3 as local shell --sname demo --setcookie targetx
1> net_adm:ping(list_to_atom("hub@" ++ lists:nth(2, string:split(atom_to_list(node()), "@")))).
pong
```

### Deploying to a GRiSP 2 Board for Development

> [!WARNING]
> !!! REMEMBER TO DELETE `_build` IF YOU PREVIOUSLY USED A PROFILE DIFFERENT FROM `dev` !!!

Remember to add all the hosts pointing to your development server in the hosts
file `grisp/grisp2/common/deploy/files/etc/hosts`.

```bash
rebar3 as dev deploy
```

### Deploying to a GRiSP 2 Board for Production

> [!WARNING]
> !!! REMEMBER TO DELETE `_build` IF YOU PREVIOUSLY USED A PROFILE DIFFERENT FROM `prod` !!!

```bash
rebar3 as prod deploy
```


## License

Copyright © 2025 Stritzinger GmbH

Licensed under the Apache License, Version 2.0. See [LICENSE](LICENSE) file for details.
