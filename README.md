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

```bash
rebar3 as local shell --sname demo@localhost --setcookie targetx
```

### Deploying to a GRiSP 2 Board for Development

```bash
rebar3 as dev deploy
```

### Deploying to a GRiSP 2 Board for Production

```bash
rebar3 as prod deploy
```


## License

Copyright © 2023 Stritzinger GmbH

Licensed under the Apache License, Version 2.0. See [LICENSE](LICENSE) file for details.
