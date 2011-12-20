{application, yuna, [

    {description, "YUNA - Distributed Consensus emulator"},
    {vsn, "0.0.1"},
    {modules, [yuna, main, peers, conf, log_serv, chan, chan_filters,
               randel, services, bcast]},
    {registered, []},
    {application, [kernel, stdlib]},
    {mod, {yuna, []}},

    {env, [
        % Default configuration, can be overriden by configuration file.
        %{faulty_prob, 0.01},        % 1% of nodes are faulty
        %{faulty_fail_prob, 0.05},   % 5% crash probab. for faulty node

        {faulty_prob, 0.1},
        {faulty_fail_prob, 0.15},

        {deliver_mindel, 500},      % Minimum deliver delay
        {deliver_maxdel, 1500},     % Maximum deliver delay
        {deliver_dist, {random, uniform, []}},  % Delay distribution

        % Pingpong testing application:
        %{keeper, pingpong_keeper},  % Nodes keeper
        %{keeper_args, 3}

        {log_args, { standard_error,    % For visual logging
                     "statfile.log"     % For stats retrival
                   }
        },
        {keeper, gfd_keeper},
        {keeper_args, { {3, 20, 5},     % TFail, TCleanup, TGossip
                        500,            % NPeers
                        500,            % TBeacon, [ms]
                        120             % BeaconWait, [TBeacon]
                        % 120 beacons = 1 min
                      }
        }
    ]}

]}.
