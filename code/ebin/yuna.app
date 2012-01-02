{application, yuna, [

    {description, "YUNA - Distributed Consensus implementation"},
    {vsn, "0.0.1"},
    {modules, [chan_filters, tweaked_chan, bcast, peers, keeper_proto,
               keeper_inject, yuna, gen_peer, gen_keeper, peer_chan,
               peer_ctrl, main, log_serv, randel, utils, services]}
    {registered, []},
    {application, [kernel, stdlib]},
    {mod, {yuna, []}},

    {env, [
        % Default configuration, can be overriden by configuration file.
        {faulty_prob, 0.01},        % 1% of nodes are faulty
        {faulty_fail_prob, 0.05},   % 5% crash probab. for faulty node

        %{faulty_prob, 0.1},
        %{faulty_fail_prob, 0.15},

        {deliver_mindel, 500},      % Minimum deliver delay
        {deliver_maxdel, 1500},     % Maximum deliver delay
        {deliver_dist, {random, uniform, []}},  % Delay distribution

        % Pingpong testing application:
        %{keeper, pingpong_keeper},  % Nodes keeper
        %{keeper_args, 3}

        {log_args, { standard_error,    % For visual logging
                     "statfile"         % For stats retrival
                   }
        },
        {keeper, gfd_keeper},
        {keeper_args, { {10, 20, 4},     % TFail, TCleanup, TGossip
                        500,            % NPeers
                        0.01,           % StatPeers [ratio]
                        500,            % TBeacon, [ms]
                        120             % BeaconWait, [TBeacon]
                        % 120 beacons = 1 min
                      }
        }
    ]}

]}.
