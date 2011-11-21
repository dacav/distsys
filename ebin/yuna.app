{application, yuna,
    [   {description, "YUNA - Distributed Consensus emulator"},
        {vsn, "0.0.1"},
        {modules, [yuna, conf, log_serv, chan, chan_filters, randel, services]},
        {registered, []},
        {application, [kernel, stdlib]},
        {mod, {yuna, []}}
    ]
}.
