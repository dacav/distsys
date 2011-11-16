% ------------------------------------------------------------------------
% Random value specification:
% ------------------------------------------------------------------------

-define(DEFAULT_MIN, 0).
-define(DEFAULT_MAX, 1000).
-define(DEFAULT_DIST, fun random:uniform/0).

-record(randspec, {min=?DEFAULT_MIN,
                   max=?DEFAULT_MAX,
                   dist=?DEFAULT_DIST}).
