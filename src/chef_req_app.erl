%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Kevin Smith <kevin@opscode.com>
%% @author Christopher Maier <cm@opscode.com>
%% @copyright 2011 Opscode, Inc.
%% @end

-module(chef_req_app).

-behaviour(application).

-include_lib("eunit/include/eunit.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%% Application callbacks
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    %ok = load_ibrowse_config(),
    %chef_req_sup:start_link().
    ok.

stop(_State) ->
    ok.

load_ibrowse_config() ->
    ConfigFile = filename:absname(filename:join(["etc", "ibrowse", "ibrowse.config"])),
    error_logger:info_msg("Loading ibrowse configuration from ~s~n", [ConfigFile]),
    ok = ibrowse:rescan_config(ConfigFile),
    ok.

