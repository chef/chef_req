%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Mark Anderson <mark@opscode.com>
%% @author Kevin Smith <kevin@opscode.com>
%% @copyright Copyright 2011 Opscode Inc.
%% @doc Helper module for calling various Chef REST endpoints
%% @end

-module(chef_rest_client).

-include("chef_rest_client.hrl").
-include_lib("ej/include/ej.hrl").

-export([generate_signed_headers/3,
         generate_signed_headers/4,
         make_chef_rest_client/3]).



-spec make_chef_rest_client(string(), string(), any()) -> #chef_rest_client{}.

make_chef_rest_client(BaseUrl, UserName, PrivateKey) ->
    #chef_rest_client{base_url = BaseUrl,
                      user_name = UserName,
                      private_key = PrivateKey,
                      request_source = user}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% NOTE: removed functions:
%% * make_webui_chef_rest_client/3 -- to avoid dep on chef_keyring module.
%% * request/2 -- redundant
%% * do_chef_get/2,5 -- redundant
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


generate_signed_headers(#chef_rest_client{base_url = BaseUrl,
                                          user_name = UserName,
                                          private_key = PrivateKey,
                                          request_source = RequestSource},
                        Path, Method, Body) ->
    Url = BaseUrl ++ Path,
    ExtraHeaders = case RequestSource of
                       web ->
                           [{"x_ops_request_source", "web"}];
                       user ->
                           []
                   end,
    Headers0 = generate_signed_headers(PrivateKey, UserName, Method, Path,
                                       Body),
    Headers = [{"Accept", "application/json"}|Headers0] ++ ExtraHeaders,
    {Url, Headers};
generate_signed_headers(PrivateKey, User, Method, Path) ->
    generate_signed_headers(PrivateKey, User, Method, Path, <<"">>).

generate_signed_headers(#chef_rest_client{}=Client, Path, Method) ->
    generate_signed_headers(Client, Path, Method, <<"">>).

generate_signed_headers(PrivateKey, User, Method, Path, Body) ->
    Time = httpd_util:rfc1123_date(),
    SignedHeaders = chef_authn:sign_request(PrivateKey, Body,
                                            list_to_binary(User),
                                            Method, Time, Path),
    % TODO: control the type of K and V *before* getting in here It
    % looks like ibrowse only requires that header names be atom or
    % string, but values can be iolist.  It might be worth
    % investigating whether ibrowse can be taught how to handle header
    % names that are binaries to avoid conversion.
    [{binary_to_list(K), binary_to_list(V)} || {K, V} <- SignedHeaders].
