%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Mark Anderson <mark@opscode.com>
%% @author Kevin Smith <kevin@opscode.com>
%% @copyright Copyright 2011 Opscode Inc.
%% @doc Helper module for calling various Chef REST endpoints
%% @end

-module(chef_rest_client).

-include("chef_rest_client.hrl").
-include("chef_authn.hrl").
-include_lib("public_key/include/public_key.hrl").
%-include_lib("ej/include/ej.hrl").

-export([generate_signed_headers/3,
         generate_signed_headers/4,
         make_chef_rest_client/3]).



-spec make_chef_rest_client(string(), string(), rsa_private_key()) -> #chef_rest_client{}.

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

-spec generate_signed_headers(Client :: #chef_rest_client{},
                              Path :: http_path(),
                              Method :: http_method()) ->
                              [{string(), string()}, ...].
generate_signed_headers(#chef_rest_client{}=Client, Path, Method) ->
    generate_signed_headers(Client, Path, Method, <<"">>).

-spec generate_signed_headers(Client :: #chef_rest_client{},
                              Path :: binary(),
                              Method :: binary(),
                              Body :: binary()) ->
                              [{string(), string()}, ...].
generate_signed_headers(#chef_rest_client{user_name = UserName,
                                          private_key = PrivateKey,
                                          request_source = RequestSource},
                        Path, Method, Body) ->
    ExtraHeaders = case RequestSource of
                       web ->
                           [{"x_ops_request_source", "web"}];
                       user ->
                           []
                   end,
    Headers0 = chef_authn:sign_request(PrivateKey, Body,
                                       list_to_binary(UserName),
                                       Method, now, Path),
    Headers = [{"Accept", "application/json"}|Headers0] ++ ExtraHeaders,
    Headers.

