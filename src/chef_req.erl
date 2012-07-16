-module(chef_req).

-export([request/3,
         request/4,
         request/5,
         make_config/3,
         load_config/1,
         clone_config/3
         ]).

-include("chef_req.hrl").
-include("chef_authn.hrl").
-include_lib("chef_rest_client.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("ej/include/ej.hrl").

-define(gv(K, L), proplists:get_value(K, L)).
-define(ibrowse_opts, [{ssl_options, []}, {response_format, binary}]).


request(Method, Path, ReqConfig) ->
    request(Method, Path, [], <<"">>, ReqConfig).

request(Method, Path, Body, ReqConfig) ->
    request(Method, Path, [], Body, ReqConfig).

request(Method, Path, Headers, Body,
        #req_config{api_root = ApiRoot, name = Name, private_key = Private}) ->
    {Url, FullHeaders} = make_headers(method_to_bin(Method), ApiRoot, Path,
				      Name, Private, Body),
    FullHeaders1 = Headers ++ FullHeaders,
    ibrowse:send_req(Url, FullHeaders1, Method, Body, ?ibrowse_opts).

make_config(ApiRoot, Name, {key, Key}) ->
    Private = chef_authn:extract_private_key(Key),
    #req_config{api_root = ApiRoot, name = Name, private_key = Private};
make_config(ApiRoot, Name, KeyPath) ->
    {ok, PBin} = file:read_file(KeyPath),
    make_config(ApiRoot, Name, {key, PBin}).

clone_config(#req_config{}=Config, Name, Key) ->
    Private = chef_authn:extract_private_key(Key),
    Config#req_config{name = Name, private_key = Private}.

load_config(Path) ->
    {ok, Config} = file:consult(Path),
    PrivatePath  = ?gv(private_key, Config),
    ApiRoot = ?gv(api_root, Config),
    Name = ?gv(client_name, Config),
    make_config(ApiRoot, Name, PrivatePath).

-spec make_headers(Method::http_method(), ApiRoot::string(),
                   Path::string(), Name::string(),
                   Private::rsa_private_key(), Body::binary()) ->
                   {string(), [{string(), string()}, ...]}.
make_headers(Method, ApiRoot, Path, Name, Private, Body) ->
    Client = chef_rest_client:make_chef_rest_client(ApiRoot, Name, Private),
    %% TODO: duplicates code from chef_rest_client:generate_signed_headers/4
    Url = ApiRoot ++ Path,
    Headers0 = chef_rest_client:generate_signed_headers(Client, Path,
                                                        Method, Body),
    Headers1 = header_for_body(Body, Headers0),
    {Url, [{"Accept", "application/json"},
           {"X-CHEF-VERSION", ?CHEF_VERSION} | Headers1]}.

-spec header_for_body(binary(), [{string(), string()}]) ->
    [{string(), string()}, ...].
header_for_body(<<"">>, Headers) ->
    Headers;
header_for_body(_Body, Headers) when is_binary(_Body) ->
    [{"content-type", "application/json"}|Headers].


-spec method_to_bin('delete' | 'get' | 'head' | 'post' | 'put') -> http_method().
method_to_bin(get) ->
    <<"GET">>;
method_to_bin(put) ->
    <<"PUT">>;
method_to_bin(post) ->
    <<"POST">>;
method_to_bin(delete) ->
    <<"DELETE">>;
method_to_bin(head) ->
    <<"HEAD">>.

