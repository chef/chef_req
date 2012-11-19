-module(chef_req).

-export([request/3,
         request/4,
         request/5,
         generate_request_params/5,
         make_config/4,
         load_config/1,
         teststart/0
         ]).

%% For dialyzer -Wunderspecs
-type http_method() :: <<_:24,_:_*8>>.

-include("chef_req.hrl").
-include_lib("chef_rest_client.hrl").
-include_lib("eunit/include/eunit.hrl").
%-include_lib("ej/include/ej.hrl").

-define(gv(K, L), proplists:get_value(K, L)).
-define(ibrowse_opts, [{ssl_options, []}, {response_format, binary}]).

teststart() ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    application:start(ibrowse),
    load_config("examples/chef.config").

-spec request(Method::http_method_as_atom(), Path::string(),
              ReqConfig::#chef_req_config{}) ->
              ibrowse_response().
request(Method, Path, ReqConfig) ->
    request(Method, Path, [], <<"">>, ReqConfig).

-spec request(Method::http_method_as_atom(), Path::string(),
              Body::binary(), ReqConfig::#chef_req_config{}) ->
              ibrowse_response().
request(Method, Path, Body, ReqConfig) ->
    request(Method, Path, [], Body, ReqConfig).

-spec request(Method::http_method_as_atom(), Path::string(),
              [{string(), string()}, ...] | [], Body::binary(),
              ReqConfig::#chef_req_config{}) ->
              ibrowse_response().
request(Method, Path, Headers, Body, ChefReqConfig) ->
    {Url, FullHeaders, ReqMethod, ReqBody} =
      generate_request_params(Method, Path, Headers, Body, ChefReqConfig),
    ibrowse:send_req(Url, FullHeaders, ReqMethod, ReqBody, ?ibrowse_opts).

-spec generate_request_params(Method::http_method_as_atom(), Path::string(),
              [{string(), string()}, ...] | [], Body::binary(),
              ReqConfig::#chef_req_config{}) ->
              {string(), [{string(), string()}, ...], http_method_as_atom(), binary()}.
generate_request_params(Method, Path, Headers, Body,
    #chef_req_config{api_root = ApiRoot, base_path = BasePath, name = Name, private_key = Private}) ->
    FullPath = BasePath ++ "/" ++ Path,
    {Url, FullHeaders} = make_headers(method_to_bin(Method), ApiRoot, FullPath,
				      Name, Private, Body),
    FullHeaders1 = Headers ++ FullHeaders,
    {Url, FullHeaders1, Method, Body}.


-spec make_config(ApiRoot::string(), BasePath::string(), Name::string(), {key, Key::binary()} | string()) ->
      #chef_req_config{}.
make_config(ApiRoot, BasePath, Name, {key, Key}) ->
    Private = chef_authn:extract_private_key(Key),
    #chef_req_config{api_root = ApiRoot, base_path = BasePath, name = Name, private_key = Private};
make_config(ApiRoot, BasePath, Name, KeyPath) ->
    {ok, PBin} = file:read_file(KeyPath),
    make_config(ApiRoot, BasePath, Name, {key, PBin}).

-spec load_config(Path::string()) -> #chef_req_config{}.
%% @doc Loads Chef client configuration from the file at `Path'.
load_config(Path) ->
  {ok, Config} = file:consult(filename:absname(Path)),
    PrivatePath  = ?gv(private_key, Config),
    ApiURL = ?gv(server, Config),
    %{ok,{https,[],"api.opscode.com",443,
           %"/organizations/chef-oss-dev/nodes",[]}}
    {ok, {Scheme, _UserInfo, Host, Port, BasePath, _Query}} = http_uri:parse(ApiURL),

    Fmt = "~s://~s:~B",
    RootURL = lists:flatten(io_lib:format(Fmt, [Scheme, Host, Port])),

    Name = ?gv(username, Config),
    make_config(RootURL, BasePath, Name, PrivatePath).

-spec make_headers(Method::binary(), ApiRoot::string(),
                   Path::string(), Name::string(),
                   Private::rsa_private_key(), Body::binary()) ->
                   {string(), [{string(), string()}, ...]}.
make_headers(Method, ApiRoot, Path, Name, Private, Body) ->
    Client = chef_rest_client:make_chef_rest_client(ApiRoot, Name, Private),
    %% TODO: duplicates code from chef_rest_client:generate_signed_headers/4
    Url = ApiRoot ++ Path,
    Headers0 = chef_rest_client:generate_signed_headers(Client, list_to_binary(Path),
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

