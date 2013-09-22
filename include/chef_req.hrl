
-include_lib("public_key/include/public_key.hrl").

%% version used in X-CHEF-VERSION header sent to server
-define(CHEF_VERSION, "0.10.0").

-record(chef_req_config, {
          api_root :: string(),               % e.g., https://api.opscode.com:443
          base_path :: string(),                % e.g., /organizations/clownco
          name :: string(),                   % user or client name
          private_key :: rsa_private_key()}). % the parsed representation of your user.pem

-type header_list() :: [{string(), string()}, ...].

-type response_body() :: binary().

-type http_method_as_atom() :: get | put | post | delete.

-type ibrowse_response() :: {ok, string(), [{string(), string()}, ...], binary()} | {error, term()}.

