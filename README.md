# Chef Req

chef\_req is a library for making authenticated API requests in erlang.

## Try It
To get started in a quick and dirty fashion:

1. edit `examples/chef.config` with your Chef credentials.
2. compile the code by running `make`
3. Fire up the shell: `erl -pa ebin -pa deps/*/ebin`
4. Start the apps and read the config: `Conf = chef_req:teststart().`
5. Make a request: `chef_req:request(get, "/nodes", Conf).`

## Configuration
A sample config file is given in `examples/chef.config`:

    %% Your user or client name. This is configured as `node_name` in a
    %% knife.rb or client.rb file.
    {username, "kallistec"}.
    %% Path to the private key for your user or client.
    {private_key, "/Users/ddeleo/opscode-ops/chef-oss-dev/.chef/kallistec.pem"}.
    %% Your Chef server URL. This is configured as `chef_server_url` in
    %% a knife.rb or client.rb file.
    {server, "https://api.opscode.com/organizations/chef-oss-dev"}.

To read your configuration, call `chef_req:load_config/1` with the path
to your config file. The path can be relative to your cwd. The function
returns a `#chef_req_config{}` record that you can pass to the request
functions.

## Request API
Requests are made using `chef_req:request/3`, `chef_req:request/4`, or 
`chef_req:request/5`.

* `chef_req:request/3`: make requests that do not have a request body,
  such as GET.
        chef_req:request(get, "/nodes", Conf).
* `chef_req:request/4`: make requests that have a request body, such as
  a PUT or POST.
        chef_req:request(post, "/nodes", <<"node data as JSON">>, Conf).
* `chef_req:request/5`: Allows you to specify additional headers to be
  sent with the request.

