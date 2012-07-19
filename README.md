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

## Request API
Requests are made using `chef\_req:request/3`, `chef\_req:request/4`, or 
`chef\_req:request/5`.

* `chef\_req:request/3`: make requests that do not have a request body,
  such as GET.
        chef_req:request(get, "/nodes", Conf).
* `chef\_req:request/4`: make requests that have a request body, such as
  a PUT or POST.
        chef_req:request(post, "/nodes", <<"node data as JSON">>, Conf).
* `chef_req:request/5`: Allows you to specify additional headers to be
  sent with the request.

