# Chef Req

chef_req is a library for making authenticated API requests in erlang.

## Try It

To get started in a quick and dirty fashion:

1. Install erlang, rebar, etc. Erlang R15B is required for now.
2. edit `examples/chef.config` with your Chef credentials.
3. Fetch deps and compile the code: `make compile`
4. Fire up the shell: `erl -pa ebin -pa deps/*/ebin`
5. Start the apps and read the config: `Conf = chef_req:manual_start().`
6. Make a request: `chef_req:request(get, "/nodes", Conf).`

## Configuration

A sample config file is given in `examples/chef.config`:

```
%% Your user or client name. This is configured as `node_name` in a
%% knife.rb or client.rb file.
{username, "kallistec"}.
%% Path to the private key for your user or client.
{private_key, "/Users/ddeleo/opscode-ops/chef-oss-dev/.chef/kallistec.pem"}.
%% Your Chef server URL. This is configured as `chef_server_url` in
%% a knife.rb or client.rb file.
{server, "https://api.opscode.com/organizations/chef-oss-dev"}.
```

To read your configuration, call `chef_req:load_config/1` with the path to your config file. The path can be relative to your cwd. The function returns a `#chef_req_config{}` record that you can pass to the request functions.

## Request API

Requests are made using `chef_req:request/3`, `chef_req:request/4`, or `chef_req:request/5`.

- `chef_req:request/3`: Make requests that do not have a request body, such as GET:

  ```erlang
  chef_req:request(get, "/nodes", Conf).
  ```

- `chef_req:request/4`: Make requests that have a request body, such as a PUT or POST:

  ```erlang
  chef_req:request(post, "/nodes", <<"node data as JSON">>, Conf).
  ```

- `chef_req:request/5`: Allows you to specify additional headers to be sent with the request.

## Contributing

For information on contributing to this project see <https://github.com/chef/chef/blob/master/CONTRIBUTING.md>

## License

- Copyright:: 2012-2016 Chef Software, Inc.
- License:: Apache License, Version 2.0

```text
Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
```
