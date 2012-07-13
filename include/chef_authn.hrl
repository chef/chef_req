-define(signing_algorithm, <<"sha1">>).

-define(signing_version, <<"1.1">>).

-define(signing_version_v1_0, <<"1.0">>).

-define(signing_versions, [?signing_version_v1_0, ?signing_version]).

-define(signing_version_key, <<"version">>).

-define(signing_algorithm_key, <<"algorithm">>).

-define(version1_sig_format, <<"Method:~s\nHashed Path:~s\n"
                               "X-Ops-Content-Hash:~s\n"
                               "X-Ops-Timestamp:~s\nX-Ops-UserId:~ts">>).

-define(required_headers, [<<"X-Ops-UserId">>,
                           <<"X-Ops-Timestamp">>,
                           <<"X-Ops-Sign">>,
                           % FIXME: mixlib-authorization requires host, but
                           % it is not used as part of the signing protocol AFAICT
                           % <<"host">>,
                           <<"X-Ops-Content-Hash">>]).

