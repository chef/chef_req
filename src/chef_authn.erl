%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@opscode.com>
%% @author Christopher Brown <cb@opscode.com>
%% @copyright 2011 Opscode, Inc.
%% @doc chef_authn - Request signing and authentication for Opscode Chef
%%
%% This module is an Erlang port of the mixlib-authentication Ruby gem.
%% It can be used to sign HTTP requests to send to a Chef server or to
%% validate such requests (for server implementation).

-module(chef_authn).
-include("chef_authn.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").


-define(buf_size, 16384).

-export([
         sign_request/5,
         sign_request/6,
         sign_request/8,
         extract_public_or_private_key/1,
         extract_private_key/1,
         hash_string/1,
         hash_file/1,
         authenticate_user_request/6,
         validate_headers/2
         ]).

-ifdef(TEST).
-compile([export_all]).
-endif.


-include_lib("public_key/include/public_key.hrl").


-spec sign_request(rsa_private_key(), user_id(), http_method(),
                   erlang_time(), http_path()) ->
      [{binary(), binary()}, ...].
%% @doc Sign an HTTP request without a body (primarily GET)
%% === Arguments ===
%% <ul>
%% <li> `PrivateKey': an rsa_private_key record (as defined in the public_key
%%       module. {@link chef_authn:extract_public_or_private_key/1} can create
%%       this record from a binary. </li>
%% <li> `User': Chef User or Client name, as a binary. </li>
%% <li> `Method': HTTP Method (e.g., GET, PUT, POST, DELETE) as a binary. </li>
%% <li> `Time': The timestamp to include with the request signature, in
%%      the format given by `calendar:universal_time()' </li>
%% <li> `Path': full path to the resource, including protocol and host. </li>
%% </ul>
sign_request(PrivateKey, User, Method, Time, Path) ->
    sign_request(PrivateKey, <<"">>, User, Method, Time, Path, ?signing_algorithm, ?signing_version).

%% @doc Sign an HTTP request with a body (PUT/POST)
-spec sign_request(rsa_private_key(), http_body(), user_id(), http_method(),
                   erlang_time(), http_path()) ->
    [{binary(), binary()}, ...].

sign_request(PrivateKey, Body, User, Method, Time, Path) ->
    sign_request(PrivateKey, Body, User, Method, Time, Path, ?signing_algorithm, ?signing_version).

%% @doc Sign an HTTP request so it can be sent to a Chef server.
%%
%% Returns a list of header tuples that should be included in the
%% final HTTP request.
%%
-spec sign_request(rsa_private_key(), http_body(), user_id(), http_method(),
                   erlang_time(), http_path(), signing_algorithm(), signing_version()) ->
    [{binary(), binary()}, ...].
sign_request(PrivateKey, Body, User, Method, Time, Path, SignAlgorithm, SignVersion) ->
    CTime = time_iso8601(Time),
    HashedBody = hashed_body(Body),
    SignThis = canonicalize_request(HashedBody, User, Method, CTime, Path, SignAlgorithm, SignVersion),
    Sig = base64:encode(public_key:encrypt_private(SignThis, PrivateKey)),
    X_Ops_Sign = iolist_to_binary(io_lib:format("version=~s", [SignVersion])),
    [{<<"X-Ops-Content-Hash">>, HashedBody},
     {<<"X-Ops-UserId">>, User},
     {<<"X-Ops-Sign">>, X_Ops_Sign},
     {<<"X-Ops-Timestamp">>, CTime}]
       ++ sig_header_items(Sig).

-spec extract_public_or_private_key(binary()) -> term() | {error, bad_key}.
%% @doc Accepts a public or private key in PEM format and returns a RSA public
%% or private key record as appropriate. See documentation for the public_key
%% module for more information about these data types.
extract_public_or_private_key(RawKey) ->
    try public_key:pem_decode(RawKey) of
        [Key] -> process_key(Key)
    catch
        _:_ ->
            {error, bad_key}
    end.

-spec extract_private_key(binary()) -> term() | {error, bad_key}.
extract_private_key(RawKey) ->
    case catch public_key:pem_decode(RawKey) of
        [{Type, Der, _}] ->
            public_key:der_decode(Type, Der);
        _ ->
            {error, bad_key}
    end.

-spec process_key( {'RSAPublicKey', binary(), _} |
                   {'SubjectPublicKeyInfo', _, _}) ->
                         rsa_public_key() | rsa_private_key() | {error, bad_key}.
process_key({'SubjectPublicKeyInfo', _, _} = PubEntry) ->
    public_key:pem_entry_decode(PubEntry);
process_key({'RSAPublicKey', Der, _}) ->
    public_key:der_decode('RSAPublicKey', Der);
process_key({Type, Der, _}) ->
    public_key:der_decode(Type, Der).
%process_key(_) ->
%    {error, bad_key}.

-spec(hash_string(string()|binary()) -> sha_hash64()).
%% @doc Base 64 encoded SHA1 of `Str'
hash_string(Str) ->
    base64:encode(crypto:sha(Str)).

-spec(hash_file(pid()) -> sha_hash64()).
%% @doc Base 64 encoded SHA1 of contents of `F', which must be the pid of a file
hash_file(F) ->
    hash_file(F, crypto:sha_init()).

-spec hash_file(file:io_device(),binary()) -> sha_hash64().
hash_file(F, Ctx) ->
    case io:get_chars(F, "", ?buf_size) of
        eof ->
            base64:encode(crypto:sha_final(Ctx));
        Data ->
            hash_file(F, crypto:sha_update(Ctx, Data))
    end.



%% @doc Converts Erlang time-tuple to iso8601 formatted date string.
%%
%% Example output looks like `<<"2003-12-13T18:30:02Z">>'
-spec(time_iso8601(erlang_time() | 'now') -> binary()).
time_iso8601(now) ->
    time_iso8601(calendar:universal_time());
time_iso8601({{Year, Month, Day}, {Hour, Min, Sec}}) ->
    % Is there a way to build a binary straight away?
    Fmt = "~4B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
    iolist_to_binary(io_lib:format(Fmt,
                                   [Year, Month, Day,
                                    Hour, Min, Sec])).

%% @doc Convert an iso8601 time string to Erlang date time
%% representation.
-spec(time_iso8601_to_date_time(string()|binary()) -> erlang_time()).
time_iso8601_to_date_time(ATime) when is_binary(ATime) ->
    time_iso8601_to_date_time(binary_to_list(ATime));
time_iso8601_to_date_time(ATime) ->
    [Year, Month, Day, Hour, Min, Sec] =
        [ list_to_integer(S) || S <- string:tokens(ATime, "-T:Z") ],
    {{Year, Month, Day}, {Hour, Min, Sec}}.

-spec(canonical_time(string() | binary()) -> iso8601_time()).
%% @doc Convert a string or binary HTTP request time to iso8601 format
canonical_time(T) when is_binary(T) ->
    canonical_time(binary_to_list(T));
canonical_time(T) when is_list(T) ->
    time_iso8601(httpd_util:convert_request_date(T)).

%% @doc Canonicalize an HTTP request path by removing doubled slashes
%% and trailing slash (except for case of root path).
-spec  canonical_path(binary()) -> binary().
canonical_path(Path = <<"/">>) ->
    Path;
canonical_path(Path) ->
    NoDoubles = re:replace(Path, "/+/", <<"/">>, [{return, binary}, global]),
    Path1 = re:replace(NoDoubles, "/$", % fix emacs erlang-mode: "
                       "", [{return, binary}]),
    %% remove query parameters
    re:replace(Path1, "\\?.*$", "", [{return, binary}]).


%% @doc Canonicalize HTTP method as all uppercase binary

canonical_method(Method) ->
    list_to_binary(string:to_upper(binary_to_list(Method))).

-spec(hashed_body(binary() | pid()) -> binary()).
%% @doc Return the SHA1 hash of the body which can either be a binary
%% or the pid of a file.
hashed_body(Body) when is_pid(Body) ->
    hash_file(Body);
hashed_body(Body) when is_binary(Body) ->
    hash_string(Body);
hashed_body(Body) when is_list(Body) ->
    hashed_body(iolist_to_binary(Body)).

-spec(canonicalize_request(sha_hash64(), user_id(), http_method(), iso8601_time(),
                           http_path(), signing_algorithm(), signing_version())
      -> binary()).
%% @doc Canonicalize an HTTP request into a binary that can be signed
%% for verification.
%%
%% NOTE: this function assumes that `Time' is already in canonical
%% form (see canonical_time/1).  Other arguments are canonicalized.
%%
canonicalize_request(BodyHash, UserId, _Method, Time, _Path, _SignAlgorithm, _SignVersion)
  when BodyHash =:= undefined orelse
         UserId =:= undefined orelse
           Time =:= undefined ->
    undefined;
canonicalize_request(BodyHash, UserId, Method, Time, Path, _SignAlgorithm, SignVersion) ->
    Format = ?version1_sig_format,
    CanonicalUserId = case SignVersion of
                          <<"1.1">> ->
                              hash_string(UserId);
                          <<"1.0">> ->
                              UserId
                      end,
    iolist_to_binary(io_lib:format(Format, [canonical_method(Method),
                                            hash_string(canonical_path(Path)),
                                            BodyHash,
                                            Time,
                                            CanonicalUserId])).


%% @doc Generate X-Ops-Authorization-I for use in building auth headers
-spec xops_header(non_neg_integer()) -> header_name().
xops_header(I) ->
    iolist_to_binary(io_lib:format(<<"X-Ops-Authorization-~B">>, [I])).

%% @doc Given an encrypted signature base64 binary, split it up with
%% line feeds evry 60 characters and build up a list of
%% X-Ops-Authorization-i header tuples.
%%
-spec sig_header_items(binary()) -> [{binary(),binary()}].
sig_header_items(Sig) ->
    % Ruby's Base64.encode64 method inserts line feeds every 60
    % encoded characters.
    Lines = sig_to_list(Sig, 60),
    [ {xops_header(I), L} ||
        {L, I} <- lists:zip(Lines, lists:seq(1, length(Lines))) ].

%% @doc Split a binary into chunks of size N
%% -spec sig_to_list(binary(), pos_integer()) -> [binary()]. % TODO PROBLEMATIC
sig_to_list(Sig, N) ->
    lists:reverse(sig_to_list(Sig, N, [])).

-spec sig_to_list(binary(), 60, [<<_:480>>]) -> [binary(), ...].
sig_to_list(Sig, N, Acc) ->
    case iolist_size(Sig) =< N of
        true ->
            [Sig|Acc];
        false ->
            <<Line:N/binary, Rest/binary>> = Sig,
            sig_to_list(Rest, N, [Line|Acc])
    end.

%% @doc Validate that all required headers are present
%%
%% Returns 'ok' if all required headers are present.  Otherwise, throws
%% `{missing, [header_name()]}' providing a list of the
%% missing headers in the exception.
%%
%% @throws {missing, [binary()]} | bad_clock | bad_sign_desc
%%
-spec validate_headers(header_fun(), time_skew()) -> [{'algorithm',binary()} |
                                                      {'version',binary()},...].
validate_headers(GetHeader, TimeSkew) ->
    Missing = [ H || H <- ?required_headers, GetHeader(H) == undefined ],
    case Missing of
        [] ->
            validate_time_in_bounds(GetHeader, TimeSkew),
            validate_sign_description(GetHeader);
        TheList -> throw({missing_headers, TheList})
    end.

%% @doc Validate that the request time is within `TimeSkew' seconds of now.
%%
%% Returns 'ok' if request time in the X-Ops-Timestamp header is
%% wihtin bounds.  Otherwise, throws `bad_clock'
%%
%% @throws bad_clock
%%
-spec validate_time_in_bounds(header_fun(), time_skew()) -> 'ok' | no_return().
validate_time_in_bounds(GetHeader, TimeSkew) ->
    ReqTime = GetHeader(<<"X-Ops-Timestamp">>),
    case time_in_bounds(ReqTime, TimeSkew) of
        true -> ok;
        false -> throw(bad_clock);
        invalid_reqtime -> throw({bad_headers, [<<"X-Ops-Timestamp">>]})
    end.

%% @doc Validate that the X-Ops-Sign header describes a supported signing format.
%%
%% Returns 'ok' if the signing format is supported.  Otherwise, throws
%% `bad_sign_desc'
%%
%% @throws bad_sign_desc
%%
-spec validate_sign_description(header_fun()) -> [{'algorithm',binary()} |
                                                  {'version',binary()},...].
validate_sign_description(GetHeader) ->
    SignDesc = parse_signing_description(GetHeader(<<"X-Ops-Sign">>)),
    SignVersion = proplists:get_value(?signing_version_key, SignDesc),
    SignAlgorithm = proplists:get_value(?signing_algorithm_key, SignDesc),
    case lists:member(SignVersion, ?signing_versions) of
        true ->
            [{algorithm, SignAlgorithm}, {version, SignVersion}];
        false ->
            throw(bad_sign_desc)
    end.

%% @doc Determine if a request is valid
%%
%% The `GetHeader' argument is a fun that closes over the request
%% headers and can be called to obtain the value of a header.  It
%% should either return the value of the header as binary or
%% 'undefined'.
%%
%% A request signed with a timestamp more than `TimeSkew' seconds from
%% now will not be authenticated.
%%
%% `PublicKey' is a binary containing an RSA public key in PEM format.
%%
-spec authenticate_user_request(get_header_fun(),
                                http_method(),
                                http_path(),
                                http_body(),
                                public_key_data(),
                                time_skew()) ->
				       {name, user_id()} | {no_authn, Reason::term()}.
authenticate_user_request(GetHeader, Method, Path, Body, PublicKey, TimeSkew) ->
    try
        do_authenticate_user_request(GetHeader, Method, Path, Body, PublicKey, TimeSkew)
    catch
        throw:Why -> {no_authn, Why}
    end.

-spec do_authenticate_user_request(get_header_fun(),
				   http_method(),
				   http_path(),
				   http_body(),
				   public_key_data(),
                   time_skew())
				  ->  {name, user_id()} | {no_authn, bad_sig}.

do_authenticate_user_request(GetHeader, Method, Path, Body, PublicKey, TimeSkew) ->
    % NOTE: signing description validation and time_skew validation
    % are done in the wrapper function.
    UserId = GetHeader(<<"X-Ops-UserId">>),
    ReqTime = GetHeader(<<"X-Ops-Timestamp">>),
    ContentHash = GetHeader(<<"X-Ops-Content-Hash">>),
    AuthSig = sig_from_headers(GetHeader, 1, []),
    Decrypted = decrypt_sig(AuthSig, PublicKey),
    [{algorithm, SignAlgorithm}, {version, SignVersion}] =  validate_headers(GetHeader, TimeSkew),

    BodyHash = hashed_body(Body),
    Plain = canonicalize_request(BodyHash, UserId, Method, ReqTime,
                                 Path, SignAlgorithm, SignVersion),
    try
        Decrypted = Plain,
        %% the signing will also validate this, but since we require that the
        %% X-Ops-Content-Hash be sent, we should verify it. A TODO item is to move this
        %% check early in the request handling so that we error out before fetching key data
        %% if the content hash is wrong.
        ContentHash = BodyHash,
        {name, UserId}
    catch
        error:{badmatch, _} -> {no_authn, bad_sig}
    end.

-spec decrypt_sig(binary(), public_key_data() | rsa_public_key()) -> binary() | decrypt_failed.
decrypt_sig(Sig, {'RSAPublicKey', _, _} = PK) ->
    try
        public_key:decrypt_public(base64:decode(Sig), PK)
    catch
        error:{badmatch, _} ->
            decode_failed;
        error:decrypt_failed ->
            decrypt_failed
    end;
decrypt_sig(Sig, {Type, _} = KeyData)  when Type =:= cert orelse Type=:= key ->
    PK = read_key_data(KeyData),
    decrypt_sig(Sig, PK).

sig_from_headers(GetHeader, I, Acc) ->
    Header = xops_header(I),
    case GetHeader(Header) of
        undefined ->
            iolist_to_binary(lists:reverse(Acc));
        Part ->
            sig_from_headers(GetHeader, I+1, [Part|Acc])
    end.

-spec time_in_bounds(undefined | string() | binary(), pos_integer()) -> boolean() | invalid_reqtime.
time_in_bounds(undefined, _Skew) ->
    false;
time_in_bounds(ReqTime, Skew) ->
    Now = calendar:now_to_universal_time(os:timestamp()),
    try
        time_in_bounds(time_iso8601_to_date_time(ReqTime), Now, Skew)
    catch
        error:_ ->
            invalid_reqtime
    end.

-spec time_in_bounds(erlang_time(), erlang_time(), pos_integer() ) -> boolean().
time_in_bounds(T1, T2, Skew) ->
    S1 = calendar:datetime_to_gregorian_seconds(T1),
    S2 = calendar:datetime_to_gregorian_seconds(T2),
    (S2 - S1) < Skew.

-spec parse_signing_description('undefined' | binary()) -> [{binary(),binary()}].
parse_signing_description(undefined) ->
    [];
parse_signing_description(Desc) ->
    [ {Key, Value} ||
        [Key, Value] <- [ re:split(KV, "=") || KV <- re:split(Desc, ";") ] ].

-spec read_key_data(public_key_data()) -> rsa_public_key().
read_key_data({cert, Data}) when is_binary(Data) ->
    read_cert(Data);
read_key_data({key, Data}) ->
    read_public_key(Data).


-spec read_public_key(binary() |
                      {'RSAPublicKey', binary(), _} |
                      {'SubjectPublicKeyInfo', _, _}) -> rsa_public_key().
read_public_key({'RSAPublicKey', Der, _}) ->
    public_key:der_decode('RSAPublicKey', Der);
read_public_key({'SubjectPublicKeyInfo', _, _} = PubEntry) ->
    public_key:pem_entry_decode(PubEntry);
read_public_key(Bin) when is_binary(Bin) ->
    [Decode] = public_key:pem_decode(Bin),
    read_public_key(Decode).


-spec read_cert(binary()) -> rsa_public_key().  %% der_decode only spec's term
read_cert(Bin) ->
    Cert = public_key:pem_entry_decode(hd(public_key:pem_decode(Bin))),
    TbsCert = Cert#'Certificate'.tbsCertificate,
    Spki = TbsCert#'TBSCertificate'.subjectPublicKeyInfo,
    {0, KeyDer} = Spki#'SubjectPublicKeyInfo'.subjectPublicKey,
    public_key:der_decode('RSAPublicKey', KeyDer).
