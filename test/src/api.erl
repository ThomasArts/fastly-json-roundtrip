%% coding: latin-1
%% This code is generated from /Users/thomas/Quviq/Customers/Fastly/json_parser/specs/api.yaml
%% on 2022-01-21 12:15:47 UTC
%% Using openapi rebar3 plugin version: #b9fccc0
%% Do not manually change this code!
%%
%% json_schema/0 implements a JSON Schema for the definitions
%% Reference should be fixed!
%% At the moment, ref with definition is not folowwed by jesse
%% Use jsx:prettify(jsx:encode(json_schema())) to get a JSON string.

-module(api).

-export([json_schema/0]).
-export([operation/1, operations/0, definitions/0, baseurl/1]).

-export([request/3, http_request/4]).
-export([validate_request/2, validate_response/5]).

baseurl(Config) ->
  proplists:get_value(base_url, Config, <<"http://localhost:7676">>).

definitions() ->
    [].

json_schema() ->
    #{<<"$schema">> => <<"http://json-schema.org/draft-04/schema#">>,
      <<"properties">> =>
          maps:from_list([{iolist_to_binary(K), V} || {K, V} <- definitions()])}.

operations() ->
    [{postJson,
      #{method => post, parameters => [], path => <<"/json">>,
        requestBody => [{"text/plain", #{schema => #{<<"type">> => <<"string">>}}}],
        responses => #{200 => [{"application/json", undefined}]}, tags => []}},
     {putJson,
      #{method => put, parameters => [], path => <<"/json">>,
        requestBody =>
            [{"application/json", #{schema => #{<<"type">> => <<"object">>}}}],
        responses =>
            #{200 => [{"application/json", #{schema => #{<<"type">> => <<"string">>}}}]},
        tags => []}}].

operation(Id) -> proplists:get_value(Id, operations()).



request(OpId, Params, Config) ->
    request(api, OpId, Params, Config).

validate_request(OperationId, Args) ->
    validate_request(api, OperationId, Args).

validate_response(OperationId, Request, StatusCode,
                  Headers, RawBody) ->
    validate_response(api,
                      OperationId,
                      Request,
                      StatusCode,
                      Headers,
                      RawBody).

request(Mod, OpId, ParamsIn, Config) ->
    begin
        Params = if is_list(ParamsIn) ->
                        maps:from_list(ParamsIn);
                    true -> ParamsIn
                 end,
        Op = Mod:operation(OpId),
        Method = maps:get(method, Op),
        BaseUrl = string:trim(Mod:baseurl(Config),
                              trailing,
                              "/"),
        {ParamHeaders, _} = header(Op, Params),
        {Path, PKeys} = path(Op, Params),
        {Query, QKeys} = query(Op, Params),
        Headers = ParamHeaders ++
                      proplists:get_value(headers, Config, []) ++
                          case proplists:get_value(security, Config) of
                              undefined -> [];
                              {authorization_basic,
                               [#{username := User, password := Password}]} ->
                                  Token =
                                      base64:encode(unicode:characters_to_binary([User,
                                                                                  ":",
                                                                                  Password])),
                                  [{"Authorization",
                                    to_str(["Basic ", Token])}];
                              {authorization_bearer,
                               [#{accessToken := Token}]} ->
                                  [{"Authorization",
                                    to_str(["Bearer ", Token])}];
                              {api_key, [ApiKey]} ->
                                  case get_by_similar_key(ApiKey, Params) of
                                      undefined -> [];
                                      {_, ApiKeyValue} ->
                                          [{ApiKey, to_str(ApiKeyValue)}]
                                  end
                          end,
        BinKeys = PKeys ++
                      QKeys ++ [list_to_binary(K) || {K, _} <- Headers],
        BodyParams = maps:without(BinKeys ++
                                      [binary_to_list(K)
                                       || K <- BinKeys, is_binary(K)]
                                          ++
                                          [list_to_atom(binary_to_list(K))
                                           || K <- BinKeys, is_binary(K)],
                                  Params),
        WithoutBody = case maps:size(BodyParams) == 0 of
                          true -> [];
                          false -> BodyParams
                      end,
        case Method of
            get ->
                #{method => Method,
                  url => iolist_to_binary([BaseUrl, Path, Query]),
                  headers => Headers};
            _ ->
                RBs = maps:get(requestBody, Op, []),
                {ContentType, RequestBody} = case
                                                 proplists:get_value("Content-Type",
                                                                     Headers)
                                                 of
                                                 undefined ->
                                                     case maps:get(body,
                                                                   Params,
                                                                   undefined)
                                                         of
                                                         undefined
                                                             when WithoutBody ==
                                                                      [] ->
                                                             {"text/plain", []};
                                                         undefined ->
                                                             make_body(RBs,
                                                                       WithoutBody,
                                                                       Path);
                                                         Body ->
                                                             make_body(RBs,
                                                                       Body,
                                                                       Path)
                                                     end;
                                                 "multipart/form-data" ->
                                                     MultiParts =
                                                         maps:get(multipart,
                                                                  Params),
                                                     Boundary = "eqc2020",
                                                     StartBoundary = "--" ++
                                                                         Boundary,
                                                     LineSeparator = "\r\n",
                                                     Parts = lists:map(fun
                                                                           ({Key,
                                                                             Value,
                                                                             Mime}) ->
                                                                               {NewMime,
                                                                                PartBody} =
                                                                                   make_body(Mime,
                                                                                             Value,
                                                                                             undefined),
                                                                               iolist_to_binary([StartBoundary,
                                                                                                 LineSeparator,
                                                                                                 "Content-Disposition: form-data; name=\"",
                                                                                                 Key,
                                                                                                 "\"",
                                                                                                 LineSeparator,
                                                                                                 "Content-Type: ",
                                                                                                 NewMime,
                                                                                                 LineSeparator,
                                                                                                 LineSeparator,
                                                                                                 PartBody,
                                                                                                 LineSeparator])
                                                                       end,
                                                                       MultiParts),
                                                     Body = to_str(Parts ++
                                                                       [StartBoundary,
                                                                        <<"--">>,
                                                                        LineSeparator]),
                                                     {"multipart/form-data; boundary="
                                                          ++ Boundary,
                                                      Body};
                                                 CT ->
                                                     case maps:get(body,
                                                                   Params,
                                                                   undefined)
                                                         of
                                                         undefined ->
                                                             make_body(CT,
                                                                       WithoutBody,
                                                                       Path);
                                                         Body ->
                                                             make_body(CT,
                                                                       Body,
                                                                       Path)
                                                     end
                                             end,
                #{method => Method,
                  url => iolist_to_binary([BaseUrl, Path, Query]),
                  headers => Headers, body => RequestBody,
                  content_type => ContentType}
        end
    end.

header(#{path := Endpoint, parameters := Parameters},
       Args)
    when is_map(Args) ->
    begin
        InHeader = [Param
                    || Param <- Parameters,
                       lists:member({"in", "header"}, Param)],
        {lists:foldl(fun (Param, Headers) ->
                             Name = proplists:get_value("name", Param),
                             case {proplists:get_value("required",
                                                       Param,
                                                       false),
                                   get_by_similar_key(Name, Args)}
                                 of
                                 {false, undefined} -> Headers;
                                 {true, undefined} ->
                                     error({required, Name, Param, Endpoint});
                                 {_, {Key, Value}} ->
                                     [{to_str(Key), to_str(Value)} | Headers]
                             end
                     end,
                     [],
                     InHeader),
         [iolist_to_binary(Key)
          || P <- InHeader, {"name", Key} <- P]}
    end.

path(#{path := Endpoint, parameters := Parameters},
     Args)
    when is_map(Args) ->
    begin
        InPath = [Param
                  || Param <- Parameters,
                     lists:member({"in", "path"}, Param)],
        {lists:foldl(fun (Param, Path) ->
                             Name = proplists:get_value("name", Param),
                             case {proplists:get_value("required",
                                                       Param,
                                                       false),
                                   get_by_similar_key(Name, Args)}
                                 of
                                 {false, undefined} -> Path;
                                 {true, undefined} ->
                                     throw({error,
                                            {required, Name, Param, Endpoint}});
                                 {_, {_Key, Value}} ->
                                     iolist_to_binary(string:replace(Path,
                                                                     "{" ++
                                                                         Name ++
                                                                             "}",
                                                                     to_str(Value)))
                             end
                     end,
                     Endpoint,
                     InPath),
         [iolist_to_binary(Key)
          || P <- InPath, {"name", Key} <- P]}
    end.

query(#{path := Endpoint, parameters := Parameters},
      Args)
    when is_map(Args) ->
    begin
        InQuery = [Param
                   || Param <- Parameters,
                      lists:member({"in", "query"}, Param)],
        Query = lists:foldr(fun (Param, Query) ->
                                    Name = proplists:get_value("name", Param),
                                    Explode = proplists:get_value("explode",
                                                                  Param,
                                                                  false),
                                    case {proplists:get_value("required",
                                                              Param,
                                                              false),
                                          get_by_similar_key(Name,
                                                             Args,
                                                             Explode)}
                                        of
                                        {false, undefined} -> Query;
                                        {true, undefined} ->
                                            throw({error,
                                                   {required,
                                                    Name,
                                                    Param,
                                                    Endpoint}});
                                        {_, {_, Value}} ->
                                            query_serialize(Name, Value, Param)
                                                ++ Query
                                    end
                            end,
                            [],
                            InQuery),
        {case [[K, "=", V] || {K, V} <- Query] of
             [] -> <<>>;
             Qs -> iolist_to_binary(["?" | lists:join("&", Qs)])
         end,
         [iolist_to_binary(Key)
          || P <- InQuery, {"name", Key} <- P]}
    end.

query_serialize(Name, Value, Param) ->
    begin
        Style = proplists:get_value("style", Param, "form"),
        Explode = proplists:get_value("explode", Param, true),
        Array = is_array(proplists:get_value("schema", Param)),
        AllowReserved = proplists:get_value("allowReserved",
                                            Param,
                                            false),
        EncodedValue = case AllowReserved of
                           true -> Value;
                           false when Array ->
                               [http_uri:encode(to_str(V)) || V <- Value];
                           false -> http_uri:encode(to_str(Value))
                       end,
        case {Array, Explode} of
            {false, _} -> [{Name, EncodedValue}];
            {true, true} -> [{Name, V} || V <- EncodedValue];
            {true, false} ->
                case Style of
                    "form" ->
                        [{Name,
                          iolist_to_binary(lists:join(",", EncodedValue))}];
                    "spaceDelimited" ->
                        [{Name,
                          iolist_to_binary(lists:join("%20", EncodedValue))}];
                    "pipeDelimited" ->
                        [{Name,
                          iolist_to_binary(lists:join("|", EncodedValue))}]
                end
        end
    end.

is_array(Schema) -> maps:is_key(<<"items">>, Schema).

prepare_validation(Mod) ->
    [jesse:add_schema(Def, Schema)
     || {Def, Schema} <- Mod:definitions()].

validate(#{<<"$ref">> := Link}, Term) ->
    validate(proplists:get_value(binary_to_list(Link),
                                 definitions(),
                                 #{}),
             Term);
validate(#{<<"type">> := <<"object">>} = Schema, Term)
    when is_map(Term) ->
    try case maps:get(<<"required">>, Schema, []) --
                 maps:keys(Term)
            of
            [] -> jesse:validate_with_schema(Schema, Term);
            MissingKeys -> {error, [{missing_keys, MissingKeys}]}
        end
    catch
        Error -> {error, Error}
    end;
validate(Schema, Term) ->
    try jesse:validate_with_schema(Schema, Term) catch
        Error -> {error, Error}
    end.

validate_request(Mod, OperationId, Args)
    when is_list(Args) ->
    validate_request(Mod,
                     OperationId,
                     maps:from_list(Args));
validate_request(Mod, OperationId, Args)
    when is_map(Args) ->
    begin
        prepare_validation(Mod),
        #{parameters := Parameters} =
            Mod:operation(OperationId),
        ToCheck = [Param
                   || Param <- Parameters,
                      not lists:member({"in", "path"}, Param)],
        Errors = lists:foldl(fun (Param, Errs) ->
                                     Name = proplists:get_value("name", Param),
                                     case {proplists:get_value("required",
                                                               Param,
                                                               false),
                                           get_by_similar_key(Name, Args)}
                                         of
                                         {false, undefined} -> Errs;
                                         {true, undefined} ->
                                             [{required,
                                               Name,
                                               Param,
                                               OperationId}
                                              | Errs];
                                         {_, {_, Value}} ->
                                             case
                                                 validate(proplists:get_value("schema",
                                                                              Param,
                                                                              #{}),
                                                          Value)
                                                 of
                                                 {error, E} -> [E | Errs];
                                                 _ -> Errs
                                             end
                                     end
                             end,
                             [],
                             ToCheck),
        case Errors of
            [] -> ok;
            _ -> {errors, {Mod, OperationId, Args, Errors}}
        end
    end.

validate_response(Mod, OperationId, Request, StatusCode,
                  Headers, RawBody) ->
    begin
        AcceptedContentType =
            content_type(proplists:get_value("Accept",
                                             maps:get(headers, Request),
                                             undefined)),
        ContentType =
            content_type(proplists:get_value("content-type",
                                             Headers)),
        {ok, Body} = parse_body(ContentType,
                                iolist_to_binary(RawBody)),
        #{responses := Resps} = Mod:operation(OperationId),
        prepare_validation(Mod),
        case maps:get(StatusCode, Resps, error) of
            error -> {error, {StatusCode, unspecified}};
            [] -> {ok, StatusCode, Body};
            {"schema", Schema} ->
                case validate(Schema, Body) of
                    {ok, _} -> {ok, StatusCode, Body};
                    {error, E} -> {error, {validation, E}}
                end;
            ContentTypes ->
                case get_matching(ContentType, ContentTypes) of
                    #{schema := Schema} ->
                        case validate(Schema, Body) of
                            {ok, _} ->
                                case is_matching(ContentType,
                                                 AcceptedContentType)
                                    of
                                    true -> {ok, StatusCode, Body};
                                    false ->
                                        {error,
                                         {accepted_versus_provided_content_type,
                                          ContentType}}
                                end;
                            {error, E} -> {error, {validation, E}}
                        end;
                    undefined -> {ok, StatusCode, Body};
                    no_match ->
                        {error,
                         {header_content_type,
                          ContentType,
                          expected,
                          [K || {K, _} <- ContentTypes]}}
                end
        end
    end.

http_request(#{url := Url} = Req, HttpOpts, Opts, Cfg)
    when is_binary(Url) ->
    http_request(Req#{url := binary_to_list(Url)},
                 HttpOpts,
                 Opts,
                 Cfg);
http_request(#{method := Method, url := Url,
               headers := Headers} =
                 Req,
             HttpOpts, Opts, Cfg) ->
    begin
        {ok, ClientPid} = inets:start(httpc,
                                      [{profile, test_browser}],
                                      stand_alone),
        Resp = case Method of
                   get ->
                       log("GET ~p", [Url], Cfg),
                       httpc:request(Method,
                                     {Url, Headers},
                                     HttpOpts,
                                     [{socket_opts, [{reuseaddr, true}]}
                                      | Opts],
                                     ClientPid);
                   _ ->
                       ContentType = maps:get(content_type, Req, ""),
                       Body = maps:get(body, Req, ""),
                       log("~p ~p~nContentType ~p~n~p",
                           [Method, Url, ContentType, Body],
                           Cfg),
                       httpc:request(Method,
                                     {Url, Headers, ContentType, Body},
                                     HttpOpts,
                                     [{socket_opts, [{reuseaddr, true}]}
                                      | Opts],
                                     ClientPid)
               end,
        ok = gen_server:stop(ClientPid, normal, infinity),
        process_response(Resp, Cfg)
    end.

process_response({ok, {{_, Code, Msg}, Headers, Body}},
                 Cfg) ->
    begin
        log("Return code: ~p (~p), Headers: ~p",
            [Code, Msg, Headers],
            Cfg),
        {ok, Code, Headers, Body}
    end;
process_response({error, Reason} = Error, Cfg) ->
    begin log("Return error: ~p", [Reason], Cfg), Error end.

parse_body("application/json", <<>>) -> {ok, #{}};
parse_body("application/json", Body) ->
    {ok, jsx:decode(Body, [return_maps])};
parse_body("text/plain;charset=UTF-8", Bin) ->
    {ok, unicode:characters_to_binary(Bin, unicode, utf8)};
parse_body(_, Bin) -> {ok, Bin}.

log(Fmt, Params, Cfg) ->
    case proplists:get_value(logfun, Cfg) of
        undefined -> ok;
        F when is_function(F) -> F(Fmt, Params)
    end.

make_body("application/json" ++ _ = CT, Params, _Path) ->
    {CT, jsx:encode(Params)};
make_body("application/octet-stream" ++ _ = CT, Bin,
          _Path)
    when is_binary(Bin) ->
    {CT, Bin};
make_body("text/plain" ++ _ = CT, Bin, _Path)
    when is_binary(Bin); is_list(Bin) ->
    {CT, Bin};
make_body("application/x-www-form-urlencoded" ++ _ = CT,
          _Params, Path) ->
    {CT, http_uri:encode(Path)};
make_body([{CT, _} | _], Params, Path) ->
    make_body(CT, Params, Path);
make_body([], Params, Path) ->
    if is_binary(Params) ->
           {"application/octet-stream", Params};
       is_map(Params) ->
           {"application/json", jsx:encode(Params)};
       is_list(Params) ->
           {"application/x-www-form-urlencoded",
            http_uri:encode(Path)};
       true -> {"application/x", Params}
    end;
make_body(CT, _Params, _Path) ->
    {CT, "unknown encoding: " ++ CT}.

content_type(undefined) -> undefined;
content_type(String) ->
    case string:length(String) > 0 of
        true -> hd(string:split(String, ";"));
        false -> String
    end.

get_by_similar_key(Name, KVs, Array) ->
    case Array of
        true ->
            case get_by_similar_key(Name, KVs) of
                undefined ->
                    {ArrayName, _} = string:take(Name,
                                                 "[]",
                                                 false,
                                                 trailing),
                    get_by_similar_key(ArrayName, KVs);
                Other -> Other
            end;
        _ -> get_by_similar_key(Name, KVs)
    end.

get_by_similar_key(Name, KVs) when is_list(Name) ->
    case maps:get(Name, KVs, undefined) of
        undefined ->
            case maps:get(iolist_to_binary(Name), KVs, undefined) of
                undefined ->
                    try AtomName = list_to_existing_atom(Name),
                        {AtomName, maps:get(AtomName, KVs)}
                    catch
                        _:_ -> undefined
                    end;
                BinValue -> {iolist_to_binary(Name), BinValue}
            end;
        Value -> {Name, Value}
    end.

get_matching(Key, KVs) when is_list(KVs) ->
    case {Key, KVs} of
        {undefined, []} -> undefined;
        {undefined, [{_, V} | _]} -> V;
        _ ->
            proplists:get_value(string:to_lower(Key), KVs, no_match)
    end.

is_matching(_ContentType1, _ContentType2) -> true.

to_str(Str) ->
    if is_list(Str) ->
           binary_to_list(iolist_to_binary(Str));
       is_binary(Str) -> binary_to_list(Str);
       true -> lists:concat([Str])
    end.
