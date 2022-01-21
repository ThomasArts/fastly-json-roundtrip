%%% @author Thomas Arts <thomas@SpaceGrey.local>
%%% @doc Test fastly part of json parsing
%%%
%%% @end
%%% Created : 21 Jan 2022 by Thomas Arts <thomas@SpaceGrey.local>

-module(json_eqc).

-include_lib("eqc/include/eqc.hrl").

-compile([export_all, nowarn_export_all]).

prop_round_trip() ->
    ?FORALL(Json, gen_json(),
            ?IMPLIES(not (is_float(Json) orelse is_integer(Json)),
            begin
                Body = encode(Json),
                Request = api:request(postJson, #{body => Body}, []),
                {ok, 200, Headers, RetBody} = api:http_request(Request, [], [], []),
                ?WHENFAIL(eqc:format("Response: ~p~nBody: ~p~n", [Headers, RetBody]),
                          equals(decode(RetBody), Json))
            end)).


%% generator and helper functions

gen_json() ->
    ?LET(Words, eqc_erlang_program:words(20),
         ?SIZED(Size, gen_json(Size, [ iolist_to_binary(Str) || Str <- Words] ++ [<<"Give me a break, please">>]))).

gen_json(0, Strings) ->
    oneof([int(), %% real(),
           true, false, null,
           elements(Strings)]);
gen_json(N, Strings) ->
    Smaller = gen_json(N div 4, Strings),
    ?LAZY(oneof([gen_json(0, Strings),
                 ?LET(Array, list(Smaller), ?SHRINK(Array, Array)),
                 ?LETSHRINK([Key, T, T1], [elements(Strings), Smaller, Smaller],
                            case is_map(T1) of
                                true ->
                                    T1#{Key => T};
                                false ->
                                    oneof([#{Key => T}, #{Key => [T, T1]}])
                            end)
                ])).

encode(Json) ->
    if is_integer(Json) -> Json;
       is_float(Json) -> Json;
       true -> jsx:encode(Json)
    end.

decode(String) ->
    Wrap =
        iolist_to_binary(["{\"wrap\": ", String, "}"]),
    #{<<"wrap">> := Body} = jsx:decode(Wrap, [return_maps]),
    Body.
