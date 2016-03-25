-module('onesignal').

%% API exports
-export([create_player/3, create_player/4]).
-export([send_notification/1]).

-define(API_BASE, <<"https://onesignal.com/api/v1">>).

%%====================================================================
%% API functions
%%====================================================================

create_player(AppId, DeviceType, Identifier) ->
    create_player(AppId, DeviceType, Identifier, #{}).

create_player(AppId, DeviceType, Identifier, Params) when is_map(Params) ->
    Params2 = Params#{device_type => device_type_to_code(DeviceType),
                      app_id => AppId,
                      identifier => Identifier
                     },
    create_player(Params2).

create_player(Params) when is_map(Params) ->
    case request(post, <<"players">>, Params) of
        {ok, 200, _RespHeaders, Ref} ->
            JSON = hackney:body(Ref),
            {ok, jsx:decode(JSON, [return_maps])};
        {ok, StatusCode, _RespHeaders, _Ref} ->
            {error, StatusCode}
    end.

send_notification(Params) ->
    case request(post, <<"/notifications">>, Params) of
        {ok, 200, _RespHeaders, _Ref} ->
            ok;
        {ok, StatusCode, _RespHeaders, _Ref} ->
            {error, StatusCode}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

request(Method, Path, Params) ->
    URL = <<?API_BASE/binary, Path/binary>>,
    JSON = jsx:encode(Params),
    hackney:request(Method, URL, headers(), JSON).

headers() ->
    [{<<"content-type">>, <<"application/json">>}].

%% headers(ApiKey) when is_list(ApiKey) ->
%%     headers(list_to_binary(ApiKey));
%% headers(ApiKey) when is_binary(ApiKey) ->
%%     [{<<"Authorization">>, <<"Basic ", ApiKey/binary>>}].

%% prepare_identifier(Identifier) when is_binary(Identifier) ->
%%     binary:replace(Identifier, <<"-">>, <<"">>, [global]).

device_type_to_code(ios) -> 0;
device_type_to_code(android) -> 1;
device_type_to_code(amazon) -> 2;
device_type_to_code(windowsphone_mpns) -> 3;
device_type_to_code(chrome_app) -> 4;
device_type_to_code(chrome_website) -> 5;
device_type_to_code(windowsphone_wns) -> 6;
device_type_to_code(safari) -> 7;
device_type_to_code(firefox) -> 8.
