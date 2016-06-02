-module('onesignal').

%% API exports
-export([create_player/1, create_player/2]).
-export([send_notification/2]).

-define(API_BASE, <<"https://onesignal.com/api/v1">>).

%%====================================================================
%% API functions
%%====================================================================

create_player(DeviceType, Params) when is_binary(DeviceType), is_map(Params) ->
    Params2 = Params#{device_type => device_type_to_code(DeviceType)},
    create_player(Params2).

create_player(Params) when is_map(Params) ->
    case request(post, <<"/players">>, Params) of
        {ok, 200, _RespHeaders, Ref} ->
            {ok, JSON} = hackney:body(Ref),
            {ok, jsx:decode(JSON, [return_maps])};
        {ok, StatusCode, _RespHeaders, Ref} ->
            {ok, ErrorBody} = hackney:body(Ref),
            {error, StatusCode, ErrorBody}
    end.

send_notification(ApiKey, Params) ->
    case request(post, <<"/notifications">>, ApiKey, Params) of
        {ok, 200, _RespHeaders, Ref} ->
            {ok, JSON} = hackney:body(Ref),
            {ok, jsx:decode(JSON, [return_maps])};
        {ok, StatusCode, _RespHeaders, Ref} ->
            {ok, ErrorBody} = hackney:body(Ref),
            {error, StatusCode, ErrorBody}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

request(Method, Path, Params) ->
    URL = <<?API_BASE/binary, Path/binary>>,
    JSON = jsx:encode(Params),
    hackney:request(Method, URL, headers(), JSON).

request(Method, Path, ApiKey, Params) ->
    URL = <<?API_BASE/binary, Path/binary>>,
    JSON = jsx:encode(Params),
    hackney:request(Method, URL, headers(ApiKey), JSON).

headers() ->
    [{<<"content-type">>, <<"application/json">>}].

headers(ApiKey) when is_list(ApiKey) ->
    headers(list_to_binary(ApiKey));
headers(ApiKey) when is_binary(ApiKey) ->
    [{<<"Authorization">>, <<"Basic ", ApiKey/binary>>} | headers()].

device_type_to_code(<<"ios">>) -> 0;
device_type_to_code(<<"android">>) -> 1;
device_type_to_code(<<"amazon">>) -> 2;
device_type_to_code(<<"windowsphone_mpns">>) -> 3;
device_type_to_code(<<"chrome_app">>) -> 4;
device_type_to_code(<<"chrome_website">>) -> 5;
device_type_to_code(<<"windowsphone_wns">>) -> 6;
device_type_to_code(<<"safari">>) -> 7;
device_type_to_code(<<"firefox">>) -> 8.
