-module(docker).

-export([g/1]).
-export([p/2]).
-export([d/1]).

g(Req) ->
    make_req(get, Req, []).

p(Req, Data) ->
    make_req(post, Req, Data).

d(Req) ->
    make_req(delete, Req, []).

make_req(Method, URI, Data) ->
    case gun:open_unix(socket_path(), #{http_opts => #{keepalive => infinity}}) of
        {ok, Pid} ->
            StreamRef = send_req(Method, Pid, <<(api())/binary, URI/binary>>, Data),
            Response = wait_response(Pid, StreamRef, undefined, <<>>),
            gun:close(Pid),
            Response;
        Error -> Error
    end.

send_req(get, Pid, URI, _Data) ->
    gun:get(Pid, URI);
send_req(post, Pid, URI, Data) ->
    gun:post(Pid, URI, [{<<"content-type">>, <<"application/json">>}], to_json(Data));
send_req(delete, Pid, URI, _Data) ->
    gun:delete(Pid, URI).

socket_path() ->
    application:get_env(?MODULE, socket, <<"/var/run/docker.sock">>).

wait_response(Pid, StreamRef, InitStatus, Acc) ->
    case gun:await(Pid, StreamRef) of
        {response, nofin, Status, _Headers} ->
            wait_response(Pid, StreamRef, Status, Acc);
        {response, fin, Status, _Headers} ->
            {ok, Status, Acc};
        {data, nofin, Data} ->
            wait_response(Pid, StreamRef, InitStatus, <<Acc/binary, Data/binary>>);
        {data, fin, Data} ->
            {ok, InitStatus, from_json(<<Acc/binary, Data/binary>>)};
        Error -> Error
    end.

from_json(Data) ->
    try
        jsx:decode(Data, [return_maps])
    catch
        _:_Reason:_Stack ->
            Data
    end.

to_json(Data) when is_map(Data) -> jsx:encode(Data);
to_json(Data) when is_binary(Data) -> cow_uri:urlencode(Data).

api() -> application:get_env(docker, version, <<"/v1.37">>).
