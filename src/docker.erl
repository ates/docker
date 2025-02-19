-module(docker).

-export([g/1]).
-export([g/2]).
-export([p/1]).
-export([p/2]).
-export([p/3]).
-export([p/4]).
-export([put/1]).
-export([put/2]).
-export([put/3]).
-export([d/1]).
-export([d/2]).
-export([login/1]).
-export([logout/0]).

g(Req) ->
    g(Req, 5000).

g(Req, Timeout) ->
    make_req(get, Req, [], Timeout).

p(Req) ->
    p(Req, #{}, 5000).

p(Req, Data) ->
    p(Req, Data, 5000).

p(Req, Data, Timeout) ->
    p(Req, Data, false, Timeout).

p(Req, Data, StreamFun, Timeout) ->
    make_req(post, Req, Data, StreamFun, Timeout).

put(Req) ->
    put(Req, #{}, 5000).

put(Req, Data) ->
    put(Req, Data, 5000).

put(Req, Data, Timeout) ->
    make_req(put, Req, Data, Timeout).

d(Req) ->
    d(Req, 5000).

d(Req, Timeout) ->
    make_req(delete, Req, [], Timeout).

%% base64url encoded token
login(Token) when is_binary(Token) ->
    persistent_term:put({?MODULE, auth}, Token).

logout() ->
    persistent_term:erase({?MODULE, auth}).

make_req(Method, URI, Data, Timeout) ->
    make_req(Method, URI, Data, false, Timeout).

make_req(Method, URI, Data, StreamFun, Timeout) ->
    case gun:open_unix(socket_path(), #{http_opts => #{keepalive => infinity}}) of
        {ok, Pid} ->
            StreamRef = send_req(Method, Pid, format_uri(URI), Data),
            Response = wait_response(Pid, StreamRef, undefined, <<>>, StreamFun, Timeout),
            gun:close(Pid),
            Response;
        Error -> Error
    end.

send_req(get, Pid, URI, _Data) ->
    gun:get(Pid, URI, auth_header());
send_req(delete, Pid, URI, _Data) ->
    gun:delete(Pid, URI, auth_header());
send_req(Method, Pid, URI, Data) when Method =:= post; Method =:= put ->
    {Headers, NewData} = prepare_headers_and_data(Data),
    gun:Method(Pid, URI, auth_header() ++ Headers, NewData).

auth_header() ->
    case persistent_term:get({?MODULE, auth}, undefined) of
        undefined -> [];
        Secret -> [{<<"X-Registry-Auth">>, Secret}]
    end.

prepare_headers_and_data(Data) ->
    case Data of
        {ContentType, Binary} when is_binary(Binary) ->
            {[{<<"content-type">>, ContentType}], Binary};
        Data when is_binary(Data) ->
            {[{<<"content-type">>, <<"application/octet-steam">>}], Data};
        Data when is_map(Data) ->
            {[{<<"content-type">>, <<"application/json">>}], jsx:encode(Data)}
    end.

socket_path() ->
    application:get_env(?MODULE, socket, <<"/var/run/docker.sock">>).

wait_response(Pid, StreamRef, InitStatus, Acc, StreamFun, Timeout) ->
    case gun:await(Pid, StreamRef, Timeout) of
        {response, nofin, Status, _Headers} ->
            wait_response(Pid, StreamRef, Status, Acc, StreamFun, Timeout);
        {response, fin, Status, _Headers} ->
            {ok, Status, Acc};
        {data, nofin, Data} ->
            case is_function(StreamFun) of
                true ->
                    StreamFun(Data),
                    wait_response(Pid, StreamRef, InitStatus, Acc, StreamFun, Timeout);
                false ->
                    wait_response(Pid, StreamRef, InitStatus, <<Acc/binary, Data/binary>>, StreamFun, Timeout)
            end;
        {data, fin, Data} ->
            case is_function(StreamFun) of
                true ->
                    {ok, InitStatus, #{}};
                false ->
                    {ok, InitStatus, from_json(<<Acc/binary, Data/binary>>)}
            end;
        Error -> Error
    end.

from_json(Data) ->
    try
        jsx:decode(Data)
    catch
        _:_Reason:_Stack ->
            Data
    end.

format_uri(URI) when is_list(URI) ->
    format_uri(list_to_binary(URI));
format_uri(URI) when is_binary(URI) ->
    <<(api())/binary, URI/binary>>;
format_uri({URI, QS}) ->
    <<(api())/binary, URI/binary, "?", (cow_qs:qs(QS))/binary>>.

api() -> application:get_env(docker, version, <<"/v1.43">>).
