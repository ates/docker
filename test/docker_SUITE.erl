-module(docker_SUITE).

%% ct callbacks
-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).

%% tests: common
-export([ping/1]).

%% tests: container
-export([container_archive_get/1]).
-export([container_archive_put/1]).

-define(CONTAINER_NAME, <<"docker_test_ubuntu">>).
-define(IMAGE, <<"ubuntu:24.04">>).

all() ->
    [
        {group, common},
        {group, container}
    ].

groups() ->
    [
        {common, [], [ping]},
        {container, [sequence], [
            container_archive_get,
            container_archive_put
        ]}
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(docker),
    {ok, 200, _} = docker:p({<<"/images/create">>, [{<<"fromImage">>, ?IMAGE}]}, #{}, timer:seconds(1800)),
    Config.

end_per_suite(Config) ->
    application:stop(docker),
    Config.

init_per_group(container, Config) ->
    {ok, _Id} = container_create(),
    ok = container_start(),
    Config;

init_per_group(_Group, Config) -> Config.

end_per_group(container, _Config) ->
    container_stop(),
    container_remove();

end_per_group(_Group, _Config) -> ok.

ping(_Config) ->
    {ok, 200, <<"OK">>} = docker:g(<<"/_ping">>).

container_archive_get(_Config) ->
    {ok, 200, Tar} = docker:g({<<"/containers/", ?CONTAINER_NAME/binary, "/archive">>, [{<<"path">>, <<"/etc/passwd">>}]}),
    {ok, [{"passwd", <<"root", _/binary>>}] = Files} = erl_tar:extract({binary, Tar}, [memory]),
    ct:pal("Files: ~p", [Files]).

container_archive_put(_Config) ->
    ok = erl_tar:create("/tmp/docker_test.tar", ["/etc/passwd"]),
    {ok, Data} = file:read_file("/tmp/docker_test.tar"),
    {ok, 200, <<>>} = docker:put({<<"/containers/", ?CONTAINER_NAME/binary, "/archive">>, [{<<"path">>, <<"/tmp">>}]}, Data).

container_create() ->
    Data = #{
        <<"Image">>       => ?IMAGE,
        <<"AttachStdin">> => true,
        <<"Tty">>         => true,
        <<"OpenStdin">>   => true,
        <<"StdinOnce">>   => true
    },
    {ok, 201, #{<<"Id">> := Id}} = docker:p({<<"/containers/create">>, [{<<"name">>, ?CONTAINER_NAME}]}, Data),
    {ok, Id}.

container_start() ->
    {ok, 204, <<>>} = docker:p(<<"/containers/", ?CONTAINER_NAME/binary, "/start">>),
    ok.

container_stop() ->
    %% t=0: no grace period, kill immediately; /stop blocks until container exits
    {ok, 204, <<>>} = docker:p({<<"/containers/", ?CONTAINER_NAME/binary, "/stop">>, [{<<"t">>, <<"0">>}]}, #{}, timer:seconds(15)).

container_remove() ->
    {ok, 204, <<>>} = docker:d(<<"/containers/", ?CONTAINER_NAME/binary>>).
