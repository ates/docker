# docker - Erlang Docker Low Level Interface

## Usage

### Pull image

```erlang
1> docker:p({<<"/images/create">>, [{<<"fromImage">>, <<"alpine">>}, {<<"tag">>, <<"latest">>}]}).

{ok,200,
    <<"{\"status\":\"Pulling from library/alpine\",\"id\":\"latest\"}\r\n{\"status\":\"Pulling fs layer\",\"progressDetail\":{}"...>>}
```

### List images

```erlang
1> docker:g(<<"/images/json">>).
{ok,200,
    [#{<<"Containers">> => -1,<<"Created">> => 1577215212,
       <<"Id">> =>
           <<"sha256:cc0abc535e36a7ede71978ba2bbd8159b8a5420b91f2fbc520cdf5f673640a34">>,
       <<"Labels">> => null,<<"ParentId">> => <<>>,
       <<"RepoDigests">> =>
           [<<"alpine@sha256:2171658620155679240babee0a7714f6509fae66898db422ad803b951257db78">>],
       <<"RepoTags">> => [<<"alpine:latest">>],
       <<"SharedSize">> => -1,<<"Size">> => 5591300,
       <<"VirtualSize">> => 5591300}]}
```

### Remove image

```erlang
1> docker:d(<<"/images/cc0abc535e36a7">>).
{ok,200,
    [#{<<"Untagged">> => <<"alpine:latest">>},
     #{<<"Untagged">> =>
           <<"alpine@sha256:2171658620155679240babee0a7714f6509fae66898db422ad803b951257db78">>},
     #{<<"Deleted">> =>
           <<"sha256:cc0abc535e36a7ede71978ba2bbd8159b8a5420b91f2fbc520cdf5f673640a34">>},
     #{<<"Deleted">> =>
           <<"sha256:6b27de954cca6332272e7709b7d8ceccee1489d9452af73391df360a26123580">>}]}
```

### Create container

```erlang
1> docker:p({<<"/containers/create">>, [{<<"name">>, <<"foo">>}]}, #{<<"Image">> => <<"alpine:latest">>}).
{ok,201,
    #{<<"Id">> =>
          <<"fa4059a51fb5ebeb1c4e488f337698b61d19a01d276980f5cc977e36bea8080f">>,
      <<"Warnings">> => []}}
```

### List all containers

```erlang
1> docker:g({<<"/containers/json">>, [{<<"all">>, <<"true">>}]}).
{ok,200,
    [#{<<"Command">> => <<"/bin/sh">>,<<"Created">> => 1579290977,
       <<"HostConfig">> => #{<<"NetworkMode">> => <<"default">>},
       <<"Id">> =>
           <<"fa4059a51fb5ebeb1c4e488f337698b61d19a01d276980f5cc977e36bea8080f">>,
       <<"Image">> => <<"alpine:latest">>,
       <<"ImageID">> =>
           <<"sha256:cc0abc535e36a7ede71978ba2bbd8159b8a5420b91f2fbc520cdf5f673640a34">>,
       <<"Labels">> => #{},<<"Mounts">> => [],
       <<"Names">> => [<<"/foo">>],
       <<"NetworkSettings">> =>
           #{<<"Networks">> =>
                 #{<<"bridge">> =>
                       #{<<"Aliases">> => null,<<"DriverOpts">> => null,
                         <<"EndpointID">> => <<>>,<<"Gateway">> => <<>>,
                         <<"GlobalIPv6Address">> => <<>>,
                         <<"GlobalIPv6PrefixLen">> => 0,<<"IPAMConfig">> => null,
                         <<"IPAddress">> => <<>>,<<"IPPrefixLen">> => 0,
                         <<"IPv6Gateway">> => <<>>,<<"Links">> => null,
                         <<"MacAddress">> => <<>>,<<"NetworkID">> => <<>>}}},
       <<"Ports">> => [],<<"State">> => <<"created">>,
       <<"Status">> => <<"Created">>}]}
```

### Start container

```erlang
docker:p(<<"/containers/fa4059a51fb5ebeb/start">>).
{ok,204,<<>>}
```
