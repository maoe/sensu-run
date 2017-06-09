# sensu-run
[![Hackage](https://img.shields.io/hackage/v/sensu-run.svg)](http://hackage.haskell.org/package/sensu-run)
[![Hackage-Deps](https://img.shields.io/hackage-deps/v/sensu-run.svg)](http://packdeps.haskellers.com/feed?needle=sensu-run)
[![Stackage Nightly](http://stackage.org/package/sensu-run/badge/nightly)](http://stackage.org/nightly/package/sensu-run)
[![Build Status](https://travis-ci.org/maoe/sensu-run.svg?branch=master)](https://travis-ci.org/maoe/sensu-run)
[![Build status](https://ci.appveyor.com/api/projects/status/k9594kkn4tncotqt/branch/master?svg=true)](https://ci.appveyor.com/project/maoe/sensu-run/branch/master)

`sensu-run` runs a command and send its result to Sensu server using the [client socket input](https://sensuapp.org/docs/latest/reference/clients.html#client-socket-input) or via the Sensu API. It is useful to monitor cron jobs for example.

## Installation

Install [stack](https://docs.haskellstack.org/en/stable/README/).

```sh
stack install --resolver=nightly sensu-run
```
will install the `sensu-run` command in `~/.local/bin`.

## Usage

```console
% sensu-run --help
Usage: sensu-run (-n|--name NAME) [--source SOURCE] [--ttl SECONDS]
                 [--timeout SECONDS] --handler HANDLER ([--port PORT] |
                 [--server URL]) [--dry|--dry-run] [-s|--shell] COMMAND

Available options:
  -h,--help                Show this help text
  -n,--name NAME           The name of the check
  --source SOURCE          The check source, used to create a JIT Sensu client
                           for an external resource
  --ttl SECONDS            The time to live in seconds until check results are
                           considered stale
  --timeout SECONDS        The check executaion duration timeout in seconds
  --handler HANDLER        Sensu event handler(s) to use for events created by
                           the check
  --port PORT              Send results to the local sensu-client listening on
                           the specified port (default: 3030)
  --server URL             Send results to the specified Sensu server
  -s,--shell               Execute the command using the shell
```

`--dry-run` option is useful to check the JSON output:

```console
% sensu-run --name check-home-src-size --handler foo --dry-run -- du -s $HOME/src | jq .
{
  "name": "check-home-src-size",
  "command": "du -s /home/maoe/src",
  "issued": 1496966954,
  "executed": 1496966954,
  "duration": 1.235584,
  "status": 0,
  "output": "44567740\t/home/maoe/src\n",
  "handlers": [
    "foo"
  ]
}
```

Without the `--dry-run` option, `sensu-run` sends the output to localhost:PORT, which is expected to be listened by `sensu-client`.

`sensu-run` sets the status field depending on the command exit code and timeout:

| command exit code | `status` field |
|-------------------|----------------|
| 0                 | 0 (OK)         |
| non-zero          | 2 (CRITICAL)   |
| command timed out | 3 (UNKNOWN)    |

### Sensu API

`sensu-run` supports posting check results via Sensu API as well. Use `--server` option to specify Sensu server addresses. If multiple servers are specified, `sensu-run` tries them one by one until it succeeds.

```sh
sensu-run --name check-true --handler foo --server sensu1.example.com --server sensu2.example.com --dry-run -- du -s $HOME/src
```
