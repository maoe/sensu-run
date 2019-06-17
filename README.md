# sensu-run
[![Hackage](https://img.shields.io/hackage/v/sensu-run.svg)](http://hackage.haskell.org/package/sensu-run)
[![Hackage-Deps](https://img.shields.io/hackage-deps/v/sensu-run.svg)](http://packdeps.haskellers.com/feed?needle=sensu-run)
[![Build Status](https://travis-ci.org/maoe/sensu-run.svg?branch=master)](https://travis-ci.org/maoe/sensu-run)
[![Build status](https://ci.appveyor.com/api/projects/status/k9594kkn4tncotqt/branch/master?svg=true)](https://ci.appveyor.com/project/maoe/sensu-run/branch/master)

`sensu-run` runs a command and send its result to Sensu server using the [client socket input](https://sensuapp.org/docs/latest/reference/clients.html#client-socket-input) or via the Sensu API. It is useful to monitor cron jobs for example.

## Installation

NOTE: Currently sensu-run doesn't work on Windows. See [#16](https://github.com/maoe/sensu-run/issues/16).

Binary releases are available at [GitHub Releases](https://github.com/maoe/sensu-run/releases). Currently supported platforms for the binary releases are:

* Ubuntu (64bit)
* macOS
* Windows (x64, x86)

You can also build it yourself using [stack](https://docs.haskellstack.org/en/stable/README/):
```sh
stack install sensu-run
```
will install the `sensu-run` command in `~/.local/bin`.

## Usage

```console
% sensu-run --help
Usage: sensu-run ([-n|--name NAME] [--source SOURCE] [--ttl SECONDS]
                 [--timeout SECONDS] [--handler HANDLER] ([--port PORT] |
                 [--server URL]) [--redirect] [--no-lock] [--dry|--dry-run]
                 [-s|--shell] [COMMAND] | [-v|--version])

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
  --redirect               Redirect command output to sensu-run's output
  --no-lock                Do not create a lock file to allow multiple instances
                           to run
  --dry,--dry-run          Dump the JSON object which is supposed to be sent
  -s,--shell               Execute the command using the shell
```

`--dry-run` option is useful to check the JSON output:

```console
% sensu-run --name check-home-src-size --handler foo --dry-run -- du -s $HOME/src | jq .
{
  "name": "check-home-src-size",
  "command": "du -s /home/maoe/src",
  "issued": 1501041549,
  "executed": 1501041549,
  "duration": 1.674895,
  "status": 0,
  "output": "55513524\t/home/maoe/src\n",
  "handlers": [
    "foo"
  ],
  "user": "maoe"
}
```

Use the `--shell` option if you want to use shell functions:
```console
% sensu-run --name check-home-src-size --handler foo --dry-run --shell -- "cd $HOME/src; du -s ." | jq .
{
  "name": "check-home-src-size",
  "command": "cd /home/maoe/src; du -s .",
  "issued": 1501041568,
  "executed": 1501041568,
  "duration": 1.224157,
  "status": 0,
  "output": "55513524\t.\n",
  "handlers": [
    "foo"
  ],
  "user": "maoe"
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

## Handling signals on UNIX systems

`sensu-run` traps the following signals and resends them to the monitored process:

* SIGHUP
* SIGINT
* SIGQUIT
* SIGTERM
