# sensu-run

`sensu-run` runs a command and send its result to Sensu server using the [client socket input](https://sensuapp.org/docs/latest/reference/clients.html#client-socket-input). It is useful to monitor cron jobs for example.

## Installation

Install [stack](https://docs.haskellstack.org/en/stable/README/).

```
stack install sensu-run
```
will install the `sensu-run` command in `~/.local/bin`.

## Usage

```
% sensu-run --help
Usage: sensu-run (-n|--name NAME) [--source SOURCE] [--ttl SECONDS]
                 [--timeout SECONDS] --handler HANDLER [--port PORT]
                 [--dry|--dry-run] [-s|--shell] COMMAND

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
  --port PORT              Port number that sensu-client is listening
                           on (default: 3030)
  -s,--shell               Execute the command using the shell
```

`--dry-run` option is useful to check the JSON output:

```
% sensu-run --name check-true --handler foo --dry-run -- du -s $HOME/src | jq .
{
  "name": "check-true",
  "command": "du -s /Users/maoe/src",
  "issued": 1483426629,
  "executed": 1483426629,
  "duration": 45,
  "status": 0,
  "output": "52454088\t/Users/maoe/src\n"
}
```

Without the `--dry-run` option, `sensu-run` sends the output to localhost:PORT, which is expected to be listened by `sensu-client`.

`sensu-run` sets the status field depending on the command exit code and timeout:

| command exit code | `status` field |
|-------------------|----------------|
| 0                 | 0 (OK)         |
| non-zero          | 2 (CRITICAL)   |
| command timed out | 3 (UNKNOWN)    |
