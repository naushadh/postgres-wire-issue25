# postgres-wire-issue25

This is a sample project that reproduces `postgres-wire` [issue #25](https://github.com/postgres-haskell/postgres-wire/issues/25).

## Setup

### Database

The supplied [docker-compose file](./docker-compose.yml) can bring up the test database with data seeded using docker+compose.

- Install [docker](https://www.docker.com/get-started)
- Install [docker-compose](https://docs.docker.com/compose/install/)

Alternatively, you can run the supplied [seed file](./seed.sql) in your pre-existing database before updating the `.env` file to make the application connect there.

### Server

The issue materializes when trying to concurrently update many rows in a loop. It was first discovered in a Web framework benchmarking suite. As such we've lifted and compacted the test warp application down to the problematic bits.

- Install [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)
- Start server

  ```bash
  $ make up
  # Brings up db and starts app server in foreground
  ```

## Test

Simply run

```bash
$ make test
# hammers the app server with traffic pattern that should materialize the issue
```

NOTE: it's possible your hardware/setup may require different concurrency levels or request counts to reproduce the issue.
