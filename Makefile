define USAGE
Available commands

	- setup: Setup dev env

	- up: Bring up db and start server in foreground

	- test: Hit the server with a whole bunch of traffic

	- down: Tear down db

	- help: This message

endef
export USAGE

help:
	@echo "$$USAGE"
.PHONY: help

setup:
	stack build --pedantic
	stack install dotenv
.PHONY: setup

up: setup
	docker-compose up -d --build
	stack exec -- dotenv "postgres-wire-issue25 +RTS -A32m -N2"
.PHONY: up

test:
	ab -c 24 -n 4096 http://0.0.0.0:7041/updates?queries=20
.PHONY: test

down:
	docker-compose down --volumes
.PHONY: down
