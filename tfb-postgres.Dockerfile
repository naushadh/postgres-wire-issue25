FROM postgres:9.5-alpine

COPY seed.sql /docker-entrypoint-initdb.d/
