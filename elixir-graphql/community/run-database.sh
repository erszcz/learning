#!/usr/bin/env bash

pushd / > /dev/null
PROJECT_DIR=$(dirs -l -0)
popd > /dev/null

docker run --name community-postgresql \
        -e POSTGRES_USER=postgres \
        -e POSTGRES_PASSWORD=postgres \
        -p 5432:5432 \
        -v ${PROJECT_DIR}/docker/db/data:/var/lib/postgresql/data \
        -d postgres
