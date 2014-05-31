#!/bin/bash

### Install dependencies
export DEBIAN_FRONTEND=noninteractive # So MySQL won't prompt me for a password
apt-get update

## Lisp
apt-get install -y sbcl clisp curl

## Databases

# Postgres
apt-get install -y postgresql

# MySQL
apt-get install -y mysql-server-5.5 mysql-server mysql-client

# SQLite
apt-get install -y sqlite3

apt-get -y -f install

### Common setup
exec crane/t/provision/common.sh
