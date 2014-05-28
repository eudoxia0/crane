#!/bin/bash

### Install dependencies

## Lisp
sudo apt-get install -y sbcl clisp curl

## Databases

# Postgres
sudo apt-get install -y postgresql

# MySQL
sudo apt-get install -y mysql-server mysql-server-5.5 mysql-client

# SQLite
sudo apt-get install -y sqlite3

### Common setup
bash crane/t/provision/common.sh
