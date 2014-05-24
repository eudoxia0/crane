#!/bin/bash

### Download the sources

sudo apt-get install -y git

if [[ ! -d crane ]]; then
    git clone https://github.com/eudoxia0/crane.git
fi

cd crane/t/provision

### Install dependencies

## Lisp
sudo apt-get install -y sbcl clisp

## Databases

# Postgres
sudo apt-get install -y postgresql

# MySQL
sudo apt-get install -y mysql-server mysql-client

# SQLite
sudo apt-get install -y sqlite3

### Install Quicklisp
bash quicklisp.sh
