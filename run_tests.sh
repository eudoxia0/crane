#!/bin/bash

function test() {
    vagrant up $1
    vagrant ssh $1 -c "sudo su -c 'bash crane/t/provision/test.sh'"
    #vagrant halt $1
}

test ubuntu
