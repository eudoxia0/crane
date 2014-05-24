#!/bin/bash

sudo apt-get install -y git

if [[ ! -d crane ]]; then
    git clone https://github.com/eudoxia0/crane.git
fi

cd crane/t/provision
bash apt.sh
bash quicklisp.sh
