#!/bin/bash

QLFILE=/tmp/ql.lisp
QLURL=http://beta.quicklisp.org/quicklisp.lisp

if [[ ! -f $QLFILE ]]; then
    curl -o $QLFILE $QLURL
    sbcl --no-sysinit --no-userinit --load $QLFILE \
        --eval '(quicklisp-quickstart:install)' \
        --eval '(ql:add-to-init-file)' \
        --quit
fi
