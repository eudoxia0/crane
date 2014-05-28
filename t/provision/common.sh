#!/bin/bash

### Install Quicklisp

QLFILE=/tmp/ql.lisp
QLURL=http://beta.quicklisp.org/quicklisp.lisp

if [[ ! -f ~/quicklisp/setup.lisp ]]; then
    echo "Installing Quicklisp"
    curl -o $QLFILE $QLURL
    sbcl --no-sysinit --no-userinit --noprint --noinform \
         --load $QLFILE \
         --eval '(quicklisp-quickstart:install)' \
         --quit
fi

### Add configuration files
echo "Copying configuration files"

read -d '' LISP_INIT <<"EOF"
(setf *print-case* :downcase)

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
EOF

read -d '' SOURCE_REGISTRY <<"EOF"
(:source-registry
    (:tree (:home "code"))
    :inherit-configuration)
EOF

echo $LISP_INIT > ~/.sbclrc
echo $SOURCE_REGISTRY > ~/.config/common-lisp/source-registry.conf

### Set up databases
echo "Setting up databases"

## Postgres

sudo -u postgres createdb crane_test_db
sudo -u postgres psql -c \
    "CREATE USER crane_test_user WITH PASSWORD 'crane_test_user'"
sudo -u postgres psql -c \
    "GRANT ALL PRIVILEGES ON DATABASE crane_test_db TO crane_test_user"

## MySQL

### Run the tests
echo "Running tests"
sbcl --eval '(ql:quickload :crane-test)' --quit

### Tear down
sudo -u postgres dropdb crane_test_db
