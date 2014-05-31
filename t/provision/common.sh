#!/bin/bash

### Install Quicklisp

QLFILE=/tmp/ql.lisp
QLURL=http://beta.quicklisp.org/quicklisp.lisp
HOME=/home/vagrant

if [[ ! -f $HOME/quicklisp/setup.lisp ]]; then
    curl -o $QLFILE $QLURL
    sbcl --noinform --noprint --no-sysinit --no-userinit \
         --load $QLFILE \
         --eval "(quicklisp-quickstart:install :path \"$HOME/quicklisp\")" \
         --quit
fi

### Add configuration files

read -d '' LISP_INIT <<"EOF"
(setf *print-case* :downcase)

#-quicklisp
(let ((quicklisp-init (parse-namestring "/home/vagrant/quicklisp/setup.lisp")))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
EOF

read -d '' SOURCE_REGISTRY <<"EOF"
(:source-registry
    (:tree (:home "code"))
    :inherit-configuration)
EOF

echo $LISP_INIT > $HOME/.sbclrc
mkdir -p $HOME/.config/common-lisp
echo $SOURCE_REGISTRY > $HOME/.config/common-lisp/source-registry.conf

### Set up databases

## Postgres

sudo -u postgres dropdb crane_test_db
sudo -u postgres createdb crane_test_db
sudo -u postgres psql -c \
    "CREATE USER crane_test_user WITH PASSWORD 'crane_test_user'"
sudo -u postgres psql -c \
    "GRANT ALL PRIVILEGES ON DATABASE crane_test_db TO crane_test_user"

## MySQL

### Run the tests
chown -R vagrant $HOME
sbcl --eval '(ql:quickload :crane-test)' --quit

### Tear down
sudo -u postgres dropdb crane_test_db
