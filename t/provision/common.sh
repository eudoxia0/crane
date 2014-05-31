#!/bin/bash

### Install Quicklisp

QLFILE=/tmp/ql.lisp
QLURL=http://beta.quicklisp.org/quicklisp.lisp

if [[ ! -f ~/quicklisp/setup.lisp ]]; then
    curl -o $QLFILE $QLURL
    sbcl --noinform --noprint --no-sysinit --no-userinit \
         --load $QLFILE \
         --eval "(quicklisp-quickstart:install)" \
         --quit
fi

### Add configuration files

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
  (:directory "/home/vagrant/crane/")
  :inherit-configuration)
EOF

echo $LISP_INIT > ~/.sbclrc
mkdir -p ~/.config/common-lisp
echo $SOURCE_REGISTRY > ~/.config/common-lisp/source-registry.conf
