#!/usr/bin/env bash

if [ $# -lt 1 ]
then
  cat << EOF
Usage ./run_tests.sh <test_suite_name>

<test_suite_name> is one of:
  - java
  - chap04_scanning
  - chap06_parsing
  - chap07_evaluating
  - chap08_statements
  - chap09_control
  - chap10_functions
  - chap11_resolving
  - chap12_classes
  - chap13_inheritance

* Note: test suites bellow chap08_statements
may fail with the finished interpreter.
EOF
else
  dart tool/bin/test.dart $1 --interpreter ./sclox
fi
