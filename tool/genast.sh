#!/usr/bin/env bash

if [ $# -lt 1 ]; then
    cat << EOF
Usage: bootstrap.sh <src>
    src: Path to the src directory
EOF

exit 1
fi

javac "$1"/fighur/tool/GenerateAst.java
java -cp "$1" fighur.tool.GenerateAst "$1"/fighur/jlox