#!/bin/bash -e

JAVA_OPTS="-Dsbt.log.noformat=true -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=512m -Xmx1024M -Xss256M"

if [ "$1" == "--debug" ]; then
    JAVA_OPTS+=" -Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=5005"
    shift
fi

java $JAVA_OPTS -jar `dirname $0`/sbt-launch-0.13.1.jar "$@"
