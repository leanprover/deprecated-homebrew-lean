#!/usr/bin/env bash
ID=$1
PASSWORD=$2
VERSION=$3
case `uname -r` in
  14.*)
    OSX_NAME="yosemite"
    ;;
  15.*)
    OSX_NAME="el_capitan"
    ;;
  *)
    OSX_NAME="unknown"
    ;;
esac

BOTTLE_FILENAME=lean-${VERSION}.${OSX_NAME}.bottle.tar.gz
BINTRAY_URL=https://api.bintray.com/content/lean/lean/lean


if [ -e ${BOTTLE_FILENAME} ]
then
  curl -T ${BOTTLE_FILENAME} -u${ID}:${PASSWORD} ${BINTRAY_URL}/${VERSION}/${BOTTLE_FILENAME}
else 
  echo "File not found: ${BOTTLE_FILENAME}"
fi
