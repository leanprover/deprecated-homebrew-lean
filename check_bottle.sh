#!/usr/bin/env bash
#---------------------------------------------------
ORG_NAME=leanprover
REPO_NAME=lean
GIT_REMOTE_REPO=git@github.com:${ORG_NAME}/${REPO_NAME}
FORMULA_NAME=lean
FORMULA_FILE=${FORMULA_NAME}.rb
FORMULA_TEMPLATE=${FORMULA_NAME}.rb.template
OSX_VERSION=`sw_vers -productVersion`
#---------------------------------------------------

VERSION_STRING=`brew info ${FORMULA_NAME} 2>&1 | head -n 1 | cut -d ' ' -f 3`
echo ${VERSION_STRING}

BOTTLE_FILE_YOSEMITE=${FORMULA_NAME}-${VERSION_STRING}.yosemite.bottle.tar.gz
BOTTLE_FILE_MAVERICKS=${FORMULA_NAME}-${VERSION_STRING}.mavericks.bottle.tar.gz
wget https://github.com/${ORG_NAME}/homebrew-${REPO_NAME}/blob/mavericks/${BOTTLE_FILE_MAVERICKS}?raw=true -O ${BOTTLE_FILE_MAVERICKS}
wget https://github.com/${ORG_NAME}/homebrew-${REPO_NAME}/blob/yosemite/${BOTTLE_FILE_YOSEMITE}?raw=true -O ${BOTTLE_FILE_YOSEMITE}
MAVERICKS_FILE_HASH=`shasum ${BOTTLE_FILE_MAVERICKS} | cut -d ' ' -f 1`
YOSEMITE_FILE_HASH=`shasum ${BOTTLE_FILE_YOSEMITE} | cut -d ' ' -f 1`

MAVERICKS_FORMULA_HASH=`grep "sha1" /usr/local/Library/Formula/${FORMULA_FILE} | grep "mavericks" | cut -d "'" -f 2`
YOSEMITE_FORMULA_HASH=`grep "sha1" /usr/local/Library/Formula/${FORMULA_FILE} | grep "yosemite" | cut -d "'" -f 2`

echo "mavericks hash : ${MAVERICKS_FILE_HASH} -- ${MAVERICKS_FORMULA_HASH}"
echo "yosemite  hash : ${YOSEMITE_FILE_HASH} -- ${YOSEMITE_FORMULA_HASH}"

if [ "${MAVERICKS_FILE_HASH}" == "$MAVERICKS_FORMULA_HASH" ] ; then
    if [ "${YOSEMITE_FILE_HASH}" == "$YOSEMITE_FORMULA_HASH" ] ; then
        exit 0
    fi
fi
exit 1

