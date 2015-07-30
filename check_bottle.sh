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

VERSION_STRING=`brew info ${ORG_NAME}/${REPO_NAME}/${FORMULA_NAME} 2>&1 | head -n 1 | cut -d ' ' -f 3`
echo ${VERSION_STRING}

BOTTLE_FILE_YOSEMITE=${FORMULA_NAME}-${VERSION_STRING}.yosemite.bottle.tar.gz
wget https://github.com/${ORG_NAME}/homebrew-${REPO_NAME}/blob/gh-pages/${BOTTLE_FILE_YOSEMITE}?raw=true -O ${BOTTLE_FILE_YOSEMITE}
YOSEMITE_FILE_HASH=`shasum ${BOTTLE_FILE_YOSEMITE} | cut -d ' ' -f 1`
YOSEMITE_FORMULA_HASH=`grep "sha1" /usr/local/Library/Taps/${ORG_NAME}/homebrew-${REPO_NAME}/${FORMULA_FILE} | grep "yosemite" | cut -d "'" -f 2`

echo "yosemite  hash : ${YOSEMITE_FILE_HASH} -- ${YOSEMITE_FORMULA_HASH}"

if [ "${YOSEMITE_FILE_HASH}" == "$YOSEMITE_FORMULA_HASH" ] ; then
    exit 0
fi
exit 1

