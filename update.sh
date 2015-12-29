#!/usr/bin/env bash
#
# Usage:
#         BINTRAY_ID=<my_id> BINTRAY_PWD=<my_password> ./update.sh
#
#---------------------------------------------------
ORG_NAME=leanprover
REPO_NAME=lean
GIT_REMOTE_REPO=git@github.com:${ORG_NAME}/${REPO_NAME}
FORMULA_NAME=lean
FORMULA_FILE=${FORMULA_NAME}.rb
FORMULA_TEMPLATE=${FORMULA_NAME}.rb.template
OSX_VERSION=`sw_vers -productVersion`
PUSH_FORMULA_WITH_BOTTLE=FALSE
#---------------------------------------------------
if [ ! -d ./${REPO_NAME} ] ; then
    git clone ${GIT_REMOTE_REPO}
fi   

cd ${REPO_NAME}
git rev-parse HEAD > PREVIOUS_HASH
git fetch --all --quiet
git reset --hard origin/master --quiet
git rev-parse HEAD > CURRENT_HASH
COMMIT_DATETIME=$(date -r `git show -s --format="%ct" HEAD` +"%Y%m%d%H%M%S")
cd ..

if ! cmp ${REPO_NAME}/PREVIOUS_HASH ${REPO_NAME}/CURRENT_HASH >/dev/null 2>&1
then
    DOIT=TRUE
fi
if [[ $1 == "-f" ]] ; then
    DOIT=TRUE
fi

if [[ $DOIT == TRUE ]] ; then
    echo "===================================="
    echo "1. Update formula with a new version"
    echo "===================================="
    VERSION_MAJOR=`grep -o -i "VERSION_MAJOR \([0-9]\+\)" ${REPO_NAME}/src/CMakeLists.txt | cut -d ' ' -f 2`
    VERSION_MINOR=`grep -o -i "VERSION_MINOR \([0-9]\+\)" ${REPO_NAME}/src/CMakeLists.txt | cut -d ' ' -f 2`
    VERSION_PATCH=`grep -o -i "VERSION_PATCH \([0-9]\+\)" ${REPO_NAME}/src/CMakeLists.txt | cut -d ' ' -f 2`
    COMMIT_HASH=git`cat ${REPO_NAME}/CURRENT_HASH`
    SOURCE_VERSION=${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCH}
    VERSION_STRING=${SOURCE_VERSION}.${COMMIT_DATETIME}.${COMMIT_HASH}
    cp ${FORMULA_TEMPLATE} ${FORMULA_FILE}
    sed -i "" "s/##SOURCE_VERSION##/${SOURCE_VERSION}/g" ${FORMULA_FILE}
    sed -i "" "s/##COMMIT_HASH##/${COMMIT_HASH}/g" ${FORMULA_FILE}
    sed -i "" "s/##COMMIT_DATETIME##/${COMMIT_DATETIME}/g" ${FORMULA_FILE}
    sed -i "" "s/##ORG_NAME##/${ORG_NAME}/g" ${FORMULA_FILE}
    sed -i "" "s/##FORMULA_NAME##/${FORMULA_NAME}/g" ${FORMULA_FILE}
    echo "UPDATE FORMULA: ${VERSION_STRING}"

    echo "===================================="
    echo "2. Create a bottle"
    echo "===================================="
    brew rm -rf! ${FORMULA_NAME}
    brew install --build-bottle ./${FORMULA_NAME}.rb
    # homebrew doesn't allow to generate bottle if it's not in core formula or under a tap.
    # as a temporary solution, we copy the formula file to the /usr/local/Library/Formula
    # and remove it after building a bottle
    cp ./${FORMULA_NAME}.rb /usr/local/Library/Formula/
    brew bottle --no-revision ${FORMULA_NAME}
    rm /usr/local/Library/Formula/${FORMULA_NAME}.rb

    if [ -z ${BINTRAY_ID+x} ] || [ -z ${BINTRAY_PWD+x} ] ; then
        echo "Fail to find BINTRAY_ID and BINTRAY_PWD env variables."
    else
        echo "=============================================="
        echo "3. Upload/Publish ${VERSION_STRING} to Bintray"
        echo "=============================================="
        ./upload_to_bintray.sh ${BINTRAY_ID} ${BINTRAY_PWD} ${VERSION_STRING}
    fi
else
    echo "Nothing to do."
fi
mv ${REPO_NAME}/CURRENT_HASH ${REPO_NAME}/PREVIOUS_HASH
