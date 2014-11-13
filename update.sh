#!/usr/bin/env bash
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
    cd ${REPO_NAME}
    git rev-parse HEAD > LAST_HASH
    cd ..
fi   

cd ${REPO_NAME}
git fetch --all --quiet
git reset --hard origin/master --quiet
git rev-parse HEAD > CURRENT_HASH
cd ..

if ! cmp ${REPO_NAME}/LAST_HASH ${REPO_NAME}/CURRENT_HASH >/dev/null 2>&1
then
    DOIT=TRUE
fi
if [[ $1 == "-f" ]] ; then
    DOIT=TRUE
fi

if [[ $DOIT == TRUE ]] ; then
    # 1. Update formula with a new version
    VERSION_MAJOR=`grep -o -i "VERSION_MAJOR \([0-9]\+\)" ${REPO_NAME}/src/CMakeLists.txt | cut -d ' ' -f 2`
    VERSION_MINOR=`grep -o -i "VERSION_MINOR \([0-9]\+\)" ${REPO_NAME}/src/CMakeLists.txt | cut -d ' ' -f 2`
    VERSION_PATCH=`grep -o -i "VERSION_PATCH \([0-9]\+\)" ${REPO_NAME}/src/CMakeLists.txt | cut -d ' ' -f 2`
    COMMIT_HASH=git`cat ${REPO_NAME}/CURRENT_HASH`
    VERSION_STRING=${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCH}
    echo ${VERSION_STRING}-${COMMIT_HASH}
    cp ${FORMULA_TEMPLATE} ${FORMULA_FILE}
    sed -i "" "s/##VERSION##/${VERSION_STRING}/g" ${FORMULA_FILE}
    sed -i "" "s/##COMMIT_HASH##/${COMMIT_HASH}/g" ${FORMULA_FILE}
    sed -i "" "s/##ORG_NAME##/${ORG_NAME}/g" ${FORMULA_FILE}
    sed -i "" "s/##FORMULA_NAME##/${FORMULA_NAME}/g" ${FORMULA_FILE}

    # 2. Push formula
    git add ${FORMULA_FILE}
    git commit -m "${VERSION_STRING}-${COMMIT_HASH}-${OSX_VERSION} [skip ci]"
    git pull --rebase origin master
    git push origin master:master

    # 3. Create a bottle
    brew untap $ORG_NAME/$REPO_NAME
    brew tap $ORG_NAME/$REPO_NAME
    brew rm ${FORMULA_NAME}
    brew install --build-bottle ${FORMULA_NAME}
    brew bottle ${FORMULA_NAME}
    sed -i "" "s/##BOTTLE_COMMENT##//g" ${FORMULA_FILE}
    BOTTLE_FILE_YOSEMITE=${FORMULA_NAME}-${VERSION_STRING}-${COMMIT_HASH}.yosemite.bottle.tar.gz
    BOTTLE_FILE_MAVERICKS=${FORMULA_NAME}-${VERSION_STRING}-${COMMIT_HASH}.mavericks.bottle.tar.gz
    if [[ ${OSX_VERSION} = 10.10* ]] ; then
        # Try to download 10.9 file
        wget https://${ORG_NAME}.github.io/homebrew-${REPO_NAME}/${BOTTLE_FILE_MAVERICKS}
    fi
    if [[ ${OSX_VERSION} = 10.9* ]] ; then
        # Try to download 10.10 file
        wget https://${ORG_NAME}.github.io/homebrew-${REPO_NAME}/${BOTTLE_FILE_YOSEMITE}
    fi
    if [ -e ${BOTTLE_FILE_MAVERICKS} ] ; then
        MAVERICKS_HASH=`shasum ${BOTTLE_FILE_MAVERICKS} | cut -d ' ' -f 1`
        sed -i "" "s/##BOTTLE_MAVERICKS_HASH##/${MAVERICKS_HASH}/g" ${FORMULA_FILE}
        PUSH_FORMULA_WITH_BOTTLE=TRUE
    fi           
    if [ -e ${BOTTLE_FILE_YOSEMITE} ] ; then
        YOSEMITE_HASH=`shasum ${BOTTLE_FILE_YOSEMITE} | cut -d ' ' -f 1`
        sed -i "" "s/##BOTTLE_YOSEMITE_HASH##/${YOSEMITE_HASH}/g" ${FORMULA_FILE}
        PUSH_FORMULA_WITH_BOTTLE=TRUE
    fi           

    # 4. Update formula again with bottle
    if [[ ${PUSH_FORMULA_WITH_BOTTLE} == TRUE ]] ; then
        git add ${FORMULA_FILE}
        git commit -m "Bottle: ${VERSION_STRING}-${COMMIT_HASH}"
        git pull --rebase -s recursive -X ours origin master
        git push origin master:master
    fi

    # 5. Update gh-pages branch
    git branch -D gh-pages
    git checkout --orphan gh-pages
    rm .git/index
    git add -f *.tar.gz
    git clean -fxd
    git commit -m "Bottle: ${VERSION_STRING}-${COMMIT_HASH} [skip ci]"
    git push origin --force gh-pages:gh-pages
    git checkout master
    rm -rf *.tar.gz
else
    echo "Nothing to do."
fi
mv ${REPO_NAME}/CURRENT_HASH ${REPO_NAME}/LAST_HASH
