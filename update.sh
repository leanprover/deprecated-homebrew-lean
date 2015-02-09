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
    brew bottle --no-revision ./${FORMULA_NAME}.rb
    sed -i "" "s/##BOTTLE_COMMENT##//g" ${FORMULA_FILE}
    BOTTLE_FILE_YOSEMITE=${FORMULA_NAME}-${VERSION_STRING}.yosemite.bottle.tar.gz
    BOTTLE_FILE_MAVERICKS=${FORMULA_NAME}-${VERSION_STRING}.mavericks.bottle.tar.gz
 
    # 3. Try to download bottles for other OSX versions
    if [[ ${OSX_VERSION} = 10.10* ]] ; then
        OSX_CODENAME=yosemite
        echo "===================================="
        echo "3. Try to download 10.9 (mavericks) file"
        echo "===================================="
        wget https://raw.githubusercontent.com/${ORG_NAME}/homebrew-${REPO_NAME}/mavericks/${BOTTLE_FILE_MAVERICKS}
    fi
    if [[ ${OSX_VERSION} = 10.9* ]] ; then
        OSX_CODENAME=mavericks
        echo "===================================="
        echo "3. Try to download 10.10 (yosemite) file"
        echo "===================================="
        wget https://raw.githubusercontent.com/${ORG_NAME}/homebrew-${REPO_NAME}/yosemite/${BOTTLE_FILE_YOSEMITE}
    fi

    echo "===================================="
    echo 4. Push the current bottle to the github
    echo "===================================="
    BOTTLE_FILE=${FORMULA_NAME}-${VERSION_STRING}.${OSX_CODENAME}.bottle.tar.gz
    mkdir ${OSX_CODENAME}-tmp && cd ${OSX_CODENAME}-tmp
    git init
    git remote add origin git@github.com:${ORG_NAME}/homebrew-${REPO_NAME}
    cp ../${BOTTLE_FILE} .
    git add ${BOTTLE_FILE}
    git ci -m "Add ${BOTTLE_FILE}"
    git push --force origin master:${OSX_CODENAME}
    cd ..
    rm -rf ${OSX_CODENAME}-tmp

    echo "===================================="
    echo 5. Check whether we have all bottles to update formula or not        
    echo "===================================="
    if [ -e ${BOTTLE_FILE_MAVERICKS} ] && [ -e ${BOTTLE_FILE_YOSEMITE} ] ; then
        grep "sha1" ${FORMULA_FILE}
        echo "===================================="
        MAVERICKS_HASH=`shasum ${BOTTLE_FILE_MAVERICKS} | cut -d ' ' -f 1`
        YOSEMITE_HASH=`shasum ${BOTTLE_FILE_YOSEMITE} | cut -d ' ' -f 1`
        sed -i "" "s/##BOTTLE_MAVERICKS_HASH##/${MAVERICKS_HASH}/g" ${FORMULA_FILE}
        sed -i "" "s/##BOTTLE_YOSEMITE_HASH##/${YOSEMITE_HASH}/g" ${FORMULA_FILE}
        echo "mavericks hash : ${MAVERICKS_HASH}"
        echo "yosemite  hash : ${YOSEMITE_HASH}"
        grep "sha1" ${FORMULA_FILE}
        PUSH_FORMULA_WITH_BOTTLE=TRUE
        echo Found: ${BOTTLE_FILE_YOSEMITE} and ${BOTTLE_FILE_MAVERICKS}
    fi           

    if [ "${PUSH_FORMULA_WITH_BOTTLE}" == "TRUE" ] ; then
        echo "========================================"
        echo "6. Update master branch with new formula"
        echo "========================================"
        git add ${FORMULA_FILE}
        git commit -m "Update: ${VERSION_STRING}"
        git pull --rebase -s recursive -X ours origin master
        git push origin master:master
        echo "========================================"
        echo "7. Update gh-pages branch"
        echo "========================================"
        git branch -D gh-pages
        git checkout --orphan gh-pages
        rm .git/index
        git add -f *.tar.gz
        git clean -fxd
        git commit -m "Bottles: ${VERSION_STRING} [skip ci]"
        git push origin --force gh-pages:gh-pages
        git checkout master
        rm -rf *.tar.gz
    fi
else
    echo "Nothing to do."
fi
mv ${REPO_NAME}/CURRENT_HASH ${REPO_NAME}/PREVIOUS_HASH
