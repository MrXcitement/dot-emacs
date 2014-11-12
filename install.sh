#!/usr/bin/env bash

# install.sh -- make symbolic links in the users home directory
# Mike Barker <mike@thebarkers.com>
# May 14th, 2013

# History:
# 2014.11.12 MRB
# * Added code to install scripts based on system type.

echo "$(basename $0)"
echo "Make sympolic links in the users home directory to the bash config files and directories"

DIR="$(cd "$(dirname "$0")" && pwd)"
FSPEC="emacs.d"
for FILE in ${FSPEC}; do
    echo "Processing file ${FILE}"
    if [ -a ~/.${FILE} ]; then
	if ! [ -h ~/.${FILE} ]; then
	    mv ~/.${FILE} ~/.${FILE}.backup
	fi
    fi
    if ! [ -h ~/.${FILE} ]; then
	ln -sv ${DIR}/${FILE} ~/.${FILE}
    fi
done

# handle system specific install
unamestr=`uname`
if [[ "$unamestr" == 'Darwin' ]]; then
    install -v -m770 osx/* ~/bin
fi
