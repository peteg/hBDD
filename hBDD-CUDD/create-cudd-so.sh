#!/bin/sh
# Last Modified: <2011-08-09 15:14>
# This script takes cudd's static library files and converts it into a shared object for dynamic loading


gcc -shared -Wl,-soname,libcudd.so -o libcudd.so.1 -Wl,--whole-archive libmtr.a libst.a libdddmp.a libcudd.a libepd.a libutil.a -Wl,--no-whole-archive -lc
if [ ! -e libcudd.so ]
then
    ln -s libcudd.so.1 libcudd.so
fi
