#/usr/bin/bash

gf --run < tests.gfs | diff -u - test-gold.md


echo "If this is the first line you see, it means success!"
