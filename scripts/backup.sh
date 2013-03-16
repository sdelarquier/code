#!/bin/bash
# Backs stuff up

# First var and etc folders
echo "----> Copy /var"
cp -arvf /var /home/backup/
echo "----> Copy /etc"
cp -arvf /etc /home/backup/
