#!/bin/bash
echo "Author : Ilango Guy" &&
echo "License :Research Center for Respiratory Diseases Tours - France" &&

echo $1

prefetch $1 &&

fasterq-dump $1 -I --split-files --skip-technical &&

echo "Process complete" &&

echo "Don't forget to bring me a coffe"






