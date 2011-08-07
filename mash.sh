#!/bin/bash
for i in `seq 1 1023`
do
   echo "Connected: $i"
   wget --quiet -T 60 http://localhost:1234/server_push &
   sleep 0.20
done
