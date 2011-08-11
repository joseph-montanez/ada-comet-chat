#!/bin/bash
for i in `seq 1 50`
do
   r=( $(openssl rand 100000 | sha1sum) ); 
   id=`printf "%s${r[0]:0:16}\n"`
   echo "Connected: $i"
   wget --quiet -T 60 "http://localhost:1234/server_push?clientId=$id" &
done
