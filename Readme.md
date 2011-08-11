Ada Comet Chat
==============

I had been looking for a web server / framework hybrid handle long live 
connections. Ada AWS seemed to be the perfect solution. So this is my attempt
to make a chat application using Ada AWS. Ada AWS provides a high performance, 
concurrent Push module.

Requirements
------------

Linux (Only OS I have to test on)
Ada Gnat Compiler 4.4+
AWS 2.10.0w

Compiling on Ubuntu
-------------------
1.  <del>sudo apt-get install libaws2.7-dev</del> (too old)
2.  make
3.  ./ws
4.  Profit!
