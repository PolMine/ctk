#!/bin/sh

mkdir ~/tmp
cd ~/tmp
wget https://sourceforge.net/projects/cwb/files/cwb/cwb-3.0.0/cwb-3.0.0-linux-x86_64.tar.gz
tar xzfv cwb-3.0.0-linux-x86_64.tar.gz
./cwb-3.0.0-linux-x86_64/install-cwb.sh

wget http://cwb.sourceforge.net/temp/Perl-CWB-2.2.102.tar.gz
tar xzfv Perl-CWB-2.2.102.tar.gz
cd CWB-2.2.102
perl Makefile.PL
make
make test
make install