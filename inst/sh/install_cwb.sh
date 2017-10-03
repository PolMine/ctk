#!/bin/sh

mkdir /opt/cwb
mkdir /opt/cwb/bin
mkdir /opt/cwb/include
mkdir /opt/cwb/lib
mkdir /opt/share
mkdir /opt/share/man

cd /opt/cwb
wget https://sourceforge.net/projects/cwb/files/cwb/cwb-3.0.0/cwb-3.0.0-linux-x86_64.tar.gz
tar xzfv cwb-3.0.0-linux-x86_64.tar.gz
sed -i -e "s#PREFIX='/usr/local'#PREFIX='/opt/cwb/'#" ./cwb-3.0.0-linux-x86_64/install-cwb.sh
./cwb-3.0.0-linux-x86_64/install-cwb.sh
export PATH=$PATH:/opt/cwb/bin

wget http://cwb.sourceforge.net/temp/Perl-CWB-2.2.102.tar.gz
tar xzfv Perl-CWB-2.2.102.tar.gz
cd CWB-2.2.102
perl Makefile.PL
make
make test
make install