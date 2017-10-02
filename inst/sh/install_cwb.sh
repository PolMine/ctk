#!/bin/sh

mkdir ~/tmp
cd ~/tmp
wget https://sourceforge.net/projects/cwb/files/cwb/cwb-3.0.0/cwb-3.0.0-linux-x86_64.tar.gz
tar xzfv cwb-3.0.0-linux-x86_64.tar.gz
cd cwb-3.0.0-linux-x86_64
./install-cwb.sh