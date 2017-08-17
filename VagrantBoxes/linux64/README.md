# build MovingBoundary solver for server deployment on centos 7.2

vagrant up
vagrant ssh -c /vagrant/build.sh
vagrant ssh -c /vagrant/build-client.sh
vagrant halt
