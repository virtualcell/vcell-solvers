# builds solvers for macos sierra 10.12.5 clients

# Note: can only be run on Apple Macos hardware due to licensing.

## upon first invocation (perform manual provisioning as annotated in the bottom of the Vagrantfile):
vagrant up
vagrant ssh
... follow instructions from bottom of ./Vagrantfile
vagrant halt


## all other invocations (after box is provisioned):
vagrant up
vagrant ssh -c /vagrant/build.sh
vagrant halt
