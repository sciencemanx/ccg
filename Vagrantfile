# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure("2") do |config|
  config.vm.box = "ubuntu/xenial64"

  config.vm.synced_folder ".", "/vagrant", disabled: true
  config.vm.synced_folder "ccg", "/home/vagrant/ccg"

  config.vm.provider "virtualbox" do |vb|
    vb.name = "ccg"
    vb.memory = "2048"
  end

  config.vm.provision "shell", inline: <<-SHELL
    apt-get update
    curl -sSL https://get.haskellstack.org/ | sh
    git clone https://github.com/aquynh/capstone.git
    cd capstone
    git checkout 4.0-alpha5
    ./make.sh install
  SHELL
end
