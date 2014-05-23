# -*- mode: ruby -*-

ROOT = "http://opscode-vm-bento.s3.amazonaws.com/vagrant/virtualbox/opscode_"

URLS = {
  "debian-7.4" => ROOT + "debian-7.4_chef-provisionerless.box",
  "centos-6.5" => ROOT + "centos-6.5_chef-provisionerless.box",
  "freebsd-10.0" => ROOT + "freebsd-10.0_chef-provisionerless.box",
  "ubuntu-14.04" => ROOT + "ubuntu-14.04_chef-provisionerless.box"
}

Vagrant.configure("2") do |config|
  config.vm.network :private_network, ip: "192.168.58.101"
  config.vm.synced_folder ".", "/home/vagrant/crane"

  # Machines

  URLS.each do |name, url|
    config.vm.define "crane_" + name do |m|
      m.vm.box = name
      m.vm.box_url = url
    end
  end

  # Virtual hardware
  config.vm.provider :virtualbox do |vb|
    vb.customize ["modifyvm", :id, "--memory", "2048"]
    vb.customize ["modifyvm", :id, "--cpus", "2"]
  end
end
