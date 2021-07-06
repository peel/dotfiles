set -e

# Hetzner Cloud OS images grow the root partition to the size of the local
# disk on first boot. In case the NixOS live ISO is booted immediately on
# first powerup, that does not happen. Thus we need to grow the partition
# by deleting and re-creating it.
sgdisk -d 1 /dev/sda
sgdisk -N 1 /dev/sda
partprobe /dev/sda

mkfs.ext4 -F /dev/sda1 # wipes all data!

mount /dev/sda1 /mnt

nixos-generate-config --root /mnt

# Delete trailing `}` from `configuration.nix` so that we can append more to it.
sed -i -E 's:^\}\s*$::g' /mnt/etc/nixos/configuration.nix

# Extend/override default `configuration.nix`:
echo '
  boot.loader.grub.devices = [ "/dev/sda" ];
  # Initial empty root password for easy login:
  users.users.root.initialHashedPassword = "";
  services.openssh.permitRootLogin = "prohibit-password";
  services.openssh.enable = true;
  users.users.root.openssh.authorizedKeys.keys = [
    # Replace this by your SSH pubkey!
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDG1kHVJm6zcWqDEsACPmy+CgCsrF8yLGpXi0VwHCYGUmvIyjHrmJrr3iAt+4tJprp+FT7CSR/dXk2NajBwxRerncXfFM/nkIdrDPPgO2yxpFIms5dwt8znnjf8T05mW/dXLr7E45uaT4DBqW/eVeZlH3VuJINmi1GJrp+mMKypIFk7DzqM+bQz0MM+qRjH0433XL0Lv+o1aeYfjW12SKWHruMXLDT820T9qefxvhx83hZRGgzTNkof1svBGV9rdCcKzjPsHw1uAWex0KQI3LpuE7fzKwElR2/z2JwoPRiPqum4QilpeyqOGA8xwhJcM/zAHf1hvVuQYlXiSl+QFfMP0Lx5vW/FxZXoa9DJzk5kH0o2ALwE8F/iUOgi2Poy4mq1rhUn26ztSifgWLJLaWBagbn7+dx3dRL1rxBPCJo5cunKBViBpaxdbnSVnaHicoMtQRZ6NUL9g6r1w2oOYftff340LE3kTNzpGhs3vTqsAj0ubwwsFuRK/hjPgv8Rndw8SehuZf+s7kdisBovUyGs/+P15A0pUQ7S65A+N/8zsKZYrnaBxUQ0AqzxPKz2OEuBfwCnFneOwstiUbWs7zNz4BvYIsbCq7mUi7IESzEQ/ttYTG2xHgc38lUlePNazpvxcGWHBcNI1n3SDfhAHpIjWIzXw96AO2JKo+Yl7EpwIQ== cardno:000609697351"
  ];
}
' >> /mnt/etc/nixos/configuration.nix

nixos-install --no-root-passwd

poweroff
