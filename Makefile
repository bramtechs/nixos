build:
	nixos-rebuild switch
upgrade:
	nix-channel --update
	nixos-rebuild switch --upgrade
clean:
	nix-collect-garbage -d
iso:
	nix-build '<nixpkgs/nixos>' -A config.system.build.isoImage -I nixos-config=iso.nix
iso-test:
	qemu-system-x86_64 -enable-kvm -m 2048 -smp 4 -cdrom result/iso/nixos-*.iso
