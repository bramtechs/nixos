build:
	nixos-rebuild switch
upgrade:
	nix-channel --update
	nixos-rebuild switch --upgrade
clean:
	nix-collect-garbage -d
