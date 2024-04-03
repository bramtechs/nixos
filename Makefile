build:
	nixos-rebuild switch
upgrade:
	nixos-rebuild switch --upgrade
clean:
	nix-collect-garbage -d
