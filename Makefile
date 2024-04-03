build:
	nixos-rebuild switch
clean:
	nix-collect-garbage -d
