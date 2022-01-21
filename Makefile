all:
	cabal install --install-method=copy --overwrite-policy=always --installdir=dist-newstyle compiler
