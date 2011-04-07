all:
	ghc --make Main.hs -outputdir build -o wham

clean:
	rm -r build wham
