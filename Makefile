all:
	ghc -Wall --make Main.hs -outputdir build -o wham

report: report.tex
	pdflatex report.tex

clean:
	rm -r build wham
