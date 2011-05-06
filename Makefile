BUILD_DIR=build
TEX_BUILD_DIR=tex_build
TEX_SRC_DIR=doc
GHC_FLAGS=-Wall
APP=wham
SRC_DIR=src
SOURCES=${SRC_DIR}/Main.hs \
		${SRC_DIR}/Interpreter.hs \
		${SRC_DIR}/Translator.hs \
		${SRC_DIR}/Parser.hs \
		${SRC_DIR}/AMDefinitions.hs \
		${SRC_DIR}/BoolExc.hs \
		${SRC_DIR}/IntegerExc.hs \
		${SRC_DIR}/Evaluator.hs \
		${SRC_DIR}/Debugger.hs

all: build

build: $(SOURCES)
	mkdir -p ${BUILD_DIR}
	ghc ${GHC_FLAGS} -i${SRC_DIR} --make ${SRC_DIR}/Main.hs \
		-outputdir ${BUILD_DIR} -o ${BUILD_DIR}/${APP}

report: ${TEX_SRC_DIR}/report.tex
	mkdir -p ${TEX_BUILD_DIR}
	cd ${TEX_SRC_DIR} && \
	pdflatex -interaction=batchmode -output-directory ../${TEX_BUILD_DIR} \
	         report.tex

clean:
	rm -rf ${BUILD_DIR} ${TEX_BUILD_DIR} ${APP}
