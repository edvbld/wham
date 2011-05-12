WHAM=./dist/build/wham/wham
SAMPLE_DIR=samples
ANALYZE_DIR=doc/analysis
ANALYZE_FILES=$(ANALYZE_DIR)/always_catch.analysis $(ANALYZE_DIR)/infinite.analysis $(ANALYZE_DIR)/unneeded_catch.analysis

all : $(WHAM) analysis

$(WHAM) :
	cabal build

analysis : $(WHAM) $(ANALYZE_FILES)

$(ANALYZE_DIR)/%.analysis : $(SAMPLE_DIR)/analysis_%.while
	@mkdir -p `dirname $@`
	$(WHAM) -a $< > $@
