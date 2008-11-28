INCLUDE_DIR=include
SRC_DIR=src
EBIN_DIR=ebin
DOC_DIR=doc
INCLUDES=$(wildcard $(INCLUDE_DIR)/*.hrl)
APPFILE=$(patsubst $(SRC_DIR)/%.app, $(EBIN_DIR)/%.app, $(wildcard $(SRC_DIR)/*.app))
SOURCES=$(wildcard $(SRC_DIR)/*.erl)
TARGETS=$(patsubst $(SRC_DIR)/%.erl, $(EBIN_DIR)/%.beam, $(SOURCES)) $(patsubst $(SRC_DIR)/%.erl, $(DOC_DIR)/%.html, $(SOURCES)) $(APPFILE)
ERLC_OPTS=-W -I $(INCLUDE_DIR) -o $(EBIN_DIR) +debug_info

all: $(TARGETS)

$(EBIN_DIR)/%.app: $(SRC_DIR)/%.app
	cp $< $@

$(EBIN_DIR)/%.beam: $(SRC_DIR)/%.erl $(INCLUDES)
	erlc $(ERLC_OPTS) $<

$(DOC_DIR)/%.html: $(SRC_DIR)/%.erl
	erl -noshell -run edoc file $< -run init stop
	mv $(SRC_DIR)/*.html $(DOC_DIR)

clean:
	rm -f $(TARGETS)
