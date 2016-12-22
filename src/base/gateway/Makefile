## -*- makefile -*-
## Erlang
.PHONY: edoc dialyzer clean

APP_NAME := gate_app
APP_VSN := 0.1

ERL := erl
ERLC := $(ERL)c

INCLUDE_DIRS := ../../include ./

BASE_DIR := ../..
EBIN_DIR := $(BASE_DIR)/ebin
DOC_DIR  := $(BASE_DIR)/edoc
DOC_OPTS :={dir,\"$(DOC_DIR)\"}

ERL_SOURCES := $(wildcard *.erl)
ERL_MODULES := $(ERL_SOURCES:%.erl=%)
ERL_MODULES_IN_APP := $(ERL_SOURCES:%.erl=`%`,)
ERL_HEADERS := $(wildcard *.hrl) $(wildcard ../include/*.hrl)
APP_TPL_FILES = $(wildcard *.app.src)
APP_FILES = $(APP_TPL_FILES:%.src=%)
CTL_TPL_FILES = $(wildcard *ctl.tpl)
CTL_FILES = $(addprefix $(BASE_DIR)/, $(CTL_TPL_FILES:%.tpl=%))

ALL_FILES = $(addprefix $(EBIN_DIR)/, $(APP_FILES))

all: $(ALL_FILES)

$(EBIN_DIR)/%.app: %.app.src
	@echo "creating .app file... "
	@sed -e 's:@APP_NAME@:$(APP_NAME):g' \
		-e 's:@APP_VSN@:$(APP_VSN):g' \
		-e 's:@MODULES@:$(ERL_MODULES_IN_APP):g' -e "s:\`:\':g" -e 's/,]/]/g' $< > $@

$(BASE_DIR)/%ctl: %ctl.tpl
	@echo "create ctl script"
	@sed -e 's:@LOGDIR@:$(LOGDIR):g' -e 's:@CONFDIR@:$(CONFDIR):g'  $< > $@
	(chmod 775 $@)

edoc:
	$(ERL) -noshell -eval "edoc:application($(APP_NAME), \".\", [${DOC_OPTS}]),init:stop()"

clean:
	(rm -rf $(ALL_FILES))
