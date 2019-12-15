UNAME != uname
.if $(UNAME) == FreeBSD
GNAT ?= /usr/local/gcc6-aux/bin/gnat
.else
GNAT ?= gnat
.endif
PATH := $(GNAT_PATH):$(PATH)

all: day_1

day_1: day_1.adb
	$(GNAT) make day_1.adb

clean:
	$(GNAT) clean day_1

test-env:
	@echo "GNAT_PATH: $(GNAT_PATH)"
	@echo "PATH: $(PATH)"
	@echo "UNAME: $(UNAME)"
	@echo "gnat location: $(GNAT_PATH)"
