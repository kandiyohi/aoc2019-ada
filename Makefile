UNAME != uname
.if $(UNAME) == FreeBSD
GNAT ?= /usr/local/gcc6-aux/bin/gnat
.else
GNAT ?= gnat
.endif
PATH := $(GNAT_PATH):$(PATH)

all:
	echo "$(PWD)"
	gprbuild

clean:
	gprclean
