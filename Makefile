.PHONY: default build-dev build install uninstall test clean utop

default: build-dev

build-dev:
	jbuilder build --dev

build:
	jbuilder build

test:
	jbuilder runtest -f

install:
	jbuilder install

uninstall:
	jbuilder uninstall

clean:
	jbuilder clean

utop:
	jbuilder utop lib
