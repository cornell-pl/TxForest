TARGETS= forestServer.exe forestClient.exe
EXAMPLES= paperExOCaml.exe testPpx.exe simpleEval.exe paperEx.exe simpleEvalSurf.exe \
					paperExOld.exe
					# grades.exe shelter.exe simple.exe  dependency.exe


.PHONY: all build clean %.exe

all: build link

build:
	dune build --profile release

link: $(EXAMPLES) $(TARGETS)

%.exe:
	if [ ! -d executables ]; then mkdir executables; fi
	if [ ! -f executables/$@ ]; then ln -s ../$$(find _build -name $@) executables/$@ ; fi

test:
	dune build @runtest

install:
	dune install

clean:
	dune clean;
	cd executables; rm -f $(TARGETS); rm -f $(EXAMPLES); cd ..
