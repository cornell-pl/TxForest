SetupPath="example_fs_root"

all: build

build:
.SILENT:

.PHONY: test server client briannasgrades index grades install killAndClean kill clean 


test:
	python -m tests

server:
	python src/server.py

client:
	rlwrap python -m examples 'universalclient' $(SetupPath)

briannasgrades:
	rlwrap python -m examples 'briannasgrades' "cs4999"

index:
	python -m examples 'index' $(SetupPath) $(OP) $(I) $(NAME) $(CONTENTS)

grades:
	python -m examples 'grades' $(SetupPath) $(OP) $(HW) $(I) $(NAME) $(SCORE)

install:
	echo "TODO: install deps"

killAndClean: kill clean

kill:
	killall -9 python

clean:
	find . -name '*.pyc' -delete
	find . -name '__pycache__' -delete
	find . -name 'commit' -delete

