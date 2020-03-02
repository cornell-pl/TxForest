SetupPath="example_fs_root"

all: build

build:
.SILENT:

test:
	python -m tests

server:
	python src/server.py

client:
	python -m examples 'universalclient' $(SetupPath)

index:
	python -m examples 'index' $(SetupPath) $(OP) $(I) $(NAME) $(CONTENTS)

grades:
	python -m examples 'grades' $(SetupPath) $(OP) $(HW) $(I) $(NAME) $(SCORE)

install:
	echo "TODO: install deps"

clean:
	find . -name '*.pyc' -delete

