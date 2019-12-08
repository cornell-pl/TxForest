
all: build

build:
.SILENT:

test:
	python -m tests

server:
	python src/server.py

client:
	python -m examples 'universalclient' $(PATH)

index:
	python -m examples 'index' $(PATH) $(OP) $(I) $(NAME) $(CONTENTS)

grades:
	python -m examples 'grades' $(PATH) $(OP) $(HW) $(I) $(NAME)

install:
	echo "TODO: install deps"

clean:
	find . -name '*.pyc' -delete

