build:
	stack build uniqc

build-prof:
	stack build --profile uniqc

clean:
	stack clean

tags:
	hasktags-generate .

sources:
	stack-unpack-dependencies

