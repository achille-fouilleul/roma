CONFIG := Debug
#CONFIG := Release

.PHONY: all clean

target := bin/$(CONFIG)/cli.dll

all: $(target)

clean:
	-rm $(target)

$(target): \
	LittleEndian.fs \
	BigEndian.fs \
	ByteReader.fs \
	PEImageReader.fs \
	Flags.fs \
	Tables.fs \
	MetadataReader.fs \
	MetadataRef.fs \
	Instruction.fs \
	Metadata.fs \
	IModuleLoader.fs \
	SignatureDecoding.fs \
	IL.fs \
	CILReader.fs \
	ModuleLoading.fs
	mkdir -p $(shell dirname $@)
	fsharpc --target:library --out:$@ $^

