all: $(PROG).elf

$(PROG).elf: $(PROG).ml
	ocamlc $(PROG).ml -o $(PROG).elf

clean:
	rm $(PROG).elf $(PROG).cmi $(PROG).cmo
