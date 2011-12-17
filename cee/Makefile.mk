
cee-cfile := $(wildcard $(CEE)/*.c)
cee-ofile := $(cee-cfile:.c=.o)
cee-ofile := $(subst $(CEE)/,$(OBJ)/,$(cee-ofile))

$(OBJ)/%.o: $(CEE)/%.c
	@echo + cc $<
	@mkdir -p $(@D)
	$(V)$(CC) $(CFLAGS) -O3 -c -o $@ $<

$(ceelib-ofile): $(cee-ofile)
	@echo generating $@ ...
	$(V)$(CC) $(LIBS) -o $@ $^
