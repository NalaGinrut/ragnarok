
event-cfile := $(wildcard $(EVENT)/*.c)
event-ofile := $(event-cfile:.c=.o)
event-ofile := $(subst $(EVENT)/,$(OBJ)/event/,$(event-ofile))

$(OBJ)/event/%.o: $(EVENT)/%.c
	@echo + cc $<
	@mkdir -p $(@D)
	$(V)$(CC) $(CFLAGS) -O3 -c -o $@ $<

$(eventlib-ofile): $(event-ofile)
	@echo generating $@ ...
	$(V)$(CC) $(LIBS) -o $@ $<
