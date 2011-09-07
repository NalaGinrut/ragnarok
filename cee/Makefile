CFLAGS := -r `guile-config compile`
OBJ := ../obj

$(OBJ)/*.o: *.c
	@echo + cc $<
	@mkdir -p $(@D)
	$(V)$(CC) $(CFLAGS) -o $@ $<
