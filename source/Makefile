ifdef COMSPEC
	RM := del /q
else
	RM := rm -f
endif

.PHONY: eschecs release strip clean
eschecs:
	@fpc -dDEBUG @extrafpc.cfg eschecs.pas
release:
	@fpc -dRELEASE @extrafpc.cfg eschecs.pas
clean:
	@$(RM) units/*.o
	@$(RM) units/*.ppu
	@$(RM) units/*.rsj
