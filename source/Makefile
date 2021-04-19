../eschecs:
	@fpc -dDEBUG @extrafpc.cfg eschecs.pas
	@sh -c 'stat --printf="Executable file size: %s bytes\n" ../eschecs'
release:
	@fpc -dRELEASE @extrafpc.cfg eschecs.pas
	@sh -c 'stat --printf="Executable file size: %s bytes\n" ../eschecs'
strip:
	@fpc -dRELEASE @extrafpc.cfg eschecs.pas
	@upx -qq ../eschecs
	@sh -c 'stat --printf="Executable file size: %s bytes\n" ../eschecs'
.PHONY: clean
clean:
	@rm -f *.o
	@rm -f *.ppu
