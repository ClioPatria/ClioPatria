CC=i686-w64-mingw32-gcc
LIBS=-ladvapi32

all: win-config.exe

win-config.exe: win-config.c
	$(CC) -Wall -o $@ $< $(LIBS)

install:
	cp win-config.exe ..

clean:
	rm win-config.exe *~
