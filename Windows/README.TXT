This directory provides the  sources  for   win-config.exe.  This  is  a
frontend for setting up ClioPatria  that   is  similar  to the configure
shell script used for non-Windows systems.

The program win-config.exe performs two steps:

	- Find Prolog from the registry.  It first looks for the 64-bit
          version and otherwise falls back to the 32-bit version.
	- Change working directory to the place where win-config.exe
	  is installed.
	- Run "<path>swipl-win.exe -s setup.pl" to configure ClioPatria

I tried to write this as a  .bat   file,  but handling the registry from
there in a portable way is really a nightmare.
