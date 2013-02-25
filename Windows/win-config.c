#include <windows.h>
#include <malloc.h>
#include <stdio.h>
#include <direct.h>

#define SWI_KEY_1 "HKCU\\Software\\SWI\\Prolog"
#define SWI_KEY_2 "HKLM\\Software\\SWI\\Prolog"
#define MAXREGSTRLEN 1024

#define streq(s,q) (strcmp((s), (q)) == 0)
#define strdup(s)  strcpy(malloc(strlen(s)+1), s)

static HKEY
reg_open_key(const char *which, int w64)
{ HKEY key = HKEY_CURRENT_USER;
  REGSAM flags = KEY_READ;

  if ( w64 )
    flags |= KEY_WOW64_64KEY;

  while(*which)
  { char buf[256];
    char *s;
    HKEY tmp;

    for(s=buf; *which && !(*which == '/' || *which == '\\'); )
      *s++ = *which++;
    *s = '\0';
    if ( *which )
      which++;

    if ( streq(buf, "HKCR") )
    { key = HKEY_CLASSES_ROOT;
      continue;
    } else if ( streq(buf, "HKCU") )
    { key = HKEY_CURRENT_USER;
      continue;
    } else if ( streq(buf, "HKLM") )
    { key = HKEY_LOCAL_MACHINE;
      continue;
    } else if ( streq(buf, "HKU") )
    { key = HKEY_USERS;
      continue;
    }

    if ( RegOpenKeyEx(key, buf, 0L, flags, &tmp) == ERROR_SUCCESS )
    { RegCloseKey(key);
      key = tmp;
      continue;
    }

    return NULL;
  }

  return key;
}

static char *
read_string_key(const char *keyname, const char *valuename, int w64)
{ DWORD type;
  DWORD len;
  BYTE  data[MAXREGSTRLEN];
  HKEY key;

  if ( (key=reg_open_key(keyname, w64)) )
  { int rc = RegQueryValueEx(key, valuename, NULL, &type, data, &len);

    RegCloseKey(key);

    if ( rc == ERROR_SUCCESS && type == REG_SZ )
      return strdup((char*)data);
  }

  return NULL;
}


static char *
PrologHome(void)
{ char *home;

  if ( (home=read_string_key(SWI_KEY_1, "home", TRUE)) ) return home;
  if ( (home=read_string_key(SWI_KEY_2, "home", TRUE)) ) return home;
  if ( (home=read_string_key(SWI_KEY_1, "home", FALSE)) ) return home;
  if ( (home=read_string_key(SWI_KEY_2, "home", FALSE)) ) return home;

  return NULL;
}


static int
godir(void)
{ char data[1024];

  if ( GetModuleFileName(NULL, data, sizeof(data)) > 0 )
  { char *e = data+strlen(data);

    while(e>data && e[-1] != '\\')
      e--;
    if ( e > data )
      e[-1] = '\0';

    printf("Installing in %s\n", data);
    return _chdir(data) == 0;
  }

  return FALSE;
}


int
main(int argc, char **argv)
{ char *home;

  if ( !godir() )
  { fprintf(stderr, "Could not go to base folder\n");
    return 1;
  }

  if ( (home=PrologHome()) )
  { char cmdline[1024];

    sprintf(cmdline, "\"%s\\bin\\swipl-win.exe\" -s setup.pl", home);
    printf("Running %s\n", cmdline);

    return system(cmdline) ? 1 : 0;
  }

  return 1;
}
