
const
  VERSION = '4.0';
  DEFAULT_TITLE = 'Eschecs'; 

 {$if defined(cpu64) and defined(windows)}
  OSTYPE = 'win64';
 {$ENDIF}
 {$if defined(cpu86) and defined(windows)}
  OSTYPE = 'win32';
 {$ENDIF}
 {$if defined(cpu64) and defined(linux)}
  OSTYPE = 'lin64';
 {$ENDIF}
 {$if defined(cpu86) and defined(linux)}
  OSTYPE = 'lin32';
  {$ENDIF}
 {$if defined(cpuarm) and defined(linux)}
  OSTYPE = 'arm32';
 {$ENDIF}
 {$if defined(cpu64) and defined(freebsd)}
 OSTYPE = 'bsd64';
 {$ENDIF}
 {$if defined(cpu86) and defined(freebsd)}
 OSTYPE = 'bsd32';
 {$ENDIF}
