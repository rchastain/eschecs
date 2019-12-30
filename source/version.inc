
const
  CVersion = '5.0.4';
  CDefaultTitle = 'Eschecs'; 
{$if defined(cpu64) and defined(windows)}
  COsType = 'win64';
{$ENDIF}
{$if defined(cpu86) and defined(windows)}
  COsType = 'win32';
{$ENDIF}
{$if defined(cpu64) and defined(linux)}
  COsType = 'lin64';
{$ENDIF}
{$if defined(cpu86) and defined(linux)}
  COsType = 'lin32';
{$ENDIF}
{$if defined(cpuarm) and defined(linux)}
  COsType = 'arm32';
{$ENDIF}
{$if defined(cpu64) and defined(freebsd)}
  COsType = 'bsd64';
{$ENDIF}
{$if defined(cpu86) and defined(freebsd)}
  COsType = 'bsd32';
{$ENDIF}
