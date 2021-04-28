
unit Permission;

interface

function IsFileExecutable(const AFileName: string): boolean;
function MakeFileExecutable(const AFileName: string): boolean;
function MakeFileExecutableIf(const AFileName: string): boolean;

implementation

{$IFDEF UNIX}
uses
  SysUtils, BaseUnix;
{$ENDIF}

function IsFileExecutable(const AFileName: string): boolean;
{$IFDEF UNIX}
var
  LStat: stat;
{$ENDIF}
begin
{$IFDEF UNIX}
{.$PUSH}
{.$WARN 5057 OFF}
  //FillByte(LStat, SizeOf(stat), 0);
  LStat := Default(stat);
  result := (FpStat(AFileName, LStat) <> -1)
    and FPS_ISREG(LStat.st_mode)
    and (FpAccess(AFileName, X_OK) = 0);
{.$POP}
{$ENDIF}
end;

function MakeFileExecutable(const AFileName: string): boolean;
{$IFDEF UNIX}
const
  CPermission: TMode = 
    S_IRWXO or // Read, write, execute by others.
    S_IRWXG or // Read, write, execute by groups.
    S_IRWXU;   // Read, write, execute by user.
{$ENDIF}
begin
{$IFDEF UNIX}
  result := FpChmod(AFileName, {&777}CPermission) = 0;
{$ENDIF}
end;

function MakeFileExecutableIf(const AFileName: string): boolean;
begin
  result := IsFileExecutable(AFileName) or MakeFileExecutable(AFileName);
end;

end.
