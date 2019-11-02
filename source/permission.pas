
unit Permission;

interface

function IsFileExecutable(const AFileName: string): boolean;
function MakeFileExecutable(const AFileName: string): boolean;

implementation

uses
  BaseUnix, Unix;

function IsFileExecutable(const AFileName: string): boolean;
var
  LStat: Stat;
begin
  result := (FpStat(AFileName, LStat) <> -1) and FPS_ISREG(LStat.st_mode)
    and (FpAccess(AFileName, X_OK) = 0);
end;

function MakeFileExecutable(const AFileName: string): boolean;
begin
  result := FpChmod(AFileName, &777) = 0;
end;

end.
