unit log;

interface
procedure msg(const Msg: string);

implementation
  uses System.SysUtils, System.IOUtils;

procedure msg(const Msg: string);
var
  LogFile: TextFile;
  FileName: string;
begin
  FileName := TPath.Combine(TPath.GetDocumentsPath, 'app.log');
  AssignFile(LogFile, FileName);
  if not FileExists(FileName) then
    Rewrite(LogFile)
  else
    Append(LogFile);
  try
    WriteLn(LogFile, FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ': ' + Msg);
  finally
    CloseFile(LogFile);
  end;
end;

end.
