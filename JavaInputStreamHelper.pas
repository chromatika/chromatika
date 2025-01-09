unit JavaInputStreamHelper;

interface

uses
  Androidapi.JNIBridge,          //For TJavaArray
  Androidapi.JNI.JavaTypes, // For JInputStream
  System.SysUtils,         // For exception handling
  System.Classes;          // For TFileStream

type
  TJavaInputStreamHelper = class
    class procedure SaveToMemoryStream(Stream: JInputStream; MemoryStream: TMemoryStream);
  end;

implementation

class procedure TJavaInputStreamHelper.SaveToMemoryStream(Stream: JInputStream; MemoryStream: TMemoryStream);
const
  BUFFER_SIZE = 1024;
var
  JavaBuffer: TJavaArray<Byte>;
  BytesRead: Integer;
begin
  // create a Java byte[] array of size 1024
  JavaBuffer := TJavaArray<Byte>.Create(BUFFER_SIZE);

  repeat
    // read into the Java array
    BytesRead := Stream.read(JavaBuffer, 0, BUFFER_SIZE);
    if BytesRead > 0 then
    begin
      // write those bytes to the Delphi TMemoryStream
      MemoryStream.WriteBuffer(JavaBuffer.Data^, BytesRead);
    end;
  until BytesRead < 0;
end;

end.
