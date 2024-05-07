unit helpers;

interface
  uses FMX.Graphics; (* for TBitmap *)

  function calculateScaleFactor(img: TBitmap; maxWidth, maxHeight: Single): Single;

implementation

function calculateScaleFactor(img: TBitmap; maxWidth, maxHeight: Single): Single;
var ScaleFactorW, ScaleFactorH, ScaleFactor: Single;
begin
  ScaleFactorW := maxWidth / img.Width;
  ScaleFactorH := maxHeight / img.Height;

  if ScaleFactorW < ScaleFactorH then
    ScaleFactor := ScaleFactorW
 else
    ScaleFactor := ScaleFactorH;

  Result := ScaleFactor;
end;

end.
