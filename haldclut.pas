unit haldclut;

   interface

   uses System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs;

   procedure apply(img, hald_clut: TBitmap);

   implementation

   uses System.Math, System.UIConsts;

   type

     TClutColors = array[0..7] of TAlphaColor;  // Assuming TAlphaColor is already defined

 {
   TRGBColor = record
    Red: Byte;
    Green: Byte;
    Blue: Byte;
  end;
  }

 //not needed, not really tested
 {
 function ConvertRGBToCIELAB(rgbColor: TAlphaColor): TAlphaColor;
var
  r, g, b, X, Y, Z, Xn, Yn, Zn, fx, fy, fz, L, a: Single;
begin
  // Convert RGB to XYZ
  r := TAlphaColorRec(rgbColor).R / 255;
  g := TAlphaColorRec(rgbColor).G / 255;
  b := TAlphaColorRec(rgbColor).B / 255;

  if r > 0.04045 then r := Power((r + 0.055) / 1.055, 2.4) else r := r / 12.92;
  if g > 0.04045 then g := Power((g + 0.055) / 1.055, 2.4) else g := g / 12.92;
  if b > 0.04045 then b := Power((b + 0.055) / 1.055, 2.4) else b := b / 12.92;

  X := r * 0.4124 + g * 0.3576 + b * 0.1805;
  Y := r * 0.2126 + g * 0.7152 + b * 0.0722;
  Z := r * 0.0193 + g * 0.1192 + b * 0.9505;

  Xn := 0.95047;
  Yn := 1.0;
  Zn := 1.08883;

  // Convert XYZ to CIELAB
  if X / Xn > 0.008856 then fx := Power(X / Xn, 1 / 3) else fx := (7.787 * (X / Xn)) + (16 / 116);
  if Y / Yn > 0.008856 then fy := Power(Y / Yn, 1 / 3) else fy := (7.787 * (Y / Yn)) + (16 / 116);
  if Z / Zn > 0.008856 then fz := Power(Z / Zn, 1 / 3) else fz := (7.787 * (Z / Zn)) + (16 / 116);

  L := (116 * fy) - 16;
  a := 500 * (fx - fy);
  b := 200 * (fy - fz);

  // Convert CIELAB to TAlphaColor
  //Result := TAlphaColor(Round(L), Round(a + 128), Round(b + 128), 255);
  //Result := MakeColor(Round(L), Round(a + 128), Round(b + 128));
   Result := MakeColor(Round(L), Round(a + 128), Round(b + 128));
end;

function ApplyGammaCorrection(color: TAlphaColor; gamma: Double): TAlphaColor;
var
  r, g, b, a: Byte;
begin
  // Extract color channels
  a := TAlphaColorRec(color).A;
  r := TAlphaColorRec(color).R;
  g := TAlphaColorRec(color).G;
  b := TAlphaColorRec(color).B;

  // Apply gamma correction
  r := Round(255 * Power(r / 255, gamma));
  g := Round(255 * Power(g / 255, gamma));
  b := Round(255 * Power(b / 255, gamma));

  // Reassemble color
  Result := TAlphaColor((a shl 24) or (r shl 16) or (g shl 8) or b);
end;

function ApplyInverseGammaCorrection(color: TAlphaColor; gamma: Double): TAlphaColor;
var
  r, g, b, a: Byte;
begin
  // Extract color channels
  a := TAlphaColorRec(color).A;
  r := TAlphaColorRec(color).R;
  g := TAlphaColorRec(color).G;
  b := TAlphaColorRec(color).B;

  // Apply inverse gamma correction
  r := Round(255 * Power(r / 255, 1 / gamma));
  g := Round(255 * Power(g / 255, 1 / gamma));
  b := Round(255 * Power(b / 255, 1 / gamma));

  // Reassemble color
  Result := TAlphaColor((a shl 24) or (r shl 16) or (g shl 8) or b);
end;


function CubicInterpolate(y0, y1, y2, y3, mu: Double): Double;
var
  a0, a1, a2, a3, mu2: Double;
begin
  mu2 := mu * mu;
  a0 := -0.5 * y0 + 1.5 * y1 - 1.5 * y2 + 0.5 * y3;
  a1 := y0 - 2.5 * y1 + 2 * y2 - 0.5 * y3;
  a2 := -0.5 * y0 + 0.5 * y2;
  a3 := y1;

  Result := (a0 * mu * mu2 + a1 * mu2 + a2 * mu + a3);
end;


function ApplyGamma(Color: TAlphaColor; Gamma: Double): TAlphaColor;
var
  r, g, b: Byte;
begin
  r := Round(255 * Power(TAlphaColorRec(Color).R / 255, Gamma));
  g := Round(255 * Power(TAlphaColorRec(Color).G / 255, Gamma));
  b := Round(255 * Power(TAlphaColorRec(Color).B / 255, Gamma));
  Result := TAlphaColor((TAlphaColorRec(Color).A shl 24) or (r shl 16) or (g shl 8) or b);
end;

function ApplyInversion(Color: TAlphaColor): TAlphaColor;
var
  r, g, b: Byte;
begin
  r := 255 - TAlphaColorRec(Color).R;
  g := 255 - TAlphaColorRec(Color).G;
  b := 255 - TAlphaColorRec(Color).B;
  Result := TAlphaColor((TAlphaColorRec(Color).A shl 24) or (r shl 16) or (g shl 8) or b);
end;

}


function Clamp(value, min, max: Integer): Integer;
begin
  if value < min then
    Result := min
  else if value > max then
    Result := max
  else
    Result := value;
end;

function Interpolate(color1, color2: TAlphaColor; t: Double): TAlphaColor;
var
  r, g, b, a: Double;
begin
  r := TAlphaColorRec(color1).R * (1.0 - t) + TAlphaColorRec(color2).R * t;
  g := TAlphaColorRec(color1).G * (1.0 - t) + TAlphaColorRec(color2).G * t;
  b := TAlphaColorRec(color1).B * (1.0 - t) + TAlphaColorRec(color2).B * t;
  a := TAlphaColorRec(color1).A * (1.0 - t) + TAlphaColorRec(color2).A * t;

  // Clamp and convert to Byte at the end
  Result := TAlphaColor(Clamp(Round(a), 0, 255) shl 24 or
                        Clamp(Round(r), 0, 255) shl 16 or
                        Clamp(Round(g), 0, 255) shl 8 or
                        Clamp(Round(b), 0, 255));
end;

function TrilinearInterpolate(fracR, fracG, fracB: Double; clutPoints: array of TAlphaColor): TAlphaColor;
var
  c00, c01, c10, c11, c0, c1: TAlphaColor;
begin
  // Ensure fraction values are in the range [0, 1)
  fracR := fracR - Floor(fracR);
  fracG := fracG - Floor(fracG);
  fracB := fracB - Floor(fracB);

  // Interpolate along the R axis

  // Use the updated Interpolate function which maintains high precision
  c00 := Interpolate(clutPoints[0], clutPoints[1], fracR);
  c01 := Interpolate(clutPoints[2], clutPoints[3], fracR);
  c10 := Interpolate(clutPoints[4], clutPoints[5], fracR);
  c11 := Interpolate(clutPoints[6], clutPoints[7], fracR);

  // Interpolate along the G axis
  c0 := Interpolate(c00, c01, fracG);
  c1 := Interpolate(c10, c11, fracG);

  // Finally, interpolate along the B axis
  Result := Interpolate(c0, c1, fracB);
end;

function GetPixelFromCLUT(var clutBitmapData: TBitmapData; x, y, z, range, w, h: Integer): TPoint;
var
  idx: Integer;
begin
  // Assuming the LUT is unwrapped row by row
  idx := x + range*(y + range*z);
  Result.X := idx mod w; // Calculate the column of the CLUT, offset by the full rows
  Result.Y := Min(idx div w, h - 1); // Calculate which row of the CLUT this color is on
end;

function FetchClutColors(var clutBitmapData: TBitmapData; range, w, h: Integer; r, g, b: Double): TClutColors;
var
  x, y, z, xNext, yNext, zNext: Integer;
  point: TPoint;
begin
  x := Floor(r);
  y := Floor(g);
  z := Floor(b);
  xNext := Min(x + 1, range - 1);
  yNext := Min(y + 1, range - 1);
  zNext := Min(z + 1, range - 1);

  point := GetPixelFromCLUT(clutBitmapData, x, y, z, range, w, h);
  Result[0] := clutBitmapData.GetPixel(point.X, point.Y);

  point := GetPixelFromCLUT(clutBitmapData, xNext, y, z, range, w, h);
  Result[1] := clutBitmapData.GetPixel(point.X, point.Y);

  point := GetPixelFromCLUT(clutBitmapData, x, yNext, z, range, w, h);
  Result[2] := clutBitmapData.GetPixel(point.X, point.Y);

  point := GetPixelFromCLUT(clutBitmapData, xNext, yNext, z, range, w, h);
  Result[3] := clutBitmapData.GetPixel(point.X, point.Y);

  point := GetPixelFromCLUT(clutBitmapData, x, y, zNext, range, w, h);
  Result[4] := clutBitmapData.GetPixel(point.X, point.Y);

  point := GetPixelFromCLUT(clutBitmapData, xNext, y, zNext, range, w, h);
  Result[5] := clutBitmapData.GetPixel(point.X, point.Y);

  point := GetPixelFromCLUT(clutBitmapData, x, yNext, zNext, range, w, h);
  Result[6] := clutBitmapData.GetPixel(point.X, point.Y);

  point := GetPixelFromCLUT(clutBitmapData, xNext, yNext, zNext, range, w, h);
  Result[7] := clutBitmapData.GetPixel(point.X, point.Y);
end;


 procedure apply(img, hald_clut: TBitmap);
 var
  x, y: Integer;
  srcBitmapData, clutBitmapData: TBitmapData;
  srcPixel, finalColor: TAlphaColor;
  range, baseX, baseY, baseZ: Integer;
  fracR, fracG, fracB: Double;
  r, g, b: Double; // Declare r, g, b as Double
  //clutPoints: array[0..7] of TAlphaColor;
  clutPoints: TClutColors;
begin
  if (img = nil) or (hald_clut = nil) then
  begin
    ShowMessage('Load both image and CLUT first.');
    Exit;
  end;

  range := Round(Power(hald_clut.Width*hald_clut.Height, 1 / 3));

  if img.Map(TMapAccess.ReadWrite, srcBitmapData) and hald_clut.Map(TMapAccess.Read, clutBitmapData) then
  try
    for y := 0 to img.Height - 1 do
    begin
      for x := 0 to img.Width - 1 do
      begin
        srcPixel := srcBitmapData.GetPixel(x, y);

        // Normalize and scale RGB values to the range [0, range - 1)
        r := (TAlphaColorRec(srcPixel).R / 255.0) * (range - 1);
        g := (TAlphaColorRec(srcPixel).G / 255.0) * (range - 1);
        b := (TAlphaColorRec(srcPixel).B / 255.0) * (range - 1);

        // Calculate the fractional part of the r, g, b values
        fracR := Frac(r);
        fracG := Frac(g);
        fracB := Frac(b);

        // Get the surrounding colors from the CLUT
        clutPoints := FetchClutColors(clutBitmapData, range, hald_clut.Width, hald_clut.Height, r, g, b);

        // Perform trilinear interpolation
        finalColor := TrilinearInterpolate(fracR, fracG, fracB, clutPoints);

        {// Apply simple inversion
        with TAlphaColorRec(finalColor) do
        begin
          R := 255 - R;
          G := 255 - G;
          B := 255 - B;
          finalColor := TAlphaColor(R or (G shl 8) or (B shl 16) or (A shl 24));
        end;}

        // Set the final color to the pixel
        srcBitmapData.SetPixel(x, y, finalColor);
      end;
    end;
  finally
    img.Unmap(srcBitmapData);
    hald_clut.Unmap(clutBitmapData);
  end;

end; //apply

end.
