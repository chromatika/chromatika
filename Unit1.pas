unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Controls.Presentation,
  FMX.Layouts,      // for TLayout
  System.Threading, // for TProc
  FMX.Gestures,    // for TGestureManager
{$IFDEF ANDROID}
 Androidapi.JNI.GraphicsContentViewText, Androidapi.Helpers, FMX.Helpers.Android, Androidapi.JNI.Net, //for sharing from gallery
  Androidapi.JNI.JavaTypes, // For JInputStream and similar
  Androidapi.JNIBridge, // For IJavaObject
  Androidapi.JNI.Provider, // Required for ContentResolver
  FMX.Surfaces,            // For TBitmapSurface
  System.IOUtils,         // For file path utilities
  JavaInputStreamHelper,
{$ENDIF}
   mobile, System.Notification; //for TNotification;

type
  TForm1 = class(TForm)
    btnchooseHald: TButton;
    btnChoose: TButton;
    btnApply: TButton;
    btnSave: TButton;
    Image1: TImage;
    original: TRadioButton;
    chrome: TRadioButton;
    warm: TRadioButton;
    cool: TRadioButton;
    landscape: TRadioButton;
    AniIndicator1: TAniIndicator;
    NotificationCenter1: TNotificationCenter;
    GestureManager1: TGestureManager;
    btnRotate: TButton;
    procedure LoadSharedImage(SharedUri: Jnet_Uri);
    function GetCurrentLUT: TBitmap;
    procedure ExecuteInBackground(TaskProc: TProc; OnCompletion: TProc);
    procedure DisableRadioButtons(Disable: Boolean);
    function  GetProcessedBitmap: TBitmap;
    procedure CacheProcessedBitmap(var Bitmap: TBitmap);
    procedure UpdateUI(Bitmap: TBitmap);
    procedure scaleAndShow;
    procedure setimage(bmp: TBitmap);
    procedure RadioButtonChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnChooseClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    //procedure rearrange;
    procedure Image1Gesture(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
    procedure btnRotateClick(Sender: TObject);
    procedure ReGenerateDisplay;
    procedure DisplayPreview(const ASource: TBitmap);
  private
    { Private declarations }
    LUTChrome: TBitmap;
    LUTCool: TBitmap;
    LUTLandscape: TBitmap;
    LUTWarm: TBitmap;

    MobileService: TMobileService;

    hald_clut, img, smimg, tempBitmap, prChrome, prWarm, prCool, prLandscape: TBitmap;
    ScreenWidth, ScreenHeight: Single;
    MaxPreviewWidth: Single;
    MaxPreviewHeight: Single;
    layImage, layOriginal, layChrome, layWarm, layCool, layLandscape: TLayout;
    //gesture
    FLastDistance: Integer;
    FLastTouch: TPointF;
    FInitialWidth: Single;
    FInitialHeight: Single;
    FRotationAngle: Integer; // can be 0, 90, 180, 270
    FJobToken: Integer;
    // to preserve zoom state
    FCurrentScale: Single;
    FCurrentOffsetX: Single;
    FCurrentOffsetY: Single;
  public
    { Public declarations }
  end;



const
  offsetSmall = 20;
  offsetBig   = 50;
  offsetTiny  = 5;
  offsetHalf  = 2;
  offsetQuart = 4;
var
  Form1: TForm1;

implementation
  uses
   {$IFDEF MSWINDOWS}

     FMX.Surfaces,// for TBitmapSurface
   {$ENDIF}
  //System.Notification (*for tnotification *),
   System.Math, //for min
   haldclut, helpers, log;

type
  // EXIF orientation values per spec:
  // 1 = normal, 3 = 180 deg, 6 = 90 deg CW, 8 = 270 deg CW, etc.
  // (2,4,5,7 also exist for mirrored flips, but often not used by phones).
  TExifOrientation = 1..8;

{$R *.fmx}


function RotateBitmapFMX(const ASrc: TBitmap; Angle: Single): TBitmap;
begin
  // Create a copy so as not to modify the original
  Result := TBitmap.Create;
  try
    Result.Assign(ASrc);
    // Rotates the image in place, automatically resizing as needed
    // This is part of FMX.Graphics.TBitmap
    Result.Rotate(Angle);
  except
    FreeAndNil(Result);
    raise;
  end;
end;



function LowWord(const AValue: Cardinal): Word; inline;
begin
  Result := Word(AValue and $FFFF);
end;


// A minimal routine to parse the first IFD in the Exif segment
function InternalFindOrientationTag(const ExifData: TBytes): Word;
var
  IsLittleEndian: Boolean;
  OffsetBase: Integer;  // start of TIFF header
  IFDOffset: Cardinal;
  Count: Word;
  i: Integer;
  TagID, DataFormat: Word;
  NumComponents: Cardinal;
  ValueOffset: Cardinal;

  function GetWordB(pos: Integer): Word;
  begin
    // read 2 bytes from ExifData[pos..pos+1]
    if IsLittleEndian then
      Result := Word(ExifData[pos]) or (Word(ExifData[pos+1]) shl 8)
    else
      Result := Word(ExifData[pos+1]) or (Word(ExifData[pos]) shl 8);
  end;

  function GetCardinalB(pos: Integer): Cardinal;
  begin
    if IsLittleEndian then
    begin
      Result := (Cardinal(ExifData[pos])      ) or
                (Cardinal(ExifData[pos+1]) shl 8 ) or
                (Cardinal(ExifData[pos+2]) shl 16) or
                (Cardinal(ExifData[pos+3]) shl 24);
    end
    else
    begin
      Result := (Cardinal(ExifData[pos+3])      ) or
                (Cardinal(ExifData[pos+2]) shl 8 ) or
                (Cardinal(ExifData[pos+1]) shl 16) or
                (Cardinal(ExifData[pos])   shl 24);
    end;
  end;

begin
  Result := 1; // default
  if Length(ExifData) < 12 then
    Exit;
  // The Exif header: "Exif00" + TIFF header at offset 6
  // Byte order at ExifData[6..7], then 0x002A at [8..9], then IFD offset at [10..13]
  // Check byte order
  IsLittleEndian := ((ExifData[6] = Ord('I')) and (ExifData[7] = Ord('I')));
  // The offset to the start of the TIFF data is 6 bytes after "Exif00"
  OffsetBase := 6;
  // Check 0x002A
  if GetWordB(OffsetBase + 2) <> $002A then
    Exit;
  // The offset to IFD0
  IFDOffset := GetCardinalB(OffsetBase + 4);
  if (OffsetBase + Integer(IFDOffset) + 2) >= Length(ExifData) then
    Exit;

  // Number of directory entries in IFD0
  Count := GetWordB(OffsetBase + IFDOffset);
  if (OffsetBase + IFDOffset + 2 + (Count * 12)) > Cardinal(Length(ExifData)) then
    Exit;

  // Loop over the IFD entries
  for i := 0 to Count - 1 do
  begin
    // Each entry is 12 bytes: TagID(2), DataFormat(2), NumComponents(4), ValueOffset(4)
    TagID := GetWordB(OffsetBase + IFDOffset + 2 + (12*i));
    DataFormat := GetWordB(OffsetBase + IFDOffset + 2 + (12*i) + 2);
    NumComponents := GetCardinalB(OffsetBase + IFDOffset + 2 + (12*i) + 4);
    ValueOffset := GetCardinalB(OffsetBase + IFDOffset + 2 + (12*i) + 8);

    // Orientation tag = 0x0112
    if TagID = $0112 then
    begin
      // If DataFormat=3 => it's SHORT (2 bytes). We only expect 1 component.
      // The orientation value might be in the ValueOffset field if it fits 2 bytes.
      if (DataFormat = 3) and (NumComponents = 1) then
      begin
        // If 2 bytes can fit in that offset, we read directly
        // In EXIF, if the data is <=4 bytes, the "ValueOffset" can store the data itself
        if (ValueOffset <= $FFFF) then
          Result := LowWord(ValueOffset)  // orientation
        else
        begin
          // Otherwise need to read from that offset in the file.
          // For brevity, we skip that.
          // Usually orientation fits in 2 bytes so we can read directly from ValueOffset.
        end;
        Exit;
      end;
    end;
  end;
end;


function ReadExifOrientationFromJPEG(const AStream: TStream): TExifOrientation;
var
  Marker: Word;
  Size: Word;
  Segment: TBytes;
  i: Integer;
  Tag: Word;
  Format: Word;
  Components: Cardinal;
  Orientation: Word;
  StartPos: Int64;
begin
  Result := 1; // default: no rotation needed

  // Must start at beginning
  AStream.Position := 0;

  // Check JPEG SOI marker FF D8
  AStream.ReadBuffer(Marker, 2);
  Marker := Swap(Marker);
  if Marker <> $FFD8 then
    Exit; // not a JPEG or we can't parse

  // Repeatedly read next marker
  while AStream.Position < AStream.Size do
  begin
    // Markers are 0xFF??. We read 2 bytes:
    if AStream.Read(Marker, 2) <> 2 then
      Exit;
    Marker := Swap(Marker);

    if (Marker and $FF00) <> $FF00 then
      Exit; // not a well-formed marker

    if (Marker = $FFE1) then
    begin
      // APP1 segment => might contain Exif
      // Read segment length
      if AStream.Read(Size, 2) <> 2 then
        Exit;
      Size := Swap(Size);
      if Size < 2 then
        Exit;
      // Read entire segment
      SetLength(Segment, Size - 2);
      if AStream.Read(Segment[0], Size - 2) <> (Size - 2) then
        Exit;

      // Check for "Exif" + zero + "MM" or "II"
      // Typically: ASCII "Exif" 00 00 then TIFF header
      if (Length(Segment) < 12) or
         not ((Segment[0] = Ord('E')) and
              (Segment[1] = Ord('x')) and
              (Segment[2] = Ord('i')) and
              (Segment[3] = Ord('f')) and
              (Segment[4] = 0) and
              (Segment[5] = 0)) then
        continue; // not actually EXIF

      // The next 2 bytes are either "II" or "MM" for byte order
      // For simplicity, handle only "MM" (Motorola, big-endian) or "II" (Intel, little-endian).
      // We'll do a partial parse for orientation.
      // More robust code might handle offsets properly.
      // This minimal approach finds the Orientation tag in the first IFD.
      // You can study full EXIF docs for more correctness.

      // We'll call a small function to parse the IFD and find orientation:
      Orientation := 1;
      Orientation := InternalFindOrientationTag(Segment);
      // If found
      if Orientation in [1..8] then
        Exit(Orientation); // done
    end
    else if (Marker = $FFD9) then
    begin
      // EOI: end of image
      Exit;
    end
    else
    begin
      // skip segment
      if AStream.Read(Size, 2) <> 2 then
        Exit;
      Size := Swap(Size);
      if (Size < 2) or (AStream.Position + (Size - 2) > AStream.Size) then
        Exit;
      AStream.Position := AStream.Position + (Size - 2);
    end;
  end;
end;





// this would arrange the image height so that image would be maximally scaled
// but this is not usable because then the radio buttons are not visible
{
procedure TForm1.rearrange;
begin

  layImage.Width := ScreenWidth - offsetHalf;
  layImage.Height := smimg.Height;
//  layImage.SetBounds(0, 0, ScreenWidth - offsetHalf, ScreenHeight / 2);  // Adjust size to match where you want Image1
  layImage.SetBounds(0, 0, ScreenWidth - offsetHalf, smimg.Height);
  layOriginal.Position.X := offsetSmall; layOriginal.Position.Y := layImage.Height + offsetSmall;
  layOriginal.Height := offsetBig;

  layChrome.Position.X := offsetSmall; layChrome.Position.Y := layOriginal.Position.Y + offsetBig;
  layChrome.Height := offsetBig;
  layWarm.Position.X := offsetSmall; layWarm.Position.Y := layChrome.Position.Y + offsetBig;
  layWarm.Height := offsetBig;
  layCool.Position.X := offsetSmall; layCool.Position.Y := layWarm.Position.Y + offsetBig;
  layCool.Height := offsetBig;
  layLandscape.Position.X := offsetSmall; layLandscape.Position.Y := layCool.Position.Y + offsetBig;
  layCool.Height := offsetBig;

end;
 }
{ this works well}
{
procedure TForm1.Image1Gesture(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
var
  LImageCenter: TPointF;
  Scale: Single;
  NewPosition: TPointF;
begin
  case EventInfo.GestureID of
    igiZoom:
      begin
        if TInteractiveGestureFlag.gfBegin in EventInfo.Flags then
          FLastDistance := EventInfo.Distance;

        if not (TInteractiveGestureFlag.gfBegin in EventInfo.Flags) and
           not (TInteractiveGestureFlag.gfEnd in EventInfo.Flags) then
        begin
          LImageCenter := PointF(Image1.Width / 2, Image1.Height / 2);
          Scale := EventInfo.Distance / FLastDistance;
          Image1.Width := Image1.Width * Scale;
          Image1.Height := Image1.Height * Scale;
          Image1.Position.X := (Image1.Position.X + LImageCenter.X) - (Image1.Width / 2);
          Image1.Position.Y := (Image1.Position.Y + LImageCenter.Y) - (Image1.Height / 2);
          FLastDistance := EventInfo.Distance;
        end;

        Handled := True;
      end;
    igiPan:
      begin
        if TInteractiveGestureFlag.gfBegin in EventInfo.Flags then
        begin
          FLastTouch := EventInfo.Location;
        end;

        if not (TInteractiveGestureFlag.gfBegin in EventInfo.Flags) and
           not (TInteractiveGestureFlag.gfEnd in EventInfo.Flags) then
        begin
          NewPosition.X := Image1.Position.X + (EventInfo.Location.X - FLastTouch.X);
          NewPosition.Y := Image1.Position.Y + (EventInfo.Location.Y - FLastTouch.Y);

          // Boundary checks
          if NewPosition.X > 0 then NewPosition.X := 0;
          if NewPosition.Y > 0 then NewPosition.Y := 0;
          if NewPosition.X + Image1.Width < layImage.Width then NewPosition.X := layImage.Width - Image1.Width;
          if NewPosition.Y + Image1.Height < layImage.Height then NewPosition.Y := layImage.Height - Image1.Height;

          Image1.Position.X := NewPosition.X;
          Image1.Position.Y := NewPosition.Y;
          FLastTouch := EventInfo.Location;
        end;

        Handled := True;
      end;
  end;
end;
}

procedure TForm1.Image1Gesture(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  case EventInfo.GestureID of
    igiZoom:
      begin
        if TInteractiveGestureFlag.gfBegin in EventInfo.Flags then
          FLastDistance := EventInfo.Distance
        else if not (TInteractiveGestureFlag.gfEnd in EventInfo.Flags) then
        begin
          // Calculate scale factor
          var ScaleFactor := EventInfo.Distance / FLastDistance;
          var NewWidth  := Image1.Width * ScaleFactor;
          var NewHeight := Image1.Height * ScaleFactor;
          // Enforce minimum zoom (can’t go smaller than initial)
          if (NewWidth >= FInitialWidth) and (NewHeight >= FInitialHeight) then
          begin
            // Pivot around pinch center
            var gestureX := (EventInfo.Location.X - Image1.Position.X) / Image1.Width;
            var gestureY := (EventInfo.Location.Y - Image1.Position.Y) / Image1.Height;
            var offsetX  := gestureX * (Image1.Width - NewWidth);
            var offsetY  := gestureY * (Image1.Height - NewHeight);
            // Apply new size and position
            Image1.Width  := NewWidth;
            Image1.Height := NewHeight;
            Image1.Position.X := Image1.Position.X + offsetX;
            Image1.Position.Y := Image1.Position.Y + offsetY;
          end;
          FLastDistance := EventInfo.Distance;
        end;
        Handled := True;
      end;
    igiPan:
      begin
        if TInteractiveGestureFlag.gfBegin in EventInfo.Flags then
          FLastTouch := EventInfo.Location
        else if not (TInteractiveGestureFlag.gfEnd in EventInfo.Flags) then
        begin
          // Calculate delta
          var dx := EventInfo.Location.X - FLastTouch.X;
          var dy := EventInfo.Location.Y - FLastTouch.Y;
          var NewPosX := Image1.Position.X + dx;
          var NewPosY := Image1.Position.Y + dy;
          // Clamp within bounds
          NewPosX := Max(layImage.Width - Image1.Width, Min(NewPosX, 0));
          NewPosY := Max(layImage.Height - Image1.Height, Min(NewPosY, 0));
          Image1.Position.X := NewPosX;
          Image1.Position.Y := NewPosY;
          FLastTouch := EventInfo.Location;
        end;
        Handled := True;
      end;
    igiDoubleTap:
      begin
        // Reset zoom to base (initial size)
        Image1.Width  := FInitialWidth;
        Image1.Height := FInitialHeight;
        Image1.Position.X := (layImage.Width - FInitialWidth) / 2;
        Image1.Position.Y := (layImage.Height - FInitialHeight) / 2;
        Handled := True;
      end;
  end;
end;

procedure TForm1.ExecuteInBackground(TaskProc: TProc; OnCompletion: TProc);
var
  LocalToken: Integer;
begin
  INC(FJobToken);
  LocalToken := FJobToken;
  TTask.Run(procedure
    begin
      try
        if Assigned(TaskProc) then
          TaskProc();  // execute the passed procedure on a background thread
        TThread.Queue(nil, procedure
          begin
            if LocalToken <> FJobToken then
            begin
              Exit;
            end;
            if Assigned(OnCompletion) then
              OnCompletion();  // execute completion on main thread
          end);
      except
        on E: Exception do
          TThread.Queue(nil, procedure
            begin
              if LocalToken <> FJobToken then begin
                Exit;
              end;
              ShowMessage('Error during background execution: ' + E.Message);
            end);
      end;
    end);
end;

procedure TForm1.scaleAndShow;
var
  ScaleFactor, ScaleWidth, ScaleHeight, HighResFactor: Single;
begin
  if Assigned(smimg) then FreeAndNil(smimg);
  smimg := TBitmap.Create;
  HighResFactor := 2.0;  // adjust this value to increase the internal resolution of the image
  try
    // calculate individual scale factors for width and height
    ScaleWidth := layImage.Width / img.Width;
    ScaleHeight := layImage.Height / img.Height;
    // use the smaller scale factor to ensure the entire image fits within the bounds
    ScaleFactor := Min(ScaleWidth, ScaleHeight) * HighResFactor;
    // set the size of the scaled image
    smimg.SetSize(Round(img.Width * ScaleFactor), Round(img.Height * ScaleFactor));
    // render the scaled image
    smimg.Canvas.BeginScene;
    try
      smimg.Canvas.DrawBitmap(img, RectF(0, 0, img.Width, img.Height), RectF(0, 0, smimg.Width, smimg.Height), 1);
    finally
      smimg.Canvas.EndScene;
    end;
    // assign the scaled image to Image1 and adjust its display size to fit within layImage
    Image1.Bitmap.Assign(smimg);
    Image1.Width := Round(smimg.Width / HighResFactor);
    Image1.Height := Round(smimg.Height / HighResFactor);
    // center Image1 within layImage
    Image1.Position.X := (layImage.Width - Image1.Width) / 2;
    //Image1.Position.Y := 0;
    Image1.Position.Y := (layImage.Height - Image1.Height) /2;
    original.IsChecked := true;
  except
    on E: Exception do
      ShowMessage('Error during image scaling: ' + E.Message);
  end;
end;

{
procedure TForm1.setimage(bmp: TBitmap);
var
  ScaleFactor: Single;
begin
  //Image1.Bitmap.Assign(bmp);
  if Assigned(bmp) then
  begin
    // Free existing bitmap if it exists
    if Assigned(img) then FreeAndNil(img);
    // Create a new bitmap and assign the image from the action
    img := TBitmap.Create;
    img.Assign(bmp);
      MaxPreviewWidth := Min(ScreenWidth*2, img.Width);
      MaxPreviewHeight := Min(ScreenHeight*2, img.Height);
      FLastTouch := PointF(0, 0); FLastDistance := 0;
    scaleAndShow;
    //rearrange;
  end;
end;
 }
{
procedure TForm1.setimage(bmp: TBitmap);
begin
  if Assigned(bmp) then
  begin
    if Assigned(img) then FreeAndNil(img);
    img := TBitmap.Create;
    img.Assign(bmp);
    // Assume MaxPreviewWidth and MaxPreviewHeight refer to the size constraints
    MaxPreviewWidth := Min(ScreenWidth*2, img.Width);
    MaxPreviewHeight := Min(ScreenHeight*2, img.Height);

    FLastTouch := PointF(0, 0);  // Reset touch position
    FLastDistance := 0;  // Reset zoom distance
    scaleAndShow;  // Apply scaling and display
  end;
end;
}
procedure TForm1.setimage(bmp: TBitmap);
begin
  //ShowMessage(Format('setimage called. bmp size: %dx%d', [bmp.Width, bmp.Height]));
  if Assigned(bmp) then
  begin
    if Assigned(img) then FreeAndNil(img);
    img := TBitmap.Create;
    img.Assign(bmp);
    // calculate the preview sizes based on the maximum allowable dimensions
    MaxPreviewWidth := Min(ScreenWidth * 2, img.Width);
    MaxPreviewHeight := Min(ScreenHeight * 2, img.Height);
    // reset zoom and pan settings
    FLastTouch := PointF(0, 0);
    FLastDistance := 0;
    // apply scaling and display the image
    scaleAndShow;
     //ShowMessage(Format('scaleAndShow done. Image1 size: %dx%d', [Round(Image1.Width), Round(Image1.Height)]));
    FInitialWidth := Image1.Width;
    FInitialHeight := Image1.Height;
  end;
end;


procedure TForm1.btnChooseClick(Sender: TObject);
{$IFDEF MSWINDOWS}
var
  OpenDialog: TOpenDialog;
{$ENDIF}
begin
  INC(FJobToken);
  if Assigned(prChrome) then begin FreeAndNil(prChrome) end;
  if Assigned(prWarm)   then begin FreeAndNil(prWarm)   end;
  if Assigned(prCool)   then begin FreeAndNil(prCool)   end;
  if Assigned(prLandscape) then begin FreeAndNil(prLandscape) end;

 {$IFDEF ANDROID or IOS}
  MobileService.choose(setimage);
 {$ENDIF}

 {$IFDEF MSWINDOWS}
  OpenDialog := TOpenDialog.Create(nil);
  try
    OpenDialog.Filter := 'Image Files|*.jpg;*.png';
    if OpenDialog.Execute then
    begin
    if Assigned(img) then FreeAndNil(img);
    // Create a new bitmap and assign the image from the action
    img := TBitmap.Create;
    img.LoadFromFile(OpenDialog.FileName);
    scaleAndShow;

    // Display this image on the form's TImage control
    //Image1.Bitmap.Assign(img);
    // Update UI elements

    end;
  finally
    OpenDialog.Free;
  end;

 {$ENDIF}
end;

function TForm1.GetCurrentLUT: TBitmap;
begin
  Result := nil;
  if chrome.IsChecked then Result := LUTChrome
  else if warm.IsChecked then Result := LUTWarm
  else if cool.IsChecked then Result := LUTCool
  else if landscape.IsChecked then Result := LUTLandscape;
end;

function TForm1.GetProcessedBitmap: TBitmap;
begin
  // return the correct processed bitmap based on the selected LUT
  if chrome.IsChecked then
    Result := prChrome
  else if warm.IsChecked then
    Result := prWarm
  else if cool.IsChecked then
    Result := prCool
  else if landscape.IsChecked then
    Result := prLandscape
  else
    Result := nil;
end;

procedure TForm1.DisplayPreview(const ASource: TBitmap);
var
  RotatedCopy, Preview: TBitmap;
  ScaleWidth, ScaleHeight, ScaleFactor: Single;
begin
  if not Assigned(ASource) then Exit;

  // 1) Rotate a copy if needed
  RotatedCopy := TBitmap.Create;
  try
    RotatedCopy.Assign(ASource);
    if (FRotationAngle mod 360) <> 0 then
      RotatedCopy.Rotate(FRotationAngle mod 360);

    // 2) Scale to fit layImage
    Preview := TBitmap.Create;
    try
      ScaleWidth := layImage.Width / RotatedCopy.Width;
      ScaleHeight := layImage.Height / RotatedCopy.Height;
      ScaleFactor := Min(ScaleWidth, ScaleHeight);

      Preview.SetSize(
        Round(RotatedCopy.Width  * ScaleFactor),
        Round(RotatedCopy.Height * ScaleFactor)
      );

      Preview.Canvas.BeginScene;
      try
        Preview.Canvas.DrawBitmap(
          RotatedCopy,
          RotatedCopy.BoundsF,
          Preview.BoundsF,
          1
        );
      finally
        Preview.Canvas.EndScene;
      end;

      // 3) Show in Image1
      Image1.Bitmap.Assign(Preview);
      Image1.Width  := Preview.Width;
      Image1.Height := Preview.Height;
      Image1.Position.X := (layImage.Width  - Image1.Width)  / 2;
      Image1.Position.Y := (layImage.Height - Image1.Height) / 2;
      // Update the base scale to this new preview size
      FInitialWidth  := Image1.Width;
      FInitialHeight := Image1.Height;
    finally
      Preview.Free;
    end;

  finally
    RotatedCopy.Free;
  end;
end;


procedure TForm1.btnRotateClick(Sender: TObject);
var
  Rotated: TBitmap;
begin
 {
  // Ensure there's an image loaded
  if not Assigned(img) then
  begin
    ShowMessage('No image loaded to rotate.');
    Exit;
  end;

  // Rotate the in-memory TBitmap by +90
  Rotated := RotateBitmapFMX(img, 90);
  try
    // Then display the newly rotated image
    setimage(Rotated);
  finally
    Rotated.Free;
  end;
  }
   if not Assigned(img) then
  begin
    ShowMessage('No image loaded to rotate.');
    Exit;
  end;
  //Increase by 90
  FRotationAngle := (FRotationAngle + 90) mod 360;
  // Re-generate the display
  ReGenerateDisplay;

end;

procedure TForm1.CacheProcessedBitmap(var Bitmap: TBitmap);
begin
  if chrome.IsChecked then
    prChrome := Bitmap
  else if warm.IsChecked then
    prWarm := Bitmap
  else if cool.IsChecked then
    prCool := Bitmap
  else if landscape.IsChecked then
    prLandscape := Bitmap;
end;

procedure TForm1.UpdateUI(Bitmap: TBitmap);
begin
  Image1.Bitmap.Assign(Bitmap);
  AniIndicator1.Visible := False;
  AniIndicator1.Enabled := False;
  DisableRadioButtons(False);
  btnSave.Visible := True;
end;
{
 procedure TForm1.UpdateUI(Bitmap: TBitmap);
begin
  TThread.Queue(nil, procedure
  begin
    if Assigned(Bitmap) then
    begin
      // Clone the bitmap for UI thread safety
      var ClonedBitmap: TBitmap := TBitmap.Create;
      try
        ClonedBitmap.Assign(Bitmap);
        Image1.Bitmap.Assign(ClonedBitmap);
      finally
        ClonedBitmap.Free;
      end;
    end;
    AniIndicator1.Visible := False;
    AniIndicator1.Enabled := False;
    DisableRadioButtons(False);
    btnSave.Visible := True;
  end);
end;
 }
 {
 procedure TForm1.UpdateUI(Bitmap: TBitmap);
begin
  TThread.Queue(nil, procedure
  begin
    if Assigned(Bitmap) then
    begin
      Image1.Bitmap.Assign(Bitmap);
      Image1.Bitmap.BitmapChanged;  // Force redraw
    end;
    AniIndicator1.Visible := False;
    AniIndicator1.Enabled := False;
    btnSave.Visible := True;
  end);
end;
  }
procedure TForm1.DisableRadioButtons(Disable: Boolean);
begin
  chrome.Enabled := not Disable;
  warm.Enabled := not Disable;
  cool.Enabled := not Disable;
  landscape.Enabled := not Disable;
  original.Enabled := not Disable;
end;


procedure TForm1.RadioButtonChange(Sender: TObject);
var
  SelectedLUT, ProcessedBitmap: TBitmap;
  LocalToken: Integer;
  imgData: TBitmapData;
begin
  if not Assigned(img) then Exit; // no base image

  // Bump the concurrency token for any existing tasks
  Inc(FJobToken);
  LocalToken := FJobToken;

  // Show busy indicator only if we actually do a LUT task
  AniIndicator1.Visible := True;
  AniIndicator1.Enabled := True;
  DisableRadioButtons(True);

  SelectedLUT := GetCurrentLUT; // e.g. LUTChrome, etc.
  ProcessedBitmap := GetProcessedBitmap;

  if original.IsChecked or (SelectedLUT = nil) then
  begin
    // =========================
    // “ORIGINAL” OR NO-LUT CASE
    // =========================
    btnSave.Visible := False;

    // Because we do NOT want to run any LUT TTask, just
    // ensure we do not hang on the busy indicator:
    AniIndicator1.Visible := False;
    AniIndicator1.Enabled := False;
    DisableRadioButtons(False);

    // Also, if there's a TTask running from a previous LUT,
    // it will see that its 'LocalToken' != 'FJobToken' when it finishes,
    // so it discards results.

    // Just do rotated display of the “original”:
    ReGenerateDisplay;
    Exit;
  end
  else
  begin
    // ===============
    // LUT SELECTED
    // ===============
    btnSave.Visible := True;

    // If we don't already have a processed preview for the chosen LUT
    if not Assigned(ProcessedBitmap) then
    begin
      // Need to do background LUT
      ProcessedBitmap := TBitmap.Create;
      ProcessedBitmap.Assign(smimg); // smimg is your “original scaled”?

      // Map it
      if ProcessedBitmap.Map(TMapAccess.ReadWrite, imgData) then
      begin
        TTask.Run(procedure
        begin
          try
            haldclut.ApplyRawParallel(imgData, SelectedLUT);

            TThread.Queue(nil, procedure
            begin
              // Check concurrency token
              if LocalToken <> FJobToken then
              begin
                // user changed selection => discard
                ProcessedBitmap.Unmap(imgData);
                ProcessedBitmap.Free;
                Exit;
              end;

              // Valid result
              ProcessedBitmap.Unmap(imgData);
              CacheProcessedBitmap(ProcessedBitmap);
              UpdateUI(ProcessedBitmap);

              // show rotate preview
              ReGenerateDisplay;
            end);

          except
            on E: Exception do
              TThread.Queue(nil, procedure
              begin
                ShowMessage('Error processing image: ' + E.Message);
                ProcessedBitmap.Free;
              end);
          end;
        end);
      end
      else
      begin
        // Map failed
        ProcessedBitmap.Free;
        ShowMessage('Failed to map LUT image data.');
        // Possibly revert to original or do something else
      end;
    end
    else
    begin
      // We already have a processed LUT
      UpdateUI(ProcessedBitmap);
      ReGenerateDisplay;
    end;
  end;
end;

function MakeRotatedCopy(const ASource: TBitmap; AAngle: Single): TBitmap;
begin
  if SameValue(AAngle, 0, 0.01) then
  begin
    Result := TBitmap.Create;
    Result.Assign(ASource);
  end
  else
    Result := RotateBitmapFMX(ASource, AAngle);
end;

{$IF Defined(ANDROID) or Defined(IOS)}


procedure TForm1.btnSaveClick(Sender: TObject);
var
  tmp, tmp2: TBitmap;
  localAngle: Single;
  imgData: TBitmapData;
begin
  if not Assigned(img) then
  begin
    ShowMessage('No image to save');
    Exit;
  end;

  // 1) Figure out which LUT is selected (could be nil if "original" or no LUT).
  if chrome.IsChecked then
    hald_clut := LUTChrome
  else if warm.IsChecked then
    hald_clut := LUTWarm
  else if cool.IsChecked then
    hald_clut := LUTCool
  else if landscape.IsChecked then
    hald_clut := LUTLandscape
  else
    hald_clut := nil;  // "original" / no LUT

  // 2) Show busy indicator + disable UI
  AniIndicator1.Visible := True;
  AniIndicator1.Enabled := True;
  btnSave.Enabled := False;
  btnChoose.Enabled := False;
  original.Enabled := False;
  chrome.Enabled := False;
  warm.Enabled := False;
  cool.Enabled := False;
  landscape.Enabled := False;
  Image1.Enabled := False;
  tmp := TBitmap.Create;
  tmp.Assign(img);
  LocalAngle := FRotationAngle mod 360;
  //tmp := MakeRotatedCopy(img, FRotationAngle mod 360);
  {$IFDEF ANDROID}
  MobileService.RequestWritePermission(
    procedure(PermissionGranted: Boolean)
    begin
      if not PermissionGranted then
      begin
        ShowMessage('Cannot save image without write permission.');
        // Re-enable UI
        AniIndicator1.Visible := False;
        AniIndicator1.Enabled := False;
        btnSave.Enabled := True;
        btnChoose.Enabled := True;
        original.Enabled := True;
        chrome.Enabled := True;
        warm.Enabled := True;
        cool.Enabled := True;
        landscape.Enabled := True;
        Image1.Enabled := True;
        Exit;
      end;

      // If permission is granted, create the TBitmap we want to save:
      //tmp := TBitmap.Create;
      //tmp.Assign(img);

      // If needed, rotate
      //if (FRotationAngle mod 360) <> 0 then
       // tmp.Rotate(FRotationAngle mod 360);

      // 3) If we *do* have a LUT:
      if Assigned(hald_clut) then
      begin
        // run the background LUT
        ExecuteInBackground(
          procedure
          begin


            if tmp.Map(TMapAccess.ReadWrite, imgData) then
              try
                haldclut.ApplyRawParallel(imgData, hald_clut);
              finally
                tmp.Unmap(imgData);
              end;
          end,
          procedure
          begin
            // now do MobileService.save
            //tmp2 := TBitmap.Create;
            tmp2 := MakeRotatedCopy(tmp, localAngle);
            MobileService.save(tmp2,
              procedure(const ASaved: Boolean; const AErrorMessage: string)
              begin
                if ASaved then
                begin
                  //ShowMessage('Image saved OK')
                  //tmp.Free; //here we have a problem
                  //tmp2.Free; // cannot figure out how to free them safely
                end
              else
                begin
                  ShowMessage('Failed to save: ' + AErrorMessage);
                end;

                //tmp.Free; // free after saving
                //tmp2.Free;
                // Re-enable UI
                AniIndicator1.Visible := False;
                AniIndicator1.Enabled := False;
                btnSave.Enabled := True;
                btnChoose.Enabled := True;
                original.Enabled := True;
                chrome.Enabled := True;
                warm.Enabled := True;
                cool.Enabled := True;
                landscape.Enabled := True;
                Image1.Enabled := True;
              end
            );
          end
        );
      end
      else
      begin
        // 4) ELSE => NO LUT. We skip ApplyRaw, but still call MobileService.save
        MobileService.save(tmp,
          procedure(const ASaved: Boolean; const AErrorMessage: string)
          begin
            if ASaved then
              ShowMessage('Image saved OK')
            else
              ShowMessage('Failed to save: ' + AErrorMessage);

            //tmp.Free; // done with the bitmap
            // re-enable UI
            AniIndicator1.Visible := False;
            AniIndicator1.Enabled := False;
            btnSave.Enabled := True;
            btnChoose.Enabled := True;
            original.Enabled := True;
            chrome.Enabled := True;
            warm.Enabled := True;
            cool.Enabled := True;
            landscape.Enabled := True;
            Image1.Enabled := True;
          end
        );
      end;
    end
  );
  {$ELSE}
  // For non-Android platforms, proceed directly
  ExecuteInBackground(
    procedure
    begin
      // Background processing
      tmp.Map(TMapAccess.ReadWrite, imgData);
      haldclut.ApplyRawParallel(imgData, hald_clut);
      tmp.Unmap(imgData);
    end,
    procedure
    begin
      // This is executed in the main thread
      // Your save logic for other platforms
    end
  );
  {$ENDIF}
end;

{$ENDIF}

{$IFDEF MSWINDOWS}
procedure TForm1.btnSaveClick(Sender: TObject);
var
  tmp: TBitmap;
  SaveDialog: TSaveDialog;
  Surface: TBitmapSurface;
  Params: TBitmapCodecSaveParams;
begin
  if chrome.IsChecked then
    hald_clut := LUTChrome
  else if warm.IsChecked then
    hald_clut := LUTWarm
  else if cool.IsChecked then
    hald_clut := LUTCool
  else if landscape.IsChecked then
    hald_clut := LUTLandscape;
  tmp := TBitmap.Create;
  tmp.Assign(img);
  AniIndicator1.Visible := True;
  AniIndicator1.Enabled := True;
            // Perform saving in a synchronized block
            TThread.Synchronize(nil, procedure
              begin
                SaveDialog := TSaveDialog.Create(nil);
                try
                  SaveDialog.Title := 'Save Image As';
                  SaveDialog.Filter := 'PNG Image|*.png|JPEG Image|*.jpg';
                  SaveDialog.DefaultExt := 'png';
                  SaveDialog.FilterIndex := 1;
                  if SaveDialog.Execute then
                  begin
                   haldclut.apply(tmp, hald_clut);
                   tmp.SaveToFile(SaveDialog.FileName);
                  end;
                finally
                  SaveDialog.Free;
                end;
              end);
  AniIndicator1.Visible := False;
  AniIndicator1.Enabled := False;
end;
{$ENDIF}


function HexPreview(MS: TMemoryStream; MaxBytes: Integer = 32): string;
var
  Data: TBytes;
  i: Integer;
begin
  Result := '';
  if not Assigned(MS) or (MS.Size = 0) then Exit('(empty)');
  MS.Position := 0;
  SetLength(Data, MS.Size);
  MS.ReadBuffer(Data[0], MS.Size);
  for i := 0 to Min(MS.Size, MaxBytes) - 1 do
    Result := Result + IntToHex(Data[i], 2) + ' ';
  if MS.Size > MaxBytes then
    Result := Result + '...';
end;

{
procedure TForm1.LoadSharedImage(SharedUri: Jnet_Uri);
var
  InputStream: JInputStream;
  MemoryStream: TMemoryStream;
  Bitmap: TBitmap;
begin
  // memory stream to hold the image data
  MemoryStream := TMemoryStream.Create;
  InputStream := nil;

  try
    // Request the content resolver to open the input stream
    InputStream := TAndroidHelper.Context.getContentResolver.openInputStream(SharedUri);

    if InputStream = nil then
    begin
      ShowMessage('Failed to open InputStream for the shared image. Possibly no permission.');
      Exit;
    end;


    TJavaInputStreamHelper.SaveToMemoryStream(InputStream, MemoryStream);

    //ShowMessage('First bytes of stream: ' + HexPreview(MemoryStream, 32));

    //ShowMessage('MemoryStream.Size = ' + MemoryStream.Size.ToString);
    // for debug
    //MemoryStream.SaveToFile(TPath.Combine(TPath.GetDocumentsPath, 'shared_dump.bin'));

   MemoryStream.Position := 0;


    Bitmap := TBitmap.Create;
    try
      Bitmap.LoadFromStream(MemoryStream);
      setimage(Bitmap);
    finally
      Bitmap.Free;
    end;
  except
    on E: Exception do
    begin

      ShowMessage('Error loading shared image: ' + E.Message);
    end;
  end;

  // cleanup
  MemoryStream.Free;
  if InputStream <> nil then
    InputStream.close;
end;
}


procedure TForm1.LoadSharedImage(SharedUri: Jnet_Uri);
var
  InputStream: JInputStream;
  MemoryStream: TMemoryStream;
  OrientationValue: Integer;
  RawBmp, RotatedBmp: TBitmap;
begin
  MemoryStream := TMemoryStream.Create;
  try
    // 1) Open InputStream
    InputStream := TAndroidHelper.Context.getContentResolver.openInputStream(SharedUri);
    if InputStream = nil then
    begin
      ShowMessage('Failed to open InputStream for the shared image. Possibly no permission.');
      Exit;
    end;

    try
      // 2) Read bytes into MemoryStream
      TJavaInputStreamHelper.SaveToMemoryStream(InputStream, MemoryStream);
    finally
      InputStream.close;
    end;

    // 3) Check orientation from EXIF
    OrientationValue := ReadExifOrientationFromJPEG(MemoryStream);
    // e.g. 1=normal, 3=180°, 6=+90°, 8=+270°

    // 4) Now load the TBitmap from the stream
    MemoryStream.Position := 0;
    RawBmp := TBitmap.Create;
    try
      RawBmp.LoadFromStream(MemoryStream);
    except
      RawBmp.Free;
      raise; // or ShowMessage
    end;

    // 5) If orientation is 3,6,8, rotate
    //showmessage ('orientation: ' + inttostr(orientationvalue));
    case OrientationValue of
      3:  RotatedBmp := RotateBitmapFMX(RawBmp, 180);
      6:  RotatedBmp := RotateBitmapFMX(RawBmp, 90);
      8:  RotatedBmp := RotateBitmapFMX(RawBmp, 270);
    else
      RotatedBmp := nil;  // orientation=1 => no rotate
      //showmessage('rotation not needed');
    end;

    if Assigned(RotatedBmp) then
    begin
      try
        //showmessage ('setting rotated bmp');
        setimage(RotatedBmp);
      finally
        RotatedBmp.Free;
      end;
    end
    else
    begin
      //showmessage ('setting rawbmp');
      setimage(RawBmp);
    end;
    RawBmp.Free;

  finally
    MemoryStream.Free;
  end;
end;


procedure TForm1.ReGenerateDisplay;
var
  SourceBmp: TBitmap;
begin
  // Figure out which big bitmap is the “base” for the user’s current choice
  // If “original” is checked, that’s `img`.
  // Otherwise, a processed LUT is in `prChrome/prWarm/prCool/prLandscape`.
  if original.IsChecked then
    SourceBmp := img
  else
  begin
    SourceBmp := GetProcessedBitmap;
    if not Assigned(SourceBmp) then
      SourceBmp := img; // fallback if LUT not yet generated
  end;

  if not Assigned(SourceBmp) then Exit;

  // Instead of physically rotating or calling setimage,
  // just call the display function with the angle + scale
  DisplayPreview(SourceBmp);
end;



procedure TForm1.FormCreate(Sender: TObject);
var
  ResourceStream: TResourceStream;
  StyleObj: TFmxObject;
{$IFDEF ANDROID}
  Intent: JIntent;
  SharedUri: Jnet_Uri;
  SharedPath: string;
{$ENDIF}
begin
  inherited;
  FRotationAngle := 0;
  FJobToken := 0;
  prChrome := nil;
  prWarm := nil;
  prCool := nil;
  prLandscape := nil;

  btnChoose.Text := 'choose';
  btnApply.Text := 'apply';
  btnSave.Text := 'save';
  btnChooseHald.Visible := false;
  btnApply.Visible := false;
  btnSave.Visible := false;
  hald_clut := TBitmap.Create;

  ScreenWidth := Form1.ClientWidth;
  ScreenHeight := Form1.ClientHeight;

  MaxPreviewWidth := ScreenWidth;//200;
  MaxPreviewHeight := ScreenHeight;//200;

//  ScreenWidth := Screen.Size.Width;
//  ScreenHeight := Screen.Size.Height;
  AniIndicator1.Visible := false;
  AniIndicator1.Enabled := false;
  AniIndicator1.Align := TAlignLayout.Center;
  StyleObj := AniIndicator1.FindStyleResource('indicator');
  if Assigned(StyleObj) and (StyleObj is TShape) then
  begin
    TShape(StyleObj).Fill.Color := TAlphaColors.Blue;
  end;
  AniIndicator1.Width := ScreenWidth / 2;

{  Image1.Width := ScreenWidth - offsetHalf;
  Image1.Height := ScreenHeight / 2; //ScreenHeight / offsetHalf;
  Image1.Position.X := 0; //offsetTiny;
  Image1.Position.Y := 0; //offsetTiny;
 }
  layImage := TLayout.Create(Self);
  layImage.Parent := Self;
  layImage.Position.X := 0; layImage.Position.Y := 0;
  layImage.Width := ScreenWidth - offsetHalf;
  layImage.Height := ScreenHeight / 2;
  layImage.SetBounds(0, 0, ScreenWidth - offsetHalf, ScreenHeight / 2);
  layImage.ClipChildren := True;


  // set up Image1
  Image1.Parent := layImage;
  Image1.Align := TAlignLayout.None;
  Image1.SetBounds(0, 0, layImage.Width, layImage.Height);

  //gestures
  FLastDistance := 0;

  Image1.Touch.GestureManager := GestureManager1;
  Image1.Touch.InteractiveGestures := [TInteractiveGesture.Zoom, TInteractiveGesture.Pan, TInteractiveGesture.DoubleTap];
  Image1.OnGesture := Image1Gesture;
  FLastTouch := PointF(0, 0); // Initialize the last touch point
  FInitialWidth := Image1.Width;
  FInitialHeight := Image1.Height;

  layOriginal := TLayout.Create(Self); layOriginal.Parent := Self;

  layOriginal.Position.X := offsetSmall; layOriginal.Position.Y := Image1.Height + offsetSmall;
  layOriginal.Height := offsetBig;
  original.Parent := layOriginal; original.Align := TAlignLayout.Left;
  layOriginal.HitTest := True;

  layChrome := TLayout.Create(Self); layChrome.Parent := Self;
  layChrome.Position.X := offsetSmall; layChrome.Position.Y := layOriginal.Position.Y + offsetBig;
  layChrome.Height := offsetBig;
  chrome.Parent := layChrome; chrome.Align := TAlignLayout.Left;
  layChrome.HitTest := True;

  layWarm := TLayout.Create(Self); layWarm.Parent := Self;
  layWarm.Position.X := offsetSmall; layWarm.Position.Y := layChrome.Position.Y + offsetBig;
  layWarm.Height := offsetBig;
  warm.Parent := layWarm; warm.Align := TAlignLayout.Left;
  layWarm.HitTest := True;

  layCool := TLayout.Create(Self); layCool.Parent := Self;
  layCool.Position.X := offsetSmall; layCool.Position.Y := layWarm.Position.Y + offsetBig;
  layCool.Height := offsetBig;
  cool.Parent := layCool; cool.Align := TAlignLayout.Left;
  layCool.HitTest := True;

  layLandscape := TLayout.Create(Self); layLandscape.Parent := Self;
  layLandscape.Position.X := offsetSmall; layLandscape.Position.Y := layCool.Position.Y + offsetBig;
  layCool.Height := offsetBig;
  landscape.Parent := layLandscape; landscape.Align := TAlignLayout.Left;
  layLandscape.HitTest := True;


//  original.Position.X := offsetSmall;
//  chrome.Position.X   := offsetSmall;
//  warm.Position.X     := offsetSmall;
//  cool.Position.X := offsetSmall;
//  landscape.Position.X := offsetSmall;
//  original.Position.Y := Image1.Height + offsetSmall;
//  chrome.Position.Y   := original.Position.Y + offsetBig;
//  warm.Position.Y     := chrome.Position.Y + offsetBig;
//  cool.Position.Y := warm.Position.Y + offsetBig;
//  landscape.Position.Y := cool.Position.Y + offsetBig;

  btnChoose.Width := offsetBig; //ScreenWidth / offsetQuart;
  btnChoose.Height := offsetBig; // Set a fixed height
//  btnChoose.Position.Y := original.Position.Y; //offsetSmall;
  btnChoose.Position.Y := layOriginal.Position.Y; //offsetSmall;
  btnChoose.Position.X := ScreenWidth - btnChoose.Width - offsetSmall;//original.Position.X + original.Width + offsetBig;   //Image1.Width + offsetBig + offsetTiny;
  btnChoose.Text := '📂';

  btnSave.Width := offsetBig; //ScreenWidth / offsetQuart;
  btnSave.Height := offsetBig; // Same height as Button1
  btnSave.Position.Y := btnChoose.Position.Y + btnChooseHald.Height + offsetBig; // Positioned next to Button1
  btnSave.Position.X := btnChoose.Position.X;
  btnSave.Text := '💾';

  btnRotate.Width := offsetBig;// ScreenWidth / offsetQuart;
  btnRotate.Height := offsetBig; // Same height as Button1
  btnRotate.Position.Y := btnSave.Position.Y + btnChooseHald.Height + offsetBig; // Positioned next to Button1
  btnRotate.Position.X := btnSave.Position.X;
  btnRotate.Text := '⟳'; //rotate emoji


  original.GroupName := 'LUTOptions';
  chrome.GroupName := 'LUTOptions';
  warm.GroupName := 'LUTOptions';
  cool.GroupName := 'LUTOptions';
  landscape.GroupName := 'LUTOptions';
  original.IsChecked := True; // default selection

  original.OnChange := RadioButtonChange;
  chrome.OnChange := RadioButtonChange;
  warm.OnChange := RadioButtonChange;
  cool.OnChange := RadioButtonChange;
  landscape.OnChange := RadioButtonChange;
  //{$IF DEFINED(ANDROID)} or DEFINED(IOS)}
  //MobileService := TMobileService.Create(Self);
  //{$ENDIF}

  NotificationCenter1 := TNotificationCenter.Create(Self);
  //NotificationCenter1.OnReceiveLocalNotification := HandleNotification;

  img := TBitmap.Create;
  ResourceStream := TResourceStream.Create(HInstance, 'COLOR_CHECKER', RT_RCDATA);
  try
    img.LoadFromStream(ResourceStream);
    scaleAndShow;
    FInitialWidth := Image1.Width;
    FInitialHeight := Image1.Height;
    //Image1.Bitmap.Assign(color_checker);
  finally
    ResourceStream.Free;
  end;

  LUTChrome := TBitmap.Create;
  ResourceStream := TResourceStream.Create(HInstance, 'LUT_CHROME', RT_RCDATA);
  try
    LUTChrome.LoadFromStream(ResourceStream);
  finally
    ResourceStream.Free;
  end;

  LUTCool := TBitmap.Create;
  ResourceStream := TResourceStream.Create(HInstance, 'LUT_EVERYDAY', RT_RCDATA);
  try
    LUTCool.LoadFromStream(ResourceStream);
  finally
    ResourceStream.Free;
  end;

  LUTLandscape := TBitmap.Create;
  ResourceStream := TResourceStream.Create(HInstance, 'LUT_LANDSCAPE', RT_RCDATA);
  try
    LUTLandscape.LoadFromStream(ResourceStream);
  finally
    ResourceStream.Free;
  end;

  LUTWarm := TBitmap.Create;
  ResourceStream := TResourceStream.Create(HInstance, 'LUT_WARM', RT_RCDATA);
  try
    LUTWarm.LoadFromStream(ResourceStream);
  finally
    ResourceStream.Free;
  end;

{$IFDEF ANDROID}
MobileService := TMobileService.Create(Self);
Intent := TAndroidHelper.Activity.getIntent;
if (Intent <> nil) then
begin
  //ShowMessage('Intent Action: ' + JStringToString(Intent.getAction));
  //ShowMessage('Intent Type: ' + JStringToString(Intent.getType));

  if Intent.getAction.equals(TJIntent.JavaClass.ACTION_SEND) then
  begin
    SharedUri := TJNet_Uri.Wrap((Intent.getParcelableExtra(TJIntent.JavaClass.EXTRA_STREAM) as ILocalObject).GetObjectID);

    if SharedUri <> nil then
    begin
      // request read permission first
      MobileService.RequestReadPermission(
        procedure(PermissionGranted: Boolean)
        begin
          if PermissionGranted then
            LoadSharedImage(SharedUri)
          else
            ShowMessage('Cannot read shared image: read permission denied.');
        end
      );
    end
    else
    begin
      ShowMessage('Error: SharedUri is nil.');
      if not Intent.hasExtra(TJIntent.JavaClass.EXTRA_STREAM) then
      begin
        // fallback: check getData or clipData
        if (Intent.getData <> nil) then
          SharedUri := Intent.getData
        else
          if (Intent.getClipData <> nil) and (Intent.getClipData.getItemCount > 0)
          then begin
            SharedUri := Intent.getClipData.getItemAt(0).getUri;
          end;
        end;
    end;
  end
  else
  begin
    //ShowMessage('Error: Invalid intent action or type.');
  end;
end;
{$ENDIF}

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  MobileService.Free;
  prChrome.Free;
  prWarm.Free;
  prCool.Free;
  prLandscape.Free;
  inherited;
end;

end.
