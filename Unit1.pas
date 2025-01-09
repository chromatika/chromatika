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


{$R *.fmx}

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
var
  LImageCenter: TPointF;
  Scale, NewWidth, NewHeight: Single;
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
          NewWidth := Image1.Width * Scale;
          NewHeight := Image1.Height * Scale;

          // Prevent zooming out smaller than the initial size
          if (NewWidth >= FInitialWidth) and (NewHeight >= FInitialHeight) then
          begin
            Image1.Width := NewWidth;
            Image1.Height := NewHeight;
            Image1.Position.X := (Image1.Position.X + LImageCenter.X) - (Image1.Width / 2);
            Image1.Position.Y := (Image1.Position.Y + LImageCenter.Y) - (Image1.Height / 2);
          end;

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


procedure TForm1.ExecuteInBackground(TaskProc: TProc; OnCompletion: TProc);
begin
  TTask.Run(procedure
    begin
      try
        if Assigned(TaskProc) then
          TaskProc();  // execute the passed procedure on a background thread
        TThread.Queue(nil, procedure
          begin
            if Assigned(OnCompletion) then
              OnCompletion();  // execute completion on main thread
          end);
      except
        on E: Exception do
          TThread.Queue(nil, procedure
            begin
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
    Image1.Position.Y := 0;//(layImage.Height - Image1.Height) / 2;
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
  imgData: TBitmapData;
begin
  if not Assigned(img) then Exit;  // ensure there is an image loaded
  if original.IsChecked then
  begin
    Image1.Bitmap.Assign(smimg);  // display the original image immediately
    btnSave.Visible := False;
    Exit;  // no further processing needed
  end;
  AniIndicator1.Visible := True;
  AniIndicator1.Enabled := True;
  DisableRadioButtons(True);
  var SelectedLUT := GetCurrentLUT;
  var ProcessedBitmap: TBitmap := GetProcessedBitmap;
  if not Assigned(ProcessedBitmap) then
  begin
    ProcessedBitmap := TBitmap.Create;
    ProcessedBitmap.Assign(smimg);
    //ProcessedBitmap.Canvas.Lock;
    ProcessedBitmap.Map(TMapAccess.ReadWrite, imgData);
    TTask.Run(procedure
    begin
      try
        //haldclut.apply(ProcessedBitmap, SelectedLUT);
        //haldclut.ApplyRaw(imgData, SelectedLUT);
        haldclut.ApplyRawParallel(imgData, SelectedLUT);
        TThread.Queue(nil, procedure
        begin
          CacheProcessedBitmap(ProcessedBitmap);
          UpdateUI(ProcessedBitmap);
          //ProcessedBitmap.Canvas.Unlock;
          ProcessedBitmap.Unmap(imgData);
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
    UpdateUI(ProcessedBitmap);
  end;
end;
{$IFDEF ANDROID or IOS}
procedure TForm1.btnSaveClick(Sender: TObject);
var
  tmp: TBitmap;
  imgData: TBitmapData;
begin
  if not Assigned(img) then Exit;  // ensure there is an image loaded
  // select the correct LUT based on the selected radio button
  if chrome.IsChecked then hald_clut := LUTChrome
  else if warm.IsChecked then hald_clut := LUTWarm
  else if cool.IsChecked then hald_clut := LUTCool
  else if landscape.IsChecked then hald_clut := LUTLandscape;
  tmp := TBitmap.Create;
  tmp.Assign(img);
  // start animation and disable UI elements
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
  {$IFDEF ANDROID}
  MobileService.RequestWritePermission(
    procedure(PermissionGranted: Boolean)
    begin
      if PermissionGranted then
      begin
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
            MobileService.save(tmp,
              procedure(const ASaved: Boolean; const AErrorMessage: string)
              var
                Notification: TNotification;
              begin
                if ASaved then
                begin
                  if NotificationCenter1.Supported then
                  begin
                    Notification := NotificationCenter1.CreateNotification;
                    Notification.Name := 'image processed success';
                    Notification.AlertBody := 'Image saved successfully';
                    Notification.FireDate := Now;
                    NotificationCenter1.ScheduleNotification(Notification);
                    Notification.Free;
                  end;
                end
                else
                begin
                  ShowMessage('Failed to save image: ' + AErrorMessage);
                end;
                // Re-enable UI elements
                FreeAndNil(tmp);
                AniIndicator1.Enabled := False;
                AniIndicator1.Visible := False;
                btnSave.Enabled := True;
                btnChoose.Enabled := True;
                original.Enabled := True;
                chrome.Enabled := True;
                warm.Enabled := True;
                cool.Enabled := True;
                landscape.Enabled := True;
                Image1.Enabled := True;
                btnSave.Enabled := True;
              end);
          end
        );
      end
      else
      begin
        ShowMessage('Cannot save image without write permission.');
        // Re-enable UI elements
        FreeAndNil(tmp);
        AniIndicator1.Enabled := False;
        AniIndicator1.Visible := False;
        btnSave.Enabled := True;
        btnChoose.Enabled := True;
        original.Enabled := True;
        chrome.Enabled := True;
        warm.Enabled := True;
        cool.Enabled := True;
        landscape.Enabled := True;
        Image1.Enabled := True;
        btnSave.Enabled := True;
      end;
    end);
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
  Image1.Touch.InteractiveGestures := [TInteractiveGesture.Zoom, TInteractiveGesture.Pan];
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

  btnChoose.Width := ScreenWidth / offsetQuart;
  btnChoose.Height := offsetBig; // Set a fixed height
//  btnChoose.Position.Y := original.Position.Y; //offsetSmall;
  btnChoose.Position.Y := layOriginal.Position.Y; //offsetSmall;
  btnChoose.Position.X := ScreenWidth - btnChoose.Width - offsetSmall;//original.Position.X + original.Width + offsetBig;   //Image1.Width + offsetBig + offsetTiny;

  btnSave.Width := ScreenWidth / offsetQuart;
  btnSave.Height := offsetBig; // Same height as Button1
  btnSave.Position.Y := btnChoose.Position.Y + btnChooseHald.Height + offsetBig; // Positioned next to Button1
  btnSave.Position.X := btnChoose.Position.X;



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
  //{$IFDEF ANDROID or IOS}
  //MobileService := TMobileService.Create(Self);
  //{$ENDIF}

  NotificationCenter1 := TNotificationCenter.Create(Self);
  //NotificationCenter1.OnReceiveLocalNotification := HandleNotification;

  img := TBitmap.Create;
  ResourceStream := TResourceStream.Create(HInstance, 'COLOR_CHECKER', RT_RCDATA);
  try
    img.LoadFromStream(ResourceStream);
    scaleAndShow;
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
