unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Controls.Presentation,
  System.Threading, // for TProc
   mobile;

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

    procedure ExecuteInBackground(TaskProc: TProc);
    procedure scaleAndShow;
    procedure setimage(bmp: TBitmap);
    procedure RadioButtonChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnChooseClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
  private
    { Private declarations }
    LUTChrome: TBitmap;
    LUTCool: TBitmap;
    LUTLandscape: TBitmap;
    LUTWarm: TBitmap;
    MobileService: TMobileService;

    hald_clut, img, smimg, tempBitmap, prChrome, prWarm, prCool, prLandscape : TBitmap;
    ScreenWidth, ScreenHeight: Single;
    MaxPreviewWidth: Single;
    MaxPreviewHeight: Single;
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
   haldclut, helpers, log;


{$R *.fmx}
procedure TForm1.ExecuteInBackground(TaskProc: TProc);
begin
  TTask.Run(procedure
    begin
      try
        if Assigned(TaskProc) then
          TaskProc();  // Execute the passed procedure on a background thread

        TThread.Queue(nil, procedure
          begin
            AniIndicator1.Enabled := False;
            AniIndicator1.Visible := False;
            btnSave.Enabled := True;  // Re-enable the save button
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
  ScaleFactor: Single;
begin
    smimg := TBitmap.Create;
         //showmessage('max width ' + IntToStr(MobileService.TakePhotoFromLibraryAction1.MaxWidth));
         //showmessage('ao bmp is ' + IntToStr(bmp.Width) + 'x' + IntToStr(bmp.Height));
     //    showmessage('ao name path: ' + MobileService.namePath);
    //PreviewBitmap := TBitmap.Create;
    try
      // Set a fixed preview size or scale according to your needs
      ScaleFactor := helpers.calculateScaleFactor(img, MaxPreviewWidth, MaxPreviewHeight);
      //PreviewBitmap.SetSize(Round(img.Width * ScaleFactor), Round(img.Height * ScaleFactor));
      smimg.SetSize(Round(img.Width * ScaleFactor), Round(img.Height * ScaleFactor));

      // Resize and assign to preview
      //PreviewBitmap.Canvas.BeginScene;
      smimg.Canvas.BeginScene;
      try
        //PreviewBitmap.Canvas.DrawBitmap(img, RectF(0, 0, img.Width, img.Height),
        //RectF(0, 0, PreviewBitmap.Width, PreviewBitmap.Height), 1);
        smimg.Canvas.DrawBitmap(img, RectF(0, 0, img.Width, img.Height),
        RectF(0, 0, smimg.Width, smimg.Height), 1);
      finally
        //PreviewBitmap.Canvas.EndScene;
        smimg.Canvas.EndScene;
      end;
   finally
      // Display this resized image on the form's TImage control
      //Image1.Bitmap.Assign(PreviewBitmap);
      Image1.Bitmap.Assign(smimg);
       //  showmessage('img is ' + IntToStr(img.Width) + 'x' + IntToStr(img.Height));
       //  showmessage('smimg is ' + IntToStr(smimg.Width) + 'x' + IntToStr(smimg.Height));
      original.IsChecked := true;
   end;
end;

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
    scaleAndShow;
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

procedure TForm1.RadioButtonChange(Sender: TObject);
begin
  if not Assigned(img) then Exit;  // Ensure there is an image loaded

  if original.IsChecked then
  begin
    // Directly display the original image
    Image1.Bitmap.Assign(smimg);
    btnSave.Visible := False;  // Hide the "Save" button when showing the original image
  end
  else
  begin
    // Check which LUT to apply based on the radio button checked
    if chrome.IsChecked then
    begin
      if not Assigned(prChrome) then
      begin
        prChrome := TBitmap.Create;
        prChrome.Assign(smimg);

        haldclut.apply(prChrome, LUTChrome);
      end;
      Image1.Bitmap.Assign(prChrome);
    end
    else if warm.IsChecked then
    begin
      if not Assigned(prWarm) then
      begin
        prWarm := TBitmap.Create;
        prWarm.Assign(smimg);
        haldclut.apply(prWarm, LUTWarm);
      end;
      Image1.Bitmap.Assign(prWarm);
    end
    else if cool.IsChecked then
    begin
      if not Assigned(prCool) then
      begin
        prCool := TBitmap.Create;
        prCool.Assign(smimg);
        haldclut.apply(prCool, LUTCool);
      end;
      Image1.Bitmap.Assign(prCool);
    end
    else if landscape.IsChecked then
    begin
      if not Assigned(prLandscape) then
      begin
        prLandscape := TBitmap.Create;
        prLandscape.Assign(smimg);
        haldclut.apply(prLandscape, LUTLandscape);
      end;
      Image1.Bitmap.Assign(prLandscape);
    end;

    btnSave.Visible := True;  // Show the "Save" button when a LUT is applied
  end;
end;

{$IFDEF ANDROID or IOS}
procedure TForm1.btnSaveClick(Sender: TObject);
var
  tmp: TBitmap;
begin
  if not Assigned(img) then Exit;  // Ensure there is an image loaded

  // Select the correct LUT based on the selected radio button
  if chrome.IsChecked then hald_clut := LUTChrome
  else if warm.IsChecked then hald_clut := LUTWarm
  else if cool.IsChecked then hald_clut := LUTCool
  else if landscape.IsChecked then hald_clut := LUTLandscape;

  tmp := TBitmap.Create;
  tmp.Assign(img);

  // Start animation and disable UI elements
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

  // Execute the image processing in a background thread
  ExecuteInBackground(procedure
    begin
      haldclut.apply(tmp, hald_clut);  // Process the image

      // Save or further process the image as needed
      TThread.Queue(nil, procedure
        begin
          if MobileService.save(tmp) then
            ShowMessage('Image saved successfully.')
          else
            ShowMessage('Failed to save image.');
          tmp.Free;
            btnChoose.Enabled := True;
            original.Enabled := True;
            chrome.Enabled := True;
            warm.Enabled := True;
            cool.Enabled := True;
            landscape.Enabled := True;
            Image1.Enabled := True;
        end);
    end);
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
procedure TForm1.FormCreate(Sender: TObject);
var
  ResourceStream: TResourceStream;
  StyleObj: TFmxObject;
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

  Image1.Width := ScreenWidth - offsetHalf;
  Image1.Height := ScreenHeight / 2; //ScreenHeight / offsetHalf;
  Image1.Position.X := 0; //offsetTiny;
  Image1.Position.Y := 0; //offsetTiny;

  original.Position.X := offsetSmall;
  chrome.Position.X   := offsetSmall;
  warm.Position.X     := offsetSmall;
  cool.Position.X := offsetSmall;
  landscape.Position.X := offsetSmall;
  original.Position.Y := Image1.Height + offsetSmall;
  chrome.Position.Y   := original.Position.Y + offsetBig;
  warm.Position.Y     := chrome.Position.Y + offsetBig;
  cool.Position.Y := warm.Position.Y + offsetBig;
  landscape.Position.Y := cool.Position.Y + offsetBig;

  btnChoose.Width := ScreenWidth / offsetQuart;
  btnChoose.Height := offsetBig; // Set a fixed height
  btnChoose.Position.Y := original.Position.Y; //offsetSmall;
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
  original.IsChecked := True; // Default selection

  original.OnChange := RadioButtonChange;
  chrome.OnChange := RadioButtonChange;
  warm.OnChange := RadioButtonChange;
  cool.OnChange := RadioButtonChange;
  landscape.OnChange := RadioButtonChange;
  {$IFDEF ANDROID or IOS}
  MobileService := TMobileService.Create(Self);
  {$ENDIF}

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
