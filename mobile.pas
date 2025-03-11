unit mobile;

interface

uses
  FMX.Graphics,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.Permissions,
  System.UITypes,
  FMX.ActnList,
  FMX.MediaLibrary.Actions,
  Androidapi.Helpers,
  Androidapi.JNI.Os,
  Androidapi.JNI.JavaTypes,  // Needed for JStringToString
  FMX.DialogService;         // Needed for TDialogService.ShowMessage

type
  TImageProcessedCallback = reference to procedure(Bitmap: TBitmap);
  TSaveImageCompletionEvent = reference to procedure(const ASaved: Boolean; const AErrorMessage: string);
  TPermissionRequestResult = reference to procedure(PermissionGranted: Boolean);

  TMobileService = class
  private
    WaitingForPermissionToOpenGallery: Boolean;
    FSaveCompletion: TSaveImageCompletionEvent;
    FASaved: Boolean;
    FAResultMessage: string;
    FPermissionCallback: TPermissionRequestResult;
    FCompletion: TSaveImageCompletionEvent;

    // Private Methods
    procedure SaveImageCompletion(const ASaved: Boolean; const AResultMessage: string);
    procedure ConfigureMaxImageSize;
    procedure DoSaveImageCompletion;
    procedure DoSaveNotSupported;
    procedure DisplayRationale(Sender: TObject; const APermissions: TClassicStringDynArray; const APostRationaleProc: TProc);
    procedure PermissionRequestResult(Sender: TObject; const APermissions: TClassicStringDynArray; const AGrantResults: TClassicPermissionStatusDynArray);
  public
    ActionList1: TActionList;
    TakePhotoFromLibraryAction1: TTakePhotoFromLibraryAction;
    namePath: string;
    clbk: TImageProcessedCallback;

    // Constructor and Destructor
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    // Public Methods
    procedure RequestWritePermission(Callback: TPermissionRequestResult);
    procedure RequestReadPermission(Callback: TPermissionRequestResult);
    procedure choose(Callback: TImageProcessedCallback);
    procedure TakePhotoFromLibraryAction1DidFinishTaking(Image: TBitmap);
    function save(tmp: TBitmap; Completion: TSaveImageCompletionEvent): Boolean;
  end;

implementation

uses
  FMX.MediaLibrary,
  FMX.Platform,
  System.Math;

{ Conditional declarations for Android 13+ permissions }
const
  {$IF CompilerVersion >= 35.0} // Adjust for Delphi version compatibility
  PERMISSION_READ_MEDIA_IMAGES = 'android.permission.READ_MEDIA_IMAGES';
  PERMISSION_READ_MEDIA_VIDEO = 'android.permission.READ_MEDIA_VIDEO';
  PERMISSION_READ_MEDIA_AUDIO = 'android.permission.READ_MEDIA_AUDIO';
  {$ELSE}
  PERMISSION_READ_MEDIA_IMAGES = '';
  PERMISSION_READ_MEDIA_VIDEO = '';
  PERMISSION_READ_MEDIA_AUDIO = '';
  {$ENDIF}

{ TMobileService }

constructor TMobileService.Create(AOwner: TComponent);
begin
  inherited Create;
  ActionList1 := TActionList.Create(AOwner);
  TakePhotoFromLibraryAction1 := TTakePhotoFromLibraryAction.Create(AOwner);
  ConfigureMaxImageSize;
  TakePhotoFromLibraryAction1.ActionList := ActionList1;
  TakePhotoFromLibraryAction1.OnDidFinishTaking := TakePhotoFromLibraryAction1DidFinishTaking;
  WaitingForPermissionToOpenGallery := False;
end;

destructor TMobileService.Destroy;
begin
  TakePhotoFromLibraryAction1.Free;
  ActionList1.Free;
  inherited;
end;

procedure TMobileService.ConfigureMaxImageSize;
begin
  // Configure as needed
  TakePhotoFromLibraryAction1.MaxHeight := 5000;
  TakePhotoFromLibraryAction1.MaxWidth := 5000;
end;

procedure TMobileService.SaveImageCompletion(const ASaved: Boolean; const AResultMessage: string);
begin
  FASaved := ASaved;
  FAResultMessage := AResultMessage;
  TThread.Synchronize(nil, DoSaveImageCompletion);
end;

procedure TMobileService.DoSaveImageCompletion;
begin
  if not FASaved then
  begin
    TDialogService.ShowMessage('SaveImageCompletion Error: ' + FAResultMessage);
  end;
  if Assigned(FSaveCompletion) then
    FSaveCompletion(FASaved, FAResultMessage);
end;

procedure TMobileService.DoSaveNotSupported;
begin
  if Assigned(FCompletion) then
    FCompletion(False, 'PhotoLibraryService not supported.');
end;

procedure TMobileService.PermissionRequestResult(Sender: TObject; const APermissions: TClassicStringDynArray;
  const AGrantResults: TClassicPermissionStatusDynArray);
var
  PermissionGranted: Boolean;
  i: Integer;
begin
  PermissionGranted := True;
  for i := 0 to Length(AGrantResults) - 1 do
  begin
    if AGrantResults[i] <> TPermissionStatus.Granted then
    begin
      PermissionGranted := False;
      Break;
    end;
  end;

  if Assigned(FPermissionCallback) then
    FPermissionCallback(PermissionGranted);

  {$IFDEF ANDROID}
  if WaitingForPermissionToOpenGallery then
  begin
    WaitingForPermissionToOpenGallery := False;
    if PermissionGranted then
      TakePhotoFromLibraryAction1.Execute
    else
      TDialogService.ShowMessage('Cannot proceed without the required permissions.');
  end;
  {$ENDIF}
end;

procedure TMobileService.DisplayRationale(Sender: TObject; const APermissions: TClassicStringDynArray;
  const APostRationaleProc: TProc);
begin
  TDialogService.ShowMessage('The application needs permission to access your photo library.',
    procedure(const AResult: TModalResult)
    begin
      APostRationaleProc;
    end);
end;

procedure TMobileService.RequestWritePermission(Callback: TPermissionRequestResult);
var
  Permissions: TArray<string>;
begin
  {$IFDEF ANDROID}
  FPermissionCallback := Callback;
  if TOSVersion.Check(13) then
  begin
    Permissions := [
      PERMISSION_READ_MEDIA_IMAGES,
      PERMISSION_READ_MEDIA_VIDEO,
      PERMISSION_READ_MEDIA_AUDIO
    ];
    {Permissions := [
      PERMISSION_READ_MEDIA_IMAGES,
      PERMISSION_READ_MEDIA_VIDEO
    ]; }
  end
  else
  begin
    Permissions := [
      JStringToString(TJManifest_permission.JavaClass.WRITE_EXTERNAL_STORAGE)
    ];
  end;
  PermissionsService.RequestPermissions(Permissions, PermissionRequestResult, DisplayRationale);
  {$ELSE}
  Callback(True);
  {$ENDIF}
end;

procedure TMobileService.RequestReadPermission(Callback: TPermissionRequestResult);
var
  Permissions: TArray<string>;
begin
  FPermissionCallback := Callback;

  if TOSVersion.Check(13) then
  begin
    // If targeting Android 13 (API 33+):
    Permissions := [ PERMISSION_READ_MEDIA_IMAGES ];
  end
  else
  begin
    // For older devices
    Permissions := [
      JStringToString(TJManifest_permission.JavaClass.READ_EXTERNAL_STORAGE)
    ];
  end;

  // Now request them with the same mechanism you used for WRITE.
  PermissionsService.RequestPermissions(
    Permissions,
    PermissionRequestResult,   // your same callback
    DisplayRationale          // your same rationale routine
  );
end;

procedure TMobileService.choose(Callback: TImageProcessedCallback);
var
  Permissions: TArray<string>;
begin
  clbk := Callback;
  {$IFDEF ANDROID}
  WaitingForPermissionToOpenGallery := True;
  if TOSVersion.Check(13) then
  begin
    Permissions := [
      PERMISSION_READ_MEDIA_IMAGES,
      PERMISSION_READ_MEDIA_VIDEO,
      PERMISSION_READ_MEDIA_AUDIO
    ];
  end
  else
  begin
    Permissions := [
      JStringToString(TJManifest_permission.JavaClass.READ_EXTERNAL_STORAGE)
    ];
  end;
  PermissionsService.RequestPermissions(Permissions, PermissionRequestResult, DisplayRationale);
  {$ELSE}
  TakePhotoFromLibraryAction1.Execute;
  {$ENDIF}
end;

procedure TMobileService.TakePhotoFromLibraryAction1DidFinishTaking(Image: TBitmap);
begin
  if Assigned(Image) then
  begin
    if Assigned(clbk) then
      clbk(Image);
  end;
end;

function TMobileService.save(tmp: TBitmap; Completion: TSaveImageCompletionEvent): Boolean;
var
  PhotoLibraryService: IFMXPhotoLibrary;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXPhotoLibrary, IInterface(PhotoLibraryService)) then
  begin
    FSaveCompletion := Completion;
    PhotoLibraryService.AddImageToSavedPhotosAlbum(tmp, SaveImageCompletion);
    Result := True;
  end
  else
  begin
    Result := False;
    FCompletion := Completion;
    TThread.Synchronize(nil, DoSaveNotSupported);
  end;
end;

end.

