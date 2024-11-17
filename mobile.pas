unit mobile;

interface

uses
  FMX.Graphics, System.SysUtils, System.Classes, System.Types, System.Permissions,
  System.UITypes, FMX.ActnList, FMX.MediaLibrary.Actions, Androidapi.Helpers,
  Androidapi.JNI.Os, System.Messaging;

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
    procedure SaveImageCompletion(const ASaved: Boolean; const AResultMessage: string);
    procedure ConfigureMaxImageSize;
    procedure DoSaveImageCompletion;
    procedure DoExecuteTakePhoto;
    procedure DoShowPermissionError;
  public
    ActionList1: TActionList;
    TakePhotoFromLibraryAction1: TTakePhotoFromLibraryAction;
    namePath: string;
    clbk: TImageProcessedCallback;
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure PermissionResult(Sender: TObject; const APermissions: TClassicStringDynArray;
      const AGrantResults: TClassicPermissionStatusDynArray);
    procedure RequestWritePermission(Callback: TPermissionRequestResult);
    procedure DisplayRationale(Sender: TObject; const APermissions: TArray<string>;
      const APostRationaleProc: TProc);
    procedure TakePhotoFromLibraryAction1DidFinishTaking(Image: TBitmap);
    procedure choose(Callback: TImageProcessedCallback);
    procedure CheckAndRequestPermissions;
    function save(tmp: TBitmap; Completion: TSaveImageCompletionEvent): boolean;
  end;

implementation

uses
  FMX.DialogService, FMX.MediaLibrary, FMX.Platform, System.Math, FMX.Dialogs, System.Threading;

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

procedure TMobileService.SaveImageCompletion(const ASaved: Boolean; const AResultMessage: string);
begin
  FASaved := ASaved;
  FAResultMessage := AResultMessage;
  TThread.Queue(nil, DoSaveImageCompletion);
end;

procedure TMobileService.DoSaveImageCompletion;
begin
  if not FASaved then
  begin
    ShowMessage('SaveImageCompletion Error: ' + FAResultMessage);
  end;

  if Assigned(FSaveCompletion) then
    FSaveCompletion(FASaved, FAResultMessage);
end;

procedure TMobileService.RequestWritePermission(Callback: TPermissionRequestResult);
var
  Permissions: TArray<string>;
begin
  {$IFDEF ANDROID}
  Permissions := [JStringToString(TJManifest_permission.JavaClass.WRITE_EXTERNAL_STORAGE)];
  PermissionsService.RequestPermissions(Permissions,
    procedure(const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>)
    begin
      if (Length(AGrantResults) > 0) and (AGrantResults[0] = TPermissionStatus.Granted) then
        Callback(True)
      else
        Callback(False);
    end,
    procedure(const APermissions: TArray<string>; const APostRationaleProc: TProc)
    begin
      FMX.DialogService.TDialogService.ShowMessage('The app needs permission to save images.',
        procedure(const AResult: TModalResult)
        begin
          APostRationaleProc;
        end);
    end);
  {$ELSE}
  Callback(True);
  {$ENDIF}
end;

procedure TMobileService.ConfigureMaxImageSize;
begin
  // Configure as needed
  TakePhotoFromLibraryAction1.MaxHeight := 5000;
  TakePhotoFromLibraryAction1.MaxWidth := 5000;
end;

procedure TMobileService.CheckAndRequestPermissions;
var
  Permissions: TArray<string>;
begin
  {$IFDEF ANDROID}
  if TOSVersion.Check(13) then
  begin
    // Android 13 and above
    Permissions := [
      JStringToString(TJManifest_permission.JavaClass.READ_MEDIA_IMAGES),
      JStringToString(TJManifest_permission.JavaClass.READ_MEDIA_VIDEO),
      JStringToString(TJManifest_permission.JavaClass.READ_MEDIA_AUDIO)
    ];
  end
  else if TOSVersion.Check(10) then
  begin
    // Android 10 to 12
    Permissions := [
      JStringToString(TJManifest_permission.JavaClass.READ_EXTERNAL_STORAGE)
    ];
  end
  else
  begin
    // Below Android 10 (including Android 9)
    Permissions := [
      JStringToString(TJManifest_permission.JavaClass.READ_EXTERNAL_STORAGE),
      JStringToString(TJManifest_permission.JavaClass.WRITE_EXTERNAL_STORAGE)
    ];
  end;

  PermissionsService.RequestPermissions(Permissions, Self.PermissionResult, Self.DisplayRationale);
  {$ENDIF}
end;

procedure TMobileService.choose(Callback: TImageProcessedCallback);
begin
  clbk := Callback;
  {$IFDEF ANDROID}
  WaitingForPermissionToOpenGallery := True;
  CheckAndRequestPermissions;
  {$ELSE}
  TakePhotoFromLibraryAction1.Execute;
  {$ENDIF}
end;

procedure TMobileService.DisplayRationale(Sender: TObject; const APermissions: TArray<string>;
  const APostRationaleProc: TProc);
begin
  FMX.DialogService.TDialogService.ShowMessage('The application needs to access your photo library to choose photos.',
    procedure(const AResult: TModalResult)
    begin
      APostRationaleProc(); // Call the post-rationale procedure to request permission again
    end);
end;

procedure TMobileService.PermissionResult(Sender: TObject; const APermissions: TClassicStringDynArray;
  const AGrantResults: TClassicPermissionStatusDynArray);
var
  AllPermissionsGranted: Boolean;
  i: Integer;
begin
  AllPermissionsGranted := True;
  for i := 0 to Length(AGrantResults) - 1 do
  begin
    if AGrantResults[i] <> TPermissionStatus.Granted then
    begin
      AllPermissionsGranted := False;
      Break;
    end;
  end;

  if AllPermissionsGranted then
  begin
    TThread.Queue(nil, DoExecuteTakePhoto);
  end
  else
  begin
    TThread.Queue(nil, DoShowPermissionError);
  end;
end;

procedure TMobileService.DoExecuteTakePhoto;
begin
  if WaitingForPermissionToOpenGallery then
  begin
    TakePhotoFromLibraryAction1.Execute;
    WaitingForPermissionToOpenGallery := False;
  end;
end;

procedure TMobileService.DoShowPermissionError;
begin
  ShowMessage('Cannot proceed without the required permissions.');
end;

procedure TMobileService.TakePhotoFromLibraryAction1DidFinishTaking(Image: TBitmap);
begin
  if Assigned(Image) then
  begin
    namePath := TakePhotoFromLibraryAction1.GetNamePath;
    Self.clbk(Image);
  end;
end;

function TMobileService.save(tmp: TBitmap; Completion: TSaveImageCompletionEvent): boolean;
var
  PhotoLibraryService: IFMXPhotoLibrary;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXPhotoLibrary, IInterface(PhotoLibraryService)) then
  begin
    FSaveCompletion := Completion;  // Store the completion callback
    PhotoLibraryService.AddImageToSavedPhotosAlbum(tmp, SaveImageCompletion);  // Pass the method
    Result := True;
  end
  else
  begin
    Result := False;
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(Completion) then
          Completion(False, 'PhotoLibraryService not supported.');
      end);
  end;
end;

end.

