unit mobile;

interface

uses
  FMX.Graphics, System.SysUtils, System.Classes, System.Types, System.Permissions,
  System.UITypes, FMX.ActnList, FMX.MediaLibrary.Actions, Androidapi.Helpers,
  Androidapi.JNI.Os;

type
  TImageProcessedCallback = reference to procedure(Bitmap: TBitmap);

  TMobileService = class
  private
    WaitingForPermissionToOpenGallery: Boolean;
    procedure ConfigureMaxImageSize;
  public
    ActionList1: TActionList;
    TakePhotoFromLibraryAction1: TTakePhotoFromLibraryAction;
    namePath: string;
    clbk: TImageProcessedCallback;
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure PermissionResult(Sender: TObject; const APermissions: TClassicStringDynArray; const AGrantResults: TClassicPermissionStatusDynArray);
    procedure DisplayRationale(Sender: TObject; const APermissions: TClassicStringDynArray; const APostRationaleProc: TProc);
    procedure TakePhotoFromLibraryAction1DidFinishTaking(Image: TBitmap);
    procedure choose(Callback: TImageProcessedCallback);
    procedure CheckAndRequestPermissions;
    function save(tmp: TBitmap): boolean;
  end;

implementation

uses
  FMX.DialogService, FMX.MediaLibrary, FMX.Platform, System.Messaging, System.Math;

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

procedure TMobileService.CheckAndRequestPermissions;
var
  Permissions: TArray<string>;
begin
  {$IFDEF ANDROID}
  if TOSVersion.Check(13) then
  begin
    Permissions := [
      JStringToString(TJManifest_permission.JavaClass.READ_MEDIA_IMAGES),
      JStringToString(TJManifest_permission.JavaClass.READ_MEDIA_VIDEO),
      JStringToString(TJManifest_permission.JavaClass.READ_MEDIA_AUDIO)
    ];
  end
  else
  begin
    Permissions := [
      JStringToString(TJManifest_permission.JavaClass.READ_EXTERNAL_STORAGE)
    ];
  end;

  PermissionsService.RequestPermissions(Permissions, Self.PermissionResult, Self.DisplayRationale);
  {$ENDIF}
end;

procedure TMobileService.choose(Callback: TImageProcessedCallback);
begin
  clbk := Callback;
  {$IFDEF ANDROID}
  CheckAndRequestPermissions;
  {$ELSE}
  TakePhotoFromLibraryAction1.Execute;
  {$ENDIF}
end;

procedure TMobileService.DisplayRationale(Sender: TObject; const APermissions: TClassicStringDynArray; const APostRationaleProc: TProc);
begin
  FMX.DialogService.TDialogService.ShowMessage('The application needs to access your photo library to choose photos.',
    procedure(const AResult: TModalResult)
    begin
      APostRationaleProc(); // Call the post-rationale procedure to request permission again
    end);
end;

procedure TMobileService.PermissionResult(Sender: TObject; const APermissions: TClassicStringDynArray; const AGrantResults: TClassicPermissionStatusDynArray);
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
    TThread.Queue(nil,
      procedure
      begin
        TakePhotoFromLibraryAction1.Execute;
      end);
  end
//  else
//  begin
//    ShowMessage('Cannot proceed without the required permissions.');
//  end;
end;

procedure TMobileService.TakePhotoFromLibraryAction1DidFinishTaking(Image: TBitmap);
begin
  if Assigned(Image) then
  begin
    namePath := TakePhotoFromLibraryAction1.GetNamePath;
    Self.clbk(Image);
  end;
end;

function TMobileService.save(tmp: TBitmap): boolean;
var
  PhotoLibraryService: IFMXPhotoLibrary;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXPhotoLibrary, IInterface(PhotoLibraryService)) then
  begin
    PhotoLibraryService.AddImageToSavedPhotosAlbum(tmp);
    Result := True;
  end
  else
  begin
    Result := False;
  end;
end;

end.
