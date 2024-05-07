unit mobile;

interface
uses
  FMX.Graphics, //for tbitmap
  System.SysUtils,
  System.Classes, //for TComponent
  System.Types, // for tclassicstringdynarray
  System.Permissions,  (* for TClassicPermissionStatusDynArray *)
  System.UITypes // for tmodalresult
{$IFDEF ANDROID or IOS}
 ,FMX.ActnList, (* for TActionList *)
  FMX.MediaLibrary.Actions (* for TTakePhotoFromMediaLibraryAction *)
{$ENDIF}
;

type
  TImageProcessedCallback = reference to procedure(Bitmap: TBitmap);

  TMobileService = class
  private
    {$IFDEF ANDROID or IOS}
    WaitingForPermissionToOpenGallery: Boolean;
    function GetAvailableMemory: Int64;
    procedure ConfigureMaxImageSize;
    {$ENDIF}
  public
    {$IFDEF ANDROID or IOS}
    ActionList1: FMX.ActnList.TActionList;
    //TakePhotoFromLibraryAction1: TTakePhotoFromLibraryAction;
    TakePhotoFromLibraryAction1 : FMX.MediaLibrary.Actions.TTakePhotoFromLibraryAction;
    namePath: string;
    clbk: TImageProcessedCallback;

    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    procedure PermissionResult(Sender: TObject; const APermissions: TClassicStringDynArray; const AGrantResults: TClassicPermissionStatusDynArray);
    procedure DisplayRationale(Sender: TObject; const APermissions: TClassicStringDynArray; const APostRationaleProc: TProc);
    procedure TakePhotoFromLibraryAction1DidFinishTaking(Image: TBitmap);
    //procedure choose(var img0, smimg0: TBitmap; MaxW, MaxH: Single);
    procedure choose(Callback: TImageProcessedCallback);
    procedure CheckAndRequestWritePermission;
    function save(tmp: TBitmap): boolean;
    {$ENDIF}
  end;

//{$IFDEF ANDROID or IOS}
//    TakePhotoFromLibraryAction1: FMX.MediaLibrary.Actions.TTakePhotoFromLibraryAction;  // Dynamic Action for photo library
//{$ENDIF}



implementation
   {$IFDEF ANDROID or IOS}
uses
   FMX.DialogService,        (* for TDialogService *)
   FMX.MediaLibrary,         (* for IFMXPhotoLibrary *)
   FMX.Platform             (* for *)
   {$IFDEF ANDROID}
   ,Androidapi.Helpers, //for JStringToString
   Androidapi.JNI.OS, //for TJManifest_permission
   Androidapi.Jni.App, //not sure why
   // next two for getting android memory size in GetAvailableMemory function
   Androidapi.JNI.GraphicsContentViewText, // for TJContext
   Androidapi.JNIBridge, // for  ILocalObject
   System.Math,          //for Min
   System.Messaging
   {$ENDIF}
   ;
   {$ENDIF}



{$IFDEF ANDROID}
   const
     permBatt = 'android.permission.REQUEST_IGNORE_BATTERY_OPTIMIZATIONS';
     permRead = 'android.permission.READ_EXTERNAL_STORAGE';
     permWrite = 'android.permission.WRITE_EXTERNAL_STORAGE';
     permBlue  = 'android.permission.BLUETOOTH';
     permAccess = 'android.permission.ACCESS_FINE_LOCATION';
     permCoarse = 'android.permission.ACCESS_COARSE_LOCATION';

{$ENDIF}




{$IFDEF ANDROID}
function TMobileService.GetAvailableMemory: Int64;

var
  ActivityManager: JActivityManager;
  MemoryInfo: JActivityManager_MemoryInfo;

begin
  Result := 0;
  {$IFDEF ANDROID}
  ActivityManager := TJActivityManager.Wrap(
    (TAndroidHelper.Context.getSystemService(
      TJContext.JavaClass.ACTIVITY_SERVICE) as ILocalObject).GetObjectID);
  MemoryInfo := TJActivityManager_MemoryInfo.JavaClass.init;
  ActivityManager.getMemoryInfo(MemoryInfo);
  Result := MemoryInfo.availMem;
  {$ENDIF}
  // Add similar implementation for iOS or other platforms if needed
end;


procedure TMobileService.ConfigureMaxImageSize;
var
  AvailableMemory: Int64;
  MaxPixels: Int64;
  SuggestedMaxDimension: Integer;
begin
{$IFDEF CPUARM}
  AvailableMemory := GetAvailableMemory;
  // Assuming using up to 1/4 of available memory for the image
  MaxPixels := AvailableMemory div 4 div 4;  // Div 4 for bytes per pixel
  MaxPixels := MaxPixels div 5; // one more insurance
  SuggestedMaxDimension := Trunc(Sqrt(MaxPixels));  // Simplistic square image assumption

  // Set to a max or to calculated based on memory, also considering some practical maximum dimension
  TakePhotoFromLibraryAction1.MaxHeight := Min(SuggestedMaxDimension, 5000);
  TakePhotoFromLibraryAction1.MaxWidth := Min(SuggestedMaxDimension, 5000);
{$ENDIF}

{$IFDEF CPUARM64}
  TakePhotoFromLibraryAction1.MaxHeight := 5000;
  TakePhotoFromLibraryAction1.MaxWidth := 5000;
{$ENDIF}


end;
{$ENDIF}
{
procedure TMobileService.PermissionResult(Sender: TObject; const APermissions: TClassicStringDynArray; const AGrantResults: TClassicPermissionStatusDynArray);
begin
  if (Length(AGrantResults) > 0) and (AGrantResults[0] = TPermissionStatus.Granted) then
  begin
    TakePhotoFromLibraryAction1.Execute; // Proceed with the action
  end
end;
 }
{
procedure TMobileService.PermissionResult(Sender: TObject; const APermissions: TClassicStringDynArray; const AGrantResults: TClassicPermissionStatusDynArray);
begin
  if (Length(AGrantResults) > 0) and (AGrantResults[0] = TPermissionStatus.Granted) then
  begin
    if WaitingForPermissionToOpenGallery then
    begin
      TakePhotoFromLibraryAction1.Execute; // Proceed with opening the gallery
      WaitingForPermissionToOpenGallery := False; // Reset the flag
    end;
  end
  else
  begin
    // Handle permission denial here, possibly with a user alert
  end;
end;
 }
{$IFDEF ANDROID}
{
procedure TMobileService.PermissionResult(Sender: TObject; const APermissions: TClassicStringDynArray; const AGrantResults: TClassicPermissionStatusDynArray);
begin
  if (Length(AGrantResults) > 0) and (AGrantResults[0] = TPermissionStatus.Granted) then
  begin
    // If waiting for permission to open the gallery, proceed to open it on the main thread
    TThread.Queue(nil, // Use nil to refer to the main thread
      procedure
      begin
        if WaitingForPermissionToOpenGallery then
        begin
          TakePhotoFromLibraryAction1.Execute;
          WaitingForPermissionToOpenGallery := False;
        end;
      end);
  end;
end;
}
procedure TMobileService.PermissionResult(Sender: TObject; const APermissions: TClassicStringDynArray; const AGrantResults: TClassicPermissionStatusDynArray);
begin
  if (Length(AGrantResults) > 0) and (AGrantResults[0] = TPermissionStatus.Granted) then
  begin
    WaitingForPermissionToOpenGallery := True;  // Permission granted, set the flag
  end
  else
  begin
    // Handle permission denial here, possibly with a user alert
  end;
end;
{$ENDIF}

{$IFDEF ANDROID}
procedure TMobileService.DisplayRationale(Sender: TObject; const APermissions: TClassicStringDynArray; const APostRationaleProc: TProc);
begin
  //      if debug then begin log.msg('entered displayrationale()') end;
  FMX.DialogService.TDialogService.ShowMessage('The application needs to access your photo library to choose photos.',
    procedure(const AResult: TModalResult)
    begin
      APostRationaleProc(); // Call the post-rationale procedure to request permission again
    end);
end;
{$ENDIF}
{$IFDEF ANDROID or IOS}

procedure TMobileService.CheckAndRequestWritePermission;
begin
  {$IFDEF ANDROID}
  // Check if permission is already granted
  if not PermissionsService.IsPermissionGranted(permWrite) then
  begin
    // Request permission
    PermissionsService.RequestPermissions([permWrite], PermissionResult, DisplayRationale);
  end;
  {$ENDIF}
  // No else part needed here because iOS handles permissions differently
end;
{
   if TPlatformServices.Current.SupportsPlatformService(IFMXPhotoLibrary, PhotoLibraryService) then
  begin
    // Use the service to add the image to the saved photos album
    PhotoLibraryService.AddImageToSavedPhotosAlbum(tmp);

}
function TMobileService.save(tmp: TBitmap): boolean;
var
  PhotoLibraryService: IFMXPhotoLibrary;
begin
//  {$IFDEF ANDROID}
 // CheckAndRequestWritePermission;
//  {$ENDIF}
    if TPlatformServices.Current.SupportsPlatformService(IFMXPhotoLibrary, IInterface(PhotoLibraryService)) then
//    if TPlatformServices.Current.SupportsPlatformService(IFMXPhotoLibrary, PhotoLibraryService) then
    begin
      // Use the service to add the image to the saved photos album
      PhotoLibraryService.AddImageToSavedPhotosAlbum(tmp);
      exit(true)
    end
    else
    begin
      exit(false)
    end;
end;
{$ENDIF}

{$IFDEF ANDROID or IOS}


//procedure TMobileService.choose(Callback: TImageProcessedCallback);
//var
//  Permissions: TArray<string>;
//  prmRead: string;
//begin
//  {$IFDEF ANDROID}
//  prmRead := JStringToString(TJManifest_permission.JavaClass.READ_EXTERNAL_STORAGE);
//  Permissions := [prmRead];
//  if PermissionsService.IsPermissionGranted(prmRead) then
//  begin
//    clbk := Callback;
//    TakePhotoFromLibraryAction1.Execute; // Permission already granted, proceed directly
//  end
//  else
//  begin
//    WaitingForPermissionToOpenGallery := True; // Set the flag before requesting permissions
//    PermissionsService.RequestPermissions(Permissions, Self.PermissionResult, Self.DisplayRationale);
//  end;
//  {$ELSE}
  // Assuming permissions are managed differently on iOS
//  TakePhotoFromLibraryAction1.Execute;
//  {$ENDIF}
//end;

procedure TMobileService.choose(Callback: TImageProcessedCallback);
var
  Permissions: TArray<string>;
  prmRead: string;
begin
  {$IFDEF ANDROID}
  prmRead := JStringToString(TJManifest_permission.JavaClass.READ_EXTERNAL_STORAGE);
  Permissions := [prmRead];
  if PermissionsService.IsPermissionGranted(prmRead) then
  begin
    clbk := Callback;
    TakePhotoFromLibraryAction1.Execute; // Permission already granted, proceed directly
  end
  else
  begin
    WaitingForPermissionToOpenGallery := True; // Set the flag before requesting permissions
    PermissionsService.RequestPermissions(Permissions, Self.PermissionResult, Self.DisplayRationale);
  end;
  {$ELSE}
  // Assuming permissions are managed differently on iOS
  TakePhotoFromLibraryAction1.Execute;
  {$ENDIF}
end;

{$ENDIF}

{$IFDEF ANDROID}
procedure TMobileService.TakePhotoFromLibraryAction1DidFinishTaking(Image: TBitmap);
begin
  if Assigned(Image) then
  begin
    namePath := TakePhotoFromLibraryAction1.GetNamePath;
    Self.clbk(Image);
  end;
end;

constructor TMobileService.Create(AOwner: TComponent);
begin
  inherited Create;
  ActionList1 := TActionList.Create(AOwner);  // AOwner should probably be the form or application main component
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
{$ENDIF}
end.
