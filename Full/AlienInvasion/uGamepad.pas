//---------------------------------------------------------------------------

// This software is Copyright (c) 2016 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit uGamepad;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, IPPeerClient, IPPeerServer, FMX.Layouts, System.Actions,
  FMX.ActnList, System.Tether.Manager, System.Tether.AppProfile,
  FMX.Controls.Presentation;

type
  TFrameGamepad = class(TFrame)
    BackgroundRect: TRectangle;
    Label1: TLabel;
    GPTetheringAppProfile: TTetheringAppProfile;
    FireBTN: TRectangle;
    LeftBTN: TRectangle;
    RightBTN: TRectangle;
    Layout1: TLayout;
    CloseBTN: TRectangle;
    ConnectBTN: TRectangle;
    GPTetheringManager: TTetheringManager;
    Layout2: TLayout;
    GPTimer: TTimer;
    ProgressBar: TProgressBar;
    procedure LeftBTNMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure LeftBTNMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure UpBTNMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure UpBTNMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure RightBTNMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure RightBTNMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure FireBTNMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure FireBTNMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure GPTetheringManagerEndManagersDiscovery(const Sender: TObject;
      const RemoteManagers: TTetheringManagerInfoList);
    procedure GPTetheringManagerEndProfilesDiscovery(const Sender: TObject;
      const RemoteProfiles: TTetheringProfileInfoList);
    procedure GPTetheringManagerRemoteManagerShutdown(const Sender: TObject;
      const ManagerIdentifier: string);
    procedure ConnectBTNClick(Sender: TObject);
    procedure GPTimerTimer(Sender: TObject);
  private
    { Private declarations }
    FIsConnected: Boolean;
    IdConnectCount: Integer;
    procedure CheckRemoteProfiles;
    procedure ButtonAction(idaction: Integer);
  public
    { Public declarations }
    TetheringName: String;
    procedure ClearButtons;
    procedure HandleKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure HandleKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
  end;

const
  LEFTACTIONDOWN_C = 11;
  LEFTACTIONUP_C = 12;
  RIGHTACTIONDOWN_C = 21;
  RIGHTACTIONUP_C = 22;
  UPACTIONDOWN_C = 31;
  UPACTIONUP_C = 32;
  FIREACTIONDOWN_C = 51;
  FIREACTIONUP_C = 52;

implementation

uses
  System.Tether.NetworkAdapter;

{$R *.fmx}

procedure TFrameGamepad.CheckRemoteProfiles;
var
  I: Integer;
begin
  if GPTetheringManager.RemoteProfiles.Count > 0 then
  begin
    for I := 0 to GPTetheringManager.RemoteProfiles.Count - 1 do
      if GPTetheringManager.RemoteProfiles[I].ProfileText = TetheringName
      then
        Inc(IdConnectCount);

    if IdConnectCount > 0 then
    Begin
      GPTetheringAppProfile.Connect(GPTetheringManager.RemoteProfiles[0]);
      Label1.Text := 'You are connected with ' +
        GPTetheringManager.RemoteProfiles[0].ProfileText;
    End;
    FIsConnected := true;
  end
  else
  begin
    Label1.Text := 'You are not connected.';
    FIsConnected := false;
  end;
  ProgressBar.Visible := False;
  GPTimer.Enabled := False;
end;

procedure TFrameGamepad.GPTetheringManagerEndManagersDiscovery
  (const Sender: TObject; const RemoteManagers: TTetheringManagerInfoList);
var
  I: Integer;
begin
  if RemoteManagers.Count > 0 then
   begin
    for I := 0 to RemoteManagers.Count - 1 do
      if RemoteManagers[I].ManagerText = 'TetheringManager' then
        GPTetheringManager.PairManager(RemoteManagers[I]);
   end;
end;

procedure TFrameGamepad.GPTetheringManagerEndProfilesDiscovery
  (const Sender: TObject; const RemoteProfiles: TTetheringProfileInfoList);
begin
  CheckRemoteProfiles;
end;

procedure TFrameGamepad.GPTetheringManagerRemoteManagerShutdown
  (const Sender: TObject; const ManagerIdentifier: string);
begin
  CheckRemoteProfiles;
end;

procedure TFrameGamepad.GPTimerTimer(Sender: TObject);
begin
if ProgressBar.Value=100 then ProgressBar.Value := 0;
ProgressBar.Value := ProgressBar.Value + 20;
end;

procedure TFrameGamepad.ConnectBTNClick(Sender: TObject);
var
  I: Integer;
begin
  ProgressBar.Visible := True;
  ProgressBar.Value := 0;
  GPTimer.Enabled := True;

  IdConnectCount := 0;
  for I := GPTetheringManager.PairedManagers.Count - 1 downto 0 do
    GPTetheringManager.UnPairManager(GPTetheringManager.PairedManagers[I]);
  GPTetheringManager.DiscoverManagers;
end;

procedure TFrameGamepad.ButtonAction(idaction: Integer);
begin
  if FIsConnected = false then
    Exit;
  case idaction of
    LEFTACTIONDOWN_C:
      GPTetheringAppProfile.RunRemoteActionAsync(GPTetheringManager.RemoteProfiles
        [0], 'LeftActionDown');
    LEFTACTIONUP_C:
      GPTetheringAppProfile.RunRemoteActionAsync(GPTetheringManager.RemoteProfiles
        [0], 'LeftActionUp');
    RIGHTACTIONDOWN_C:
      GPTetheringAppProfile.RunRemoteActionAsync(GPTetheringManager.RemoteProfiles
        [0], 'RightActionDown');
    RIGHTACTIONUP_C:
      GPTetheringAppProfile.RunRemoteActionAsync(GPTetheringManager.RemoteProfiles
        [0], 'RightActionUp');
    UPACTIONDOWN_C:
      GPTetheringAppProfile.RunRemoteActionAsync(GPTetheringManager.RemoteProfiles
        [0], 'UpActionDown');
    UPACTIONUP_C:
      GPTetheringAppProfile.RunRemoteActionAsync(GPTetheringManager.RemoteProfiles
        [0], 'UpActionUp');
    FIREACTIONDOWN_C:
      GPTetheringAppProfile.RunRemoteActionAsync(GPTetheringManager.RemoteProfiles
        [0], 'FireActionDown');
    FIREACTIONUP_C:
      GPTetheringAppProfile.RunRemoteActionAsync(GPTetheringManager.RemoteProfiles
        [0], 'FireActionUp');
  end;
end;

procedure TFrameGamepad.LeftBTNMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  ButtonAction(LEFTACTIONDOWN_C);
end;

procedure TFrameGamepad.LeftBTNMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  ButtonAction(LEFTACTIONUP_C);
end;

procedure TFrameGamepad.RightBTNMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  ButtonAction(RIGHTACTIONDOWN_C);
end;

procedure TFrameGamepad.RightBTNMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  ButtonAction(RIGHTACTIONUP_C);
end;

procedure TFrameGamepad.UpBTNMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  ButtonAction(UPACTIONDOWN_C);
end;

procedure TFrameGamepad.UpBTNMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  ButtonAction(UPACTIONUP_C);
end;

procedure TFrameGamepad.FireBTNMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  ButtonAction(FIREACTIONDOWN_C);
end;

procedure TFrameGamepad.FireBTNMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  ButtonAction(FIREACTIONUP_C);
end;

procedure TFrameGamepad.HandleKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  case KeyChar of
    'Z', 'z', ' ':
      ButtonAction(FIREACTIONDOWN_C);
    'W', 'w':
      ButtonAction(UPACTIONDOWN_C);
    'A', 'a':
      ButtonAction(LEFTACTIONDOWN_C);
    'D', 'd':
      ButtonAction(RIGHTACTIONDOWN_C);
  end;

  case Key of
    vkUp:
      ButtonAction(UPACTIONDOWN_C);
    vkLeft:
      ButtonAction(LEFTACTIONDOWN_C);
    vkRight:
      ButtonAction(RIGHTACTIONDOWN_C);
  end;
  if Key<>vkHardwareBack then Key := 0;  
end;

procedure TFrameGamepad.HandleKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  case KeyChar of
    'Z', 'z', ' ':
      ButtonAction(FIREACTIONUP_C);
    'W', 'w':
      ButtonAction(UPACTIONUP_C);
    'A', 'a':
      ButtonAction(LEFTACTIONUP_C);
    'D', 'd':
      ButtonAction(RIGHTACTIONUP_C);
  end;

  case Key of
    vkUp:
      ButtonAction(UPACTIONUP_C);
    vkLeft:
      ButtonAction(LEFTACTIONUP_C);
    vkRight:
      ButtonAction(RIGHTACTIONUP_C);
  end;
  if Key<>vkHardwareBack then Key := 0;
end;

procedure TFrameGamepad.ClearButtons;
begin
  ButtonAction(FIREACTIONUP_C);
  ButtonAction(UPACTIONUP_C);
  ButtonAction(LEFTACTIONUP_C);
  ButtonAction(RIGHTACTIONUP_C);
end;

end.
