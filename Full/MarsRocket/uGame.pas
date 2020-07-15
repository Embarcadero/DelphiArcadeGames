//---------------------------------------------------------------------------

// This software is Copyright (c) 2016-2020 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit uGame;

{$DEFINE DEBUG}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.IniFiles,
  System.Messaging, System.IOUtils, FMX.Types, FMX.Controls, FMX.Forms,
  FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Layouts, System.Math, FMX.StdCtrls, FMX.Ani, FMX.Media, FMX.Platform,
  FMX.Filter.Effects, FMX.Effects, AudioManager, uMainMenu, uInstructions,
  uLevelComplete, uGameOver, uHighScores, System.Sensors,
  System.Sensors.Components, IPPeerClient, IPPeerServer, System.Actions,
  FMX.ActnList, System.Tether.Manager, System.Tether.AppProfile, uGamepad, uSettings,
  AnonThread, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
  FMX.Controls.Presentation, FMX.DialogService.Async;

type
  TPlayerData = class(TFMXObject)
  public
    SpeedX: Single;
    SpeedY: Single;
    Thrust: Single;
    Lives: Integer;
    Score: Integer;
    Level: Integer;
    Stars: Integer;

    Fuel: Integer;
    VerticalVelocity: Single;
    HorizontalVelocity: Single;
    Landed: Boolean;
  end;

  TGameForm = class(TForm)
    GameLoop: TTimer;
    ScreenLayout: TLayout;
    AssetLayout: TLayout;
    StyleBook: TStyleBook;
    Explosion: TRectangle;
    AnimateExplosion: TBitmapListAnimation;
    CollectItem: TRectangle;
    AnimateCollectItem: TBitmapListAnimation;
    LeftBTN: TRectangle;
    RightBTN: TRectangle;
    Lives1: TRectangle;
    Lives2: TRectangle;
    Lives3: TRectangle;
    ScoreLBL: TLabel;
    FireBTN: TRectangle;
    FPSLBL: TLabel;
    SoundBTN: TRectangle;
    SoundOffLine: TLine;
    MusicBTN: TRectangle;
    MusicOffLine: TLine;
    HUDLayout: TLayout;
    MusicPlayer: TMediaPlayer;
    FuelLBL: TLabel;
    AngleLBL: TLabel;
    VVLBL: TLabel;
    CliffRect: TRectangle;
    Ship: TRectangle;
    Thruster: TRectangle;
    GyroBTN: TRectangle;
    GyroOffLine: TLine;
    InterfaceBackground: TRectangle;
    LandingPad: TRectangle;
    AccLeft: TCircle;
    AccRight: TCircle;
    AccThrustCircle: TCircle;
    AboveCircle: TCircle;
    MainMenuFrame: TFrameMainMenu;
    SettingsFrame: TFrameSettings;
    GameOverFrame: TFrameGameOver;
    GamepadFrame: TFrameGamepad;
    InstructionsFrame: TFrameInstructions;
    HighScoresFrame: TFrameHighScores;
    LevelCompleteFrame: TFrameLevelComplete;
    TetheringManager: TTetheringManager;
    TetheringAppProfile: TTetheringAppProfile;
    MotionSensor: TMotionSensor;
    ActionList: TActionList;
    LeftActionDown: TAction;
    LeftActionUp: TAction;
    RightActionDown: TAction;
    RightActionUp: TAction;
    UpActionDown: TAction;
    UpActionUp: TAction;
    DownActionDown: TAction;
    DownActionUp: TAction;
    FireActionDown: TAction;
    FireActionUp: TAction;
    WarpActionClick: TAction;
    BombActionClick: TAction;
    IdTCPClient1: TIdTCPClient;
    DelayedSettings: TTimer;
    procedure GameLoopTimer(Sender: TObject);
    procedure RightBTNMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure RightBTNMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure LeftBTNMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure LeftBTNMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure FireBTNMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure FireBTNMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure FormDestroy(Sender: TObject);
    procedure MainMenuFramePlayBTNClick(Sender: TObject);
    procedure InstructionsFrameContinueBTNClick(Sender: TObject);
    procedure GameOverFramePlayAgainBTNClick(Sender: TObject);
    procedure GameOverFrameMainMenuBTNClick(Sender: TObject);
    procedure GameOverFrameMoreGamesBTNClick(Sender: TObject);
    procedure MainMenuFrameMoreGamesBTNClick(Sender: TObject);
    procedure HighScoresFrameContinueBTNClick(Sender: TObject);
    procedure MainMenuFrameHighScoresBTNClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure LevelCompleteFrameContinueBTNClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure AnimateExplosionFinish(Sender: TObject);
    procedure MusicBTNClick(Sender: TObject);
    procedure SoundBTNClick(Sender: TObject);
    procedure GyroBTNClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure GamepadFrameCloseBTNClick(Sender: TObject);
    procedure SettingsFrameHostSwitchSwitch(Sender: TObject);
    procedure SettingsFrameMainMenuBTNClick(Sender: TObject);
    procedure MainMenuFrameSettingsBTNClick(Sender: TObject);
    procedure MainMenuFrameGamepadBTNClick(Sender: TObject);
    procedure LeftActionDownExecute(Sender: TObject);
    procedure LeftActionUpExecute(Sender: TObject);
    procedure RightActionDownExecute(Sender: TObject);
    procedure RightActionUpExecute(Sender: TObject);
    procedure UpActionDownExecute(Sender: TObject);
    procedure UpActionUpExecute(Sender: TObject);
    procedure FireActionDownExecute(Sender: TObject);
    procedure FireActionUpExecute(Sender: TObject);
    procedure SettingsFrameFullScreenSwitchSwitch(Sender: TObject);
    procedure HUDLayoutMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure HighScoresFrameOkayBTNClick(Sender: TObject);
    procedure HighScoresFrameCancelBTNClick(Sender: TObject);
    procedure DelayedSettingsTimer(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
    LeftButtonDown: Boolean;
    RightButtonDown: Boolean;
    FireButtonDown: Boolean;
    AccLeftButtonDown: Boolean;
    AccRightButtonDown: Boolean;
    AccFireButtonDown: Boolean;
    GroundList: TStringList;
    ExplosionList: TStringList;
    CollectList: TStringList;
    PlayerData: TPlayerData;
    GroundPool: TStringList;
    ExplosionPool: TStringList;
    CollectPool: TStringList;
    AudioManager: TAudioManager;
    CurrentStage: Integer;
    LastStage: Integer;
    DataFilePath: String;
    SettingsFilePath: String;
    MusicEnabled: Boolean;
    SoundEnabled: Boolean;
    GyroEnabled: Boolean;
    HostEnabled: Boolean;
    FullScreenEnabled: Boolean;
    LivesSet: Boolean;
    CatchLastPos: Single;
    RScreenOut: Boolean;
    LScreenOut: Boolean;
    CleanedUp: Boolean;

    NetworkConnected: Boolean;
    NetworkChecking: Boolean;

    ScreenOrientation: TScreenOrientation;
    OrientationChangedId: Integer;
    procedure OrientationChanged(const Sender: TObject; const Msg: TMessage);
    function GetScreenOrientation: TScreenOrientation;
    procedure ReOrientgame;

    procedure StartGame;
    procedure PlayGame;
    procedure StartGameLoop;
    procedure StopGameLoop;
    procedure ShowMainMenu;
    procedure ShowHighScores;
    procedure ShowMoreGames;
    procedure ShowGamePad;
    procedure InitPlayer;
    procedure LevelComplete;
    procedure LevelFail;
    procedure GameOver;
    procedure InitAndPlayGame;
    procedure InitGame;
    procedure ShowHUD(Stage: Integer);
    procedure HideHUD(Stage: Integer);
    procedure ContinueGame;
    procedure CloseHighScores;
    procedure CleanupGame(Continue: Boolean);
    procedure ClearButtons;
    procedure LeftDownEvent(Sender: TObject);
    procedure LeftUpEvent(Sender: TObject);
    procedure RightDownEvent(Sender: TObject);
    procedure RightUpEvent(Sender: TObject);
    procedure UpDownEvent(Sender: TObject);
    procedure UpUpEvent(Sender: TObject);
    procedure CenterPlayer;
    procedure AddScore(I: Integer);
    procedure DisplayLives(Lives: Integer);
    procedure DisplayScore;
    procedure PlayerHit;
    function SpawnCollectItem(X, Y: Single; Size: Integer): TRectangle;
    function SpawnExplosion(X, Y: Single; Angle: Single): TRectangle;
    procedure CreateExplosion(X, Y: Single);
    function SpawnGround(X, Y: Single; GroundType: Integer;
      LandingPadWidth: Single): TRectangle;
    procedure ProcessAccelerometer;

    procedure AssignInterfaceBackground(R: TRectangle);
    procedure ResetGround;
    procedure BitmapToRectangle(B: TBitmap; R: TRectangle);
    function IntersectCircle(R1, R2: TRectangle): Boolean;
    function GetTargetAngle(TargetX, TargetY, OriginX, OriginY: Single): Single;
    function GetPoolObj(Pool: TStringList): TRectangle;
    procedure SetPoolObj(Pool: TStringList; Name: String; Obj: TRectangle);
    procedure PauseBitmapListAnimations(Value: Boolean);

    procedure PlaySound(Index: Integer);
    function RegisterSound(Filename: String): Integer;
    procedure PlayMusic;
    procedure StopMusic;
    function GetMaxVolume: Single;

    procedure GyroToggle;
    procedure SoundToggle;
    procedure MusicToggle;    

    procedure SetStage(Stage: Integer);
    procedure SaveSettings;
    procedure LoadSettings;
    procedure ShowSettings;
    procedure SaveAndExitSettings;
    function CheckNetworkState: Boolean;
    procedure PollNetworkState;
    procedure ToggleAppTethering(State: Boolean);
  public
    { Public declarations }
    function HandleAppEvent(AAppEvent: TApplicationEvent;
      AContext: TObject): Boolean;
    procedure ShowMsgBox(S: String);
    procedure ExitDialog(Sender: TObject);
  end;

const
  // Player
  PLAYER_LIVES = 3;
  PLAYER_FUEL = 500; // lower to increase fuel
  PLAYER_THRUST = 0.15;
  PLAYER_LANDING_VELOCITY = 4; // try 2 for a harder landing
  GAME_GRAVITY = 0.05;
  // Stages
  MAIN_MENU = 0;
  INSTRUCTIONS = 1;
  GAMEPLAY = 2;
  LEVEL_COMPLETE = 3;
  GAME_OVER = 4;
  HIGH_SCORES = 5;
  SETTINGS = 6;
  GAMEPAD = 7;
  // Sounds
  FIRE_SFX = 0;
  EXPLOSION_SFX = 1;
  FAIL_SFX = 2;
  WIN_SFX = 3;
  ALIEN_SFX = 4;
  COLLECT_SFX = 5;
  // Music
  MUSIC_FILENAME = 'music.mp3';
  // Movement Speeds
  COLLECTITEM_SPEED = 0;
  // Network Connections
  NETWORK_ADDRESS = 'www.embarcadero.com';
  NO_NETWORK_MESSAGE = 'A network connection is needed for this feature and it was not detected.';

var
  GameForm: TGameForm;

implementation

{$R *.fmx}

procedure TGameForm.ShowMsgBox(S: String);
begin
  TDialogServiceAsync.MessageDialog(S, System.UITypes.TMsgDlgType.mtInformation,[System.UITypes.TMsgDlgBtn.mbOk], System.UITypes.TMsgDlgBtn.mbOk, 0, procedure(const AResult: TModalResult)  begin end);
end;

function TGameForm.HandleAppEvent(AAppEvent: TApplicationEvent;
  AContext: TObject): Boolean;
begin
  case AAppEvent of
    // TApplicationEvent.FinishedLaunching: Log('Finished Launching');
    TApplicationEvent.BecameActive:
      begin
        if (CurrentStage = GAMEPLAY) AND (GameLoop.Enabled = False) then
          begin
           StartGameLoop;
           PauseBitmapListAnimations(False);
          end;
      end;
    // TApplicationEvent.WillBecomeInactive: Log('Will Become Inactive');
    TApplicationEvent.EnteredBackground:
      begin
        if (CurrentStage = GAMEPLAY) AND (GameLoop.Enabled = True) then
          begin
           PauseBitmapListAnimations(True);
           StopGameLoop;
          end;
      end;
    // TApplicationEvent.WillBecomeForeground: Log('Will Become Foreground');
    // TApplicationEvent.WillTerminate: Log('Will Terminate');
    // TApplicationEvent.LowMemory: Log('Low Memory');
    // TApplicationEvent.TimeChange: Log('Time Change');
    // TApplicationEvent.OpenURL: Log('Open URL');
  end;
  Result := True;
end;

procedure TGameForm.PauseBitmapListAnimations(Value: Boolean);
var
 I: Integer;
 BLAObj: TRectangle;
begin
    for I := ExplosionList.Count-1 downto 0 do
    begin
      BLAObj := TRectangle(ExplosionList.Objects[I]);
      TBitmapListAnimation(BLAObj.TagObject).Pause := Value;
    end;
end;

procedure TGameForm.PlaySound(Index: Integer);
begin
  if SoundEnabled then
    AudioManager.PlaySound(Index);
end;

function TGameForm.RegisterSound(Filename: String): Integer;
begin
  if FileExists(Filename) then
    Result := AudioManager.AddSound(Filename)
  else
    Result := -1;
end;

function TGameForm.GetMaxVolume: Single;
begin
{$IF DEFINED(IOS) OR DEFINED(MACOS)}
 Result := 0.75;
{$ELSE}
 Result := 1;
{$ENDIF}
end;

procedure TGameForm.PlayMusic;
begin

  if MusicPlayer.Media <> nil then
  begin
    // MusicPlayer.CurrentTime := 0;
    MusicPlayer.Play;

    if MusicEnabled then
    begin
      if MusicPlayer.Volume <> GetMaxVolume then
        MusicPlayer.Volume := GetMaxVolume;
    end
    else
    begin
      if MusicPlayer.Volume <> 0 then
        MusicPlayer.Volume := 0;
    end;
  end;
end;

procedure TGameForm.StopMusic;
begin
  if MusicPlayer.Filename <> '' then
  begin
    if MusicPlayer.Volume <> 0 then
      MusicPlayer.Volume := 0;
  end;
end;

procedure TGameform.LeftDownEvent(Sender: TObject);
begin
  if RScreenOut = True then
    RScreenOut:=false;

  RightButtonDown := False;
  LeftButtonDown := True;
end;

procedure TGameform.LeftUpEvent(Sender: TObject);
begin
  LeftButtonDown := False;
end;

procedure TGameform.RightDownEvent(Sender: TObject);
begin
  if LScreenOut = True then
    LScreenOut:=false;

  LeftButtonDown := False;
  RightButtonDown := True;
end;

procedure TGameform.RightUpEvent(Sender: TObject);
begin
  RightButtonDown := False;
end;

procedure TGameform.UpDownEvent(Sender: TObject);
begin
  if RScreenOut=True then
    RScreenOut := False;

  if LScreenOut=True then
    LScreenOut := False;

  FireButtonDown := True;
end;

procedure TGameform.UpUpEvent(Sender: TObject);
begin
  FireButtonDown := False;
end;

procedure TGameForm.UpActionDownExecute(Sender: TObject);
begin
  // called from RunRemoteActionAsync. Requires TThread.Synchronize for UI changes.
  UpDownEvent(Sender);
end;

procedure TGameForm.UpActionUpExecute(Sender: TObject);
begin
  // called from RunRemoteActionAsync. Requires TThread.Synchronize for UI changes.
  UpUpEvent(Sender);
end;

procedure TGameForm.ShowHighScores;
begin
  HighScoresFrame.InitFrame;
  SetStage(HIGH_SCORES);
end;

procedure TGameForm.AddScore(I: Integer);
begin
  PlayerData.Score := PlayerData.Score + I;
  ScoreLBL.Text := IntToStr(PlayerData.Score);
end;

function TGameForm.GetPoolObj(Pool: TStringList): TRectangle;
begin
  if Pool.Count > 0 then
  begin
    Result := TRectangle(Pool.Objects[Pool.Count - 1]);
    // Pool.Objects[Pool.Count-1] := nil;
    Pool.Delete(Pool.Count - 1);
  end
  else
  begin
    Result := TRectangle.Create(nil);
  end;
end;

procedure TGameForm.SetPoolObj(Pool: TStringList; Name: String;
  Obj: TRectangle);
begin
  Pool.AddObject(Name, Obj);
  Obj.Parent := nil;
end;

function TGameForm.SpawnGround(X, Y: Single; GroundType: Integer;
  LandingPadWidth: Single): TRectangle;
var
  R: TRectangle;
begin
  R := GetPoolObj(GroundPool);
  R.Parent := ScreenLayout;
  if GroundType = 1 then
  begin
    R.Width := LandingPadWidth + RandomRange(20, 40);
    R.Height := RandomRange(100, 150);
    R.XRadius := 0;
    R.YRadius := 0;
    BitmapToRectangle(LandingPad.Fill.Bitmap.Bitmap, R);
    R.TagString := 'P';
  end
  else
  begin
    // R.Width := RandomRange(15, 75);
    R.Height := RandomRange(50, 125);
    R.Width := R.Height;
    R.RotationAngle := 45;
    R.XRadius := 20;
    R.YRadius := 20;
    R.Fill.Kind := TBrushKind.Solid;
    R.Fill.Color := $FF6F5336;
    R.TagString := 'G';
  end;
  R.Position.X := X;
  R.Position.Y := Y - R.Height;
  R.Stroke.Kind := TBrushKind.None;
  R.Tag := 5;
  R.SendToBack;
  Result := R;
end;

function TGameForm.SpawnCollectItem(X, Y: Single; Size: Integer): TRectangle;
var
  R: TRectangle;
  CollectGraphic: TRectangle;
begin
  R := GetPoolObj(CollectPool);
  R.Stroke.Kind := TBrushKind.None;
  R.Fill.Kind := TBrushKind.None;
  R.RotationAngle := Random(360);
  R.Width := 50;
  R.Height := 50;
  R.Position.X := X;
  R.Position.Y := Y;

  if not Assigned(R.TagObject) then
  begin
    CollectGraphic := TRectangle.Create(R);
    CollectGraphic.Parent := R;
  end
  else
  begin
    CollectGraphic := TRectangle(R.TagObject);
  end;

  CollectGraphic.Width := R.Width;
  CollectGraphic.Height := R.Height;
  CollectGraphic.Position.X := (R.Width / 2) - (CollectGraphic.Width / 2);
  CollectGraphic.Position.Y := (R.Height / 2) - (CollectGraphic.Height / 2);
  CollectGraphic.RotationAngle := (360 - R.RotationAngle);

  CollectGraphic.Stroke.Kind := TBrushKind.None;
  CollectGraphic.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
  CollectGraphic.Fill.Kind := TBrushKind.Bitmap;
  CollectGraphic.Fill.Bitmap.Bitmap.Assign(CollectItem.Fill.Bitmap.Bitmap);

  if not Assigned(R.TagObject) then
  begin
    R.TagObject := CollectGraphic;
  end;

  R.Tag := COLLECTITEM_SPEED;
  R.TagFloat := 0;
  R.Parent := Self;
  R.SendToBack;
  Result := R;
end;

function TGameForm.SpawnExplosion(X, Y: Single; Angle: Single): TRectangle;
var
  R: TRectangle;
  AE: TBitmapListAnimation;
begin
  R := GetPoolObj(ExplosionPool);
  R.Parent := Self;
  R.Width := 75;
  R.Height := 75;
  R.RotationAngle := Angle - 90;
  R.Stroke.Kind := TBrushKind.None;
  R.Fill.Kind := TBrushKind.Bitmap;
  R.Fill.Bitmap.WrapMode := TWrapMode.Tile;
  if not Assigned(R.TagObject) then
  begin
    AE := TBitmapListAnimation.Create(R);
    AE.AnimationBitmap.Assign(AnimateExplosion.AnimationBitmap);
    AE.AnimationCount := AnimateExplosion.AnimationCount;
    AE.AnimationRowCount := AnimateExplosion.AnimationRowCount;
    AE.PropertyName := AnimateExplosion.PropertyName;
    AE.Trigger := AnimateExplosion.Trigger;
    R.TagObject := AE;
    AE.Parent := R;
    AE.OnFinish := AnimateExplosionFinish;
  end
  else
  begin
    AE := TBitmapListAnimation(R.TagObject);
  end;
  AE.Start;
  R.Position.X := X - (R.Width / 2);
  R.Position.Y := Y - (R.Height / 2);
  R.Tag := 2;
  R.TagFloat := 0;
  R.Visible := True;
  R.SendToBack;
  Result := R;
end;

procedure TGameForm.CreateExplosion(X, Y: Single);
begin
  ExplosionList.AddObject('', SpawnExplosion(X, Y, 0));
  PlaySound(EXPLOSION_SFX);
end;

procedure TGameForm.ShowHUD(Stage: Integer);
begin
  Ship.Visible := True;
  HUDLayout.Visible := True;
{$IFDEF DEBUG}
  FPSLBL.Visible := True;
{$ENDIF}
end;

procedure TGameForm.HideHUD(Stage: Integer);
begin
  if Stage <> LEVEL_COMPLETE then
    Ship.Visible := False;
  Thruster.Visible := False;
  HUDLayout.Visible := False;
  FPSLBL.Visible := False;
end;

procedure TGameForm.AssignInterfaceBackground(R: TRectangle);
begin
  BitmapToRectangle(InterfaceBackground.Fill.Bitmap.Bitmap, R);
end;

procedure TGameForm.SetStage(Stage: Integer);
begin
  case Stage of
    MAIN_MENU:
      begin
        MainMenuFrame.Visible := True;
        GameOverFrame.Visible := False;
        InstructionsFrame.Visible := False;
        HighScoresFrame.Visible := False;
        LevelCompleteFrame.Visible := False;
        SettingsFrame.Visible := False;
        GamepadFrame.Visible := False;
        HideHUD(Stage);
        StopMusic;
      end;
    INSTRUCTIONS:
      begin
        MainMenuFrame.Visible := False;
        GameOverFrame.Visible := False;
        AssignInterfaceBackground(InstructionsFrame.Rectangle1);
        InstructionsFrame.Visible := True;
        HideHUD(Stage);
        StopMusic;
      end;
    GAMEPLAY:
      begin
        InstructionsFrame.Visible := False;
        LevelCompleteFrame.Visible := False;
        ShowHUD(Stage);
        PlayMusic;
      end;
    LEVEL_COMPLETE:
      begin
        AssignInterfaceBackground(LevelCompleteFrame.Rectangle1);
        LevelCompleteFrame.Visible := True;
        if PlayerData.Stars > 0 then
        begin
          LevelCompleteFrame.CollectItem1.Fill.Bitmap.Bitmap.Assign
            (CollectItem.Fill.Bitmap.Bitmap);
          LevelCompleteFrame.CollectItem1.Visible := True;
        end
        else
          LevelCompleteFrame.CollectItem1.Visible := False;
        if PlayerData.Stars > 1 then
        begin
          LevelCompleteFrame.CollectItem2.Fill.Bitmap.Bitmap.Assign
            (CollectItem.Fill.Bitmap.Bitmap);
          LevelCompleteFrame.CollectItem2.Visible := True;
        end
        else
          LevelCompleteFrame.CollectItem2.Visible := False;
        if PlayerData.Stars > 2 then
        begin
          LevelCompleteFrame.CollectItem3.Fill.Bitmap.Bitmap.Assign
            (CollectItem.Fill.Bitmap.Bitmap);
          LevelCompleteFrame.CollectItem3.Visible := True;
        end
        else
          LevelCompleteFrame.CollectItem3.Visible := False;
        HideHUD(Stage);
      end;
    GAME_OVER:
      begin
        HighScoresFrame.Visible := False;
        AssignInterfaceBackground(GameOverFrame.Rectangle1);
        GameOverFrame.Visible := True;
        HideHUD(Stage);
      end;
    HIGH_SCORES:
      begin
        MainMenuFrame.Visible := False;
        GameOverFrame.Visible := False;
        AssignInterfaceBackground(HighScoresFrame.Rectangle1);
        HighScoresFrame.Visible := True;
        HighScoresFrame.BringToFront;
        HideHUD(Stage);
      end;
    SETTINGS:
      begin
        MainMenuFrame.Visible := False;
        SettingsFrame.Visible := True;
        HideHUD(Stage);
        StopMusic;
      end;
    GAMEPAD:
      begin
        MainMenuFrame.Visible := False;
        GamepadFrame.Visible := True;
        HideHUD(Stage);
        StopMusic;
      end;
  end;

  LastStage := CurrentStage;
  CurrentStage := Stage;
end;

procedure TGameForm.SettingsFrameFullScreenSwitchSwitch(Sender: TObject);
begin
  if FullScreenEnabled then
  begin
    FullScreenEnabled := False;
    GameForm.FullScreen := False;
  end
  else
  begin
    FullScreenEnabled := True;
    GameForm.FullScreen := True;
  end;
end;

procedure TGameForm.ToggleAppTethering(State: Boolean);
begin
  if State=True then
   begin
      try
        TetheringManager.Enabled := True;
        TetheringAppProfile.Enabled := True;
      except on E: Exception do
       begin
        ShowMsgBox('Enabling app tethering caused an error: ' + E.Message)
       end;
      end;
   end
  else
   begin
    try
      TetheringManager.Enabled := False;
      TetheringAppProfile.Enabled := False;
    except on E: Exception do
     begin
      ShowMsgBox('Disabling app tethering caused an error: ' + E.Message)
     end;
    end;
   end;
end;

procedure TGameForm.SettingsFrameHostSwitchSwitch(Sender: TObject);
begin
  if HostEnabled then
  begin
    HostEnabled := False;
    ToggleAppTethering(False);
  end
  else
  begin
    HostEnabled := True;
    if NetworkConnected=True then
     begin
      ToggleAppTethering(True);
     end
    else
     begin
       ShowMsgBox(NO_NETWORK_MESSAGE);
     end;
  end;
end;

procedure TGameForm.SettingsFrameMainMenuBTNClick(Sender: TObject);
begin
  SaveAndExitSettings;
end;

procedure TGameForm.SaveAndExitSettings;
begin
  HostEnabled := SettingsFrame.HostSwitch.IsChecked;
  GyroEnabled := SettingsFrame.GyroSwitch.IsChecked;
  FullScreenEnabled := SettingsFrame.FullScreenSwitch.IsChecked;
  SoundEnabled := SettingsFrame.SoundSwitch.IsChecked;
  MusicEnabled := SettingsFrame.MusicSwitch.IsChecked;
  SaveSettings;
  ShowMainMenu;
end;

function TGameForm.IntersectCircle(R1, R2: TRectangle): Boolean;
var
  Distance: Single;
begin
  Result := False;
  Distance := R1.Position.Point.Distance(R2.Position.Point);
  if Distance < ((R1.Width / 2) + (R2.Width / 2)) then
  begin
    Result := True;
  end;
end;

procedure TGameForm.ProcessAccelerometer;
var
  AccX, AccY: Single;
begin
  if MotionSensor.Sensor=nil then
   Exit;
   
  if (GetScreenOrientation = TScreenOrientation.Portrait) OR
    (GetScreenOrientation = TScreenOrientation.InvertedPortrait) then
  begin
    AccX := (MotionSensor.Sensor.AccelerationX * 10);
    AccY := (MotionSensor.Sensor.AccelerationY * 10);
  end
  else
  begin
    AccY := (MotionSensor.Sensor.AccelerationX * 10);
    AccX := (MotionSensor.Sensor.AccelerationY * 10);
  end;

  if (AccY > -7) then
  begin
    if RScreenOut=True then
      RScreenOut := False;

    if LScreenOut=True then
      LScreenOut := False;

    AccThrustCircle.Visible := True;
    AccFireButtonDown := True;
  end
  else
  begin
    AccThrustCircle.Visible := False;
    AccFireButtonDown := False;
  end;

  if (GetScreenOrientation = TScreenOrientation.Portrait) OR
    (GetScreenOrientation = TScreenOrientation.InvertedPortrait) then
  begin

    // right
    if (AccX > 2) then
    begin
      AccRight.Visible := True;
      AccRightButtonDown := True;
    end
    else
    begin
      AccRight.Visible := False;
      AccRightButtonDown := False;
    end;

    // left
    if (AccX < -2) then
    begin
      AccLeft.Visible := True;
      AccLeftButtonDown := True;
    end
    else
    begin
      AccLeft.Visible := False;
      AccLeftButtonDown := False;
    end;

  end
  else
  begin

    // left
    if (AccX > 2) then
    begin
      AccLeft.Visible := True;
      AccLeftButtonDown := True;
    end
    else
    begin
      AccLeft.Visible := False;
      AccLeftButtonDown := False;
    end;

    // right
    if (AccX < -2) then
    begin
      AccRight.Visible := True;
      AccRightButtonDown := True;
    end
    else
    begin
      AccRight.Visible := False;
      AccRightButtonDown := False;
    end;

  end;

end;

procedure TGameForm.GameLoopTimer(Sender: TObject);
var
  I: Integer;
  CollectAngle: Single;
  ExplosionObj: TRectangle;
  CollectObj: TRectangle;
  Time: Cardinal;
  RScreenLimit : single;

  ShipAngle: Single;
  GroundObj: TRectangle;
begin
{$IFDEF DEBUG}
  Time := TThread.GetTickCount;
{$ENDIF}
  // Check for game over
  if PlayerData.Lives <= 0 then
  begin
    LevelFail;
    GameOver;
    Exit;
  end;

  // Check for level complete
  if (PlayerData.Landed = True) then
  begin
    AddScore(1000 + PlayerData.Fuel);
    LevelComplete;
    Exit;
  end;

  // process accelerometer data
  if MotionSensor.Active = True then
  begin
    ProcessAccelerometer;
  end;

  // process inputs
  if (LeftButtonDown OR AccLeftButtonDown) AND (Ship.RotationAngle > -90) then
    Ship.RotationAngle := Ship.RotationAngle - 2;
  if (RightButtonDown OR AccRightButtonDown) AND (Ship.RotationAngle < 90) then
    Ship.RotationAngle := Ship.RotationAngle + 2;
  if (FireButtonDown OR AccFireButtonDown) then
  begin
    if PlayerData.Fuel > 0 then
    begin
      PlayerData.VerticalVelocity := PlayerData.VerticalVelocity -
        PLAYER_THRUST;
      PlayerData.HorizontalVelocity := PlayerData.HorizontalVelocity +
        PLAYER_THRUST;
      PlayerData.Fuel := Max(0, PlayerData.Fuel - 1);
      PlaySound(FIRE_SFX);
      Thruster.Visible := True;
    end
    else
    begin
      Thruster.Visible := False;
    end;
  end
  else
  begin
    Thruster.Visible := False;
  end;

  // move player
  PlayerData.VerticalVelocity := PlayerData.VerticalVelocity + GAME_GRAVITY;
  PlayerData.HorizontalVelocity :=
    Max(0, PlayerData.HorizontalVelocity - GAME_GRAVITY);

  ShipAngle := (Ship.RotationAngle) * (PI / 180);
  //change movement
   if (Ship.Position.X > ScreenLayout.Position.X) AND
      (Ship.Position.X + Ship.Width < ScreenLayout.Position.X + ScreenLayout.Width)
    then
    begin
      if (LScreenOut = False) And (RScreenOut = False) Then
      begin
        Ship.Position.X := Ship.Position.X + PlayerData.HorizontalVelocity * Sin(ShipAngle);
        if Ship.Position.X < 0 then
        begin
          Ship.Position.X :=-1;
          LScreenOut:=True;
          CatchLastPos:=Ship.Position.X;
        end;
        //ship never reach ScreenLayout.Width, make right screen limit area
        RScreenLimit:=35;
        if Ship.Position.X > (ScreenLayout.Width-RScreenLimit) then
        begin
          //make ship position little bit outside right screen limit area
          Ship.Position.X :=(ScreenLayout.Width-(RScreenLimit-1));
          RScreenOut:=True;
          CatchLastPos:=Ship.Position.X;
        end;
      end;
    end
    else
    begin
      if (Ship.Position.X > ScreenLayout.Position.X) then
      begin
        if RScreenOut = True then
          Ship.Position.X :=CatchLastPos
        else
          Ship.Position.X := (Ship.Position.X - PlayerData.HorizontalVelocity) + PlayerData.HorizontalVelocity * Sin(ShipAngle);
      end
      else
      begin
        if LScreenOut = True then
          Ship.Position.X :=CatchLastPos
        else
          Ship.Position.X := (Ship.Position.X + PlayerData.HorizontalVelocity) +PlayerData.HorizontalVelocity * Sin(ShipAngle);
      end;
    end;

  Ship.Position.Y := Ship.Position.Y + PlayerData.VerticalVelocity;

  if ((Ship.Position.Y + Ship.Height) < 0) then
  begin
    if AboveCircle.Visible = False then
      AboveCircle.Visible := True;
  end
  else
  begin
    if AboveCircle.Visible = True then
    begin
      AboveCircle.Visible := False;
    end;
  end;

  // handle HUD elements
  AboveCircle.Position.X := Ship.Position.X;

  FuelLBL.Text := 'Fuel: ' + IntToStr(PlayerData.Fuel);
  AngleLBL.Text := 'Angle: ' + FormatFloat('0.00', Ship.RotationAngle);
  VVLBL.Text := 'V. Velocity: ' + FormatFloat('0.00',
    PlayerData.VerticalVelocity);

  if (Ship.RotationAngle > -10) AND (Ship.RotationAngle < 10) then
  begin
    if AngleLBL.TextSettings.FontColor <> TAlphaColors.Aqua then
      AngleLBL.TextSettings.FontColor := TAlphaColors.Aqua;
  end
  else
  begin
    if AngleLBL.TextSettings.FontColor <> TAlphaColors.White then
      AngleLBL.TextSettings.FontColor := TAlphaColors.White;
  end;

  if PlayerData.VerticalVelocity < PLAYER_LANDING_VELOCITY then
  begin
    if VVLBL.TextSettings.FontColor <> TAlphaColors.Aqua then
      VVLBL.TextSettings.FontColor := TAlphaColors.Aqua;
  end
  else
  begin
    if VVLBL.TextSettings.FontColor <> TAlphaColors.White then
      VVLBL.TextSettings.FontColor := TAlphaColors.White;
  end;

  // process ground collisions and landing
  if GroundList.Count > 0 then
    for I := 0 to GroundList.Count - 1 do
    begin
      GroundObj := TRectangle(GroundList.Objects[I]);
      if IntersectRect(GroundObj.BoundsRect, Ship.BoundsRect) then
      begin
        if (Ship.RotationAngle > -10) AND (Ship.RotationAngle < 10) AND
          (PlayerData.VerticalVelocity < PLAYER_LANDING_VELOCITY) AND
          (Ship.Position.X > GroundObj.Position.X) AND
          ((Ship.Position.X + Ship.Width) < (GroundObj.Position.X +
          GroundObj.Width)) AND (GroundObj.TagString = 'P') then
        begin
          PlayerData.Landed := True;
        end
        else
        begin
          PlayerHit;
        end;
      end;

    end;

  if Ship.Position.Y > ScreenLayout.Height then
  begin
    PlayerHit;
  end;

  // Handle explosions
  if ExplosionList.Count > 0 then
    for I := ExplosionList.Count - 1 downto 0 do
    begin
      ExplosionObj := TRectangle(ExplosionList.Objects[I]);

      ExplosionObj.TagFloat := ExplosionObj.TagFloat + 0.1;

      if (ExplosionObj.TagFloat > ExplosionObj.Tag) then
      begin
        // ProjList.Objects[I] := nil;
        SetPoolObj(ExplosionPool, ExplosionList[I], ExplosionObj);
        ExplosionList.Delete(I);
      end;
    end;

  // Handle collectable items
  if CollectList.Count > 0 then
    for I := CollectList.Count - 1 downto 0 do
    begin
      CollectObj := TRectangle(CollectList.Objects[I]);
      CollectAngle := CollectObj.RotationAngle * PI / 180;
      CollectObj.Position.X := CollectObj.Position.X + CollectObj.Tag *
        Cos(CollectAngle);
      CollectObj.Position.Y := CollectObj.Position.Y + CollectObj.Tag *
        Sin(CollectAngle);

      if (CollectObj.BoundsRect.CenterPoint.X >=
        (ScreenLayout.Width + (CollectObj.Width / 2))) then
      begin
        CollectObj.Position.X := (ScreenLayout.Position.X + 1) -
          (CollectObj.Width / 2);
      end;

      if (CollectObj.BoundsRect.CenterPoint.Y >=
        (ScreenLayout.Height + (CollectObj.Height / 2))) then
      begin
        CollectObj.Position.Y := (ScreenLayout.Position.Y + 1) -
          (CollectObj.Height / 2);
      end;

      if (CollectObj.BoundsRect.CenterPoint.X <= (ScreenLayout.Position.X -
        (CollectObj.Width / 2))) then
      begin
        CollectObj.Position.X := (ScreenLayout.Width - 1);
      end;

      if (CollectObj.BoundsRect.CenterPoint.Y <= (ScreenLayout.Position.Y -
        (CollectObj.Height / 2))) then
      begin
        CollectObj.Position.Y := (ScreenLayout.Height - 1);
      end;

      if IntersectRect(Ship.BoundsRect, CollectObj.BoundsRect) then
      begin
        AddScore(100 * PlayerData.Level);
        CollectObj.TagFloat := CollectObj.TagFloat + 1;
        PlayerData.Stars := PlayerData.Stars + 1;
        PlaySound(COLLECT_SFX);
      end;


      if (CollectObj.TagFloat > 0) then
      begin
        SetPoolObj(CollectPool, CollectList[I], CollectObj);
        CollectList.Delete(I);
      end;
    end;

  // Handle music loop
  if (MusicPlayer.CurrentTime >= MusicPlayer.Duration) OR
    (MusicPlayer.State = TMediaState.Stopped) then
  begin
    MusicPlayer.CurrentTime := 0;
    //MusicPlayer.Stop;
    PlayMusic;
  end;

{$IFDEF DEBUG}
  FPSLBL.Text := IntToStr(TThread.GetTickCount - Time) + ' ms';
{$ENDIF}
end;

function TGameForm.GetTargetAngle(TargetX, TargetY, OriginX,
  OriginY: Single): Single;
var
  Radians: Single;
begin
  Radians := ArcTan2(TargetY - OriginY, TargetX - OriginX);
  Result := Radians / (PI / 180) + 90;
end;

procedure TGameForm.GyroBTNClick(Sender: TObject);
begin
 GyroToggle;
end;

procedure TGameForm.GyroToggle;
begin
  if GyroEnabled then
  begin
    GyroEnabled := False;
    GyroOffLine.Visible := True;
    MotionSensor.Active := False;
    AccFireButtonDown := False;
    AccThrustCircle.Visible := False;
    AccLeftButtonDown := False;
    AccLeft.Visible := False;
    AccLeftButtonDown := False;
    AccRight.Visible := False;
    AccRightButtonDown := False;
  end
  else
  begin
    GyroEnabled := True;
    GyroOffLine.Visible := False;
    MotionSensor.Active := True;
  end;
end;


procedure TGameForm.PlayerHit;
begin
  CreateExplosion(Ship.Position.X + (Ship.Width / 2),
    Ship.Position.Y + (Ship.Height / 2));
  Ship.Visible := False;
  PlayerData.Lives := PlayerData.Lives - 1;
  PlayerData.SpeedX := 0;
  PlayerData.SpeedY := 0;
  PlayerData.VerticalVelocity := 0;
  PlayerData.HorizontalVelocity := 10;
  Ship.RotationAngle := 45;
  DisplayLives(PlayerData.Lives);
  if PlayerData.Lives > 0 then
  begin
    CenterPlayer;
    PlaySound(FAIL_SFX);
  end;
end;

procedure TGameForm.DisplayScore;
begin
  ScoreLBL.Text := IntToStr(PlayerData.Score);
end;

procedure TGameForm.DelayedSettingsTimer(Sender: TObject);
begin
  if (FileExists(DataFilePath + MUSIC_FILENAME) AND (MusicPlayer.Filename='')) then
    begin
      try
        MusicPlayer.Filename := DataFilePath + MUSIC_FILENAME;
      except on E: Exception do
       begin
        ShowMsgBox('Music failed to load: ' + E.Message);
       end;
      end;
    end;

  if HostEnabled = True then
    begin
      if NetworkConnected=True then
       begin
        ToggleAppTethering(True);
       end;
    end
  else
    begin
     ToggleAppTethering(False);
    end;
  DelayedSettings.Enabled := False;
end;

procedure TGameForm.DisplayLives(Lives: Integer);
begin
  if Lives > 0 then
    Lives1.Visible := True
  else
    Lives1.Visible := False;
  if Lives > 1 then
    Lives2.Visible := True
  else
    Lives2.Visible := False;
  if Lives > 2 then
    Lives3.Visible := True
  else
    Lives3.Visible := False;
end;

procedure TGameForm.GameOverFrameMainMenuBTNClick(Sender: TObject);
begin
  CleanupGame(False);
  ShowMainMenu;
end;

procedure TGameForm.GameOverFrameMoreGamesBTNClick(Sender: TObject);
begin
  ShowMoreGames;
end;

procedure TGameForm.GameOverFramePlayAgainBTNClick(Sender: TObject);
begin
  CleanupGame(False);
  StartGame;
end;

procedure TGameForm.GamepadFrameCloseBTNClick(Sender: TObject);
begin
  if HostEnabled then
  begin
   if NetworkConnected=True then
    begin
     ToggleAppTethering(True);
    end;
  end;
  SetStage(MAIN_MENU);
end;

procedure TGameForm.CloseHighScores;
begin
  HighScoresFrame.CloseInputBox;
  if LastStage = MAIN_MENU then
    SetStage(MAIN_MENU)
  else
    SetStage(GAME_OVER);
end;

procedure TGameForm.ShowGamePad;
begin
  PollNetworkState;

  if NetworkConnected then
   begin
     if HostEnabled then
     begin
        ToggleAppTethering(False);
     end;
     GamepadFrame.TetheringName := TetheringAppProfile.Text;
     GamepadFrame.GPTetheringManager.Enabled := True;
     GamepadFrame.GPTetheringAppProfile.Enabled := True;
     SetStage(GAMEPAD);
   end
  else
   begin
    ShowMsgBox(NO_NETWORK_MESSAGE);
   end;
end;

procedure TGameForm.HighScoresFrameCancelBTNClick(Sender: TObject);
begin
  HighScoresFrame.CancelBTNClick(Sender);

end;

procedure TGameForm.HighScoresFrameContinueBTNClick(Sender: TObject);
begin
  CloseHighScores;
end;

procedure TGameForm.HighScoresFrameOkayBTNClick(Sender: TObject);
begin
  HighScoresFrame.OkayBTNClick(Sender);

end;

procedure TGameForm.HUDLayoutMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  ClearButtons;
end;

procedure TGameForm.FireActionDownExecute(Sender: TObject);
begin
  // called from RunRemoteActionAsync. Requires TThread.Synchronize for UI changes.
TThread.Synchronize(nil,
                    procedure
                    begin
                      case CurrentStage of
                        MAIN_MENU:
                          begin
                            StartGame;
                          end;
                        INSTRUCTIONS:
                          begin
                            InitAndPlayGame;
                          end;
                        GAMEPLAY:
                          begin
                             UpDownEvent(Sender);
                          end;
                        LEVEL_COMPLETE:
                          begin
                            ContinueGame;
                          end;
                        GAME_OVER:
                          begin
                            CleanupGame(False);
                            StartGame;
                          end;
                        HIGH_SCORES:
                          begin
                            CloseHighScores;
                          end;
                        SETTINGS:
                          begin
                            SaveAndExitSettings;
                          end;
                        GAMEPAD:
                          begin
                          end;
                      end;
                    end);
end;

procedure TGameForm.FireActionUpExecute(Sender: TObject);
begin
  // called from RunRemoteActionAsync. Requires TThread.Synchronize for UI changes.
  UpUpEvent(Sender);
end;

procedure TGameForm.FireBTNMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  UpDownEvent(Sender);
end;

procedure TGameForm.FireBTNMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  UpUpEvent(Sender);
end;

procedure TGameForm.SaveSettings;
var
  IniFile: TMemIniFile;
begin
  IniFile := TMemIniFile.Create(DataFilePath + 'Settings.ini');
  IniFile.WriteBool('Settings', 'MusicEnabled', MusicEnabled);
  IniFile.WriteBool('Settings', 'SoundEnabled', SoundEnabled);
  IniFile.WriteBool('Settings', 'GyroEnabled', GyroEnabled);
  IniFile.WriteBool('Settings', 'HostEnabled', HostEnabled);
  IniFile.WriteBool('Settings', 'FullScreenEnabled', FullScreenEnabled);
  IniFile.UpdateFile;
  IniFile.Free;
end;

procedure TGameForm.LoadSettings;
var
  IniFile: TMemIniFile;
begin
  IniFile := TMemIniFile.Create(DataFilePath + 'Settings.ini');
  MusicEnabled := IniFile.ReadBool('Settings', 'MusicEnabled', True);
  if MusicEnabled = False then
    MusicOffLine.Visible := True;
  SoundEnabled := IniFile.ReadBool('Settings', 'SoundEnabled', True);
  if SoundEnabled = False then
    SoundOffLine.Visible := True;
  GyroEnabled := IniFile.ReadBool('Settings', 'GyroEnabled', True);
  if GyroEnabled = False then
    GyroOffLine.Visible := True;
  HostEnabled := IniFile.ReadBool('Settings', 'HostEnabled', False);
  FullScreenEnabled := IniFile.ReadBool('Settings', 'FullScreenEnabled', False);
  if FullScreenEnabled = True then
    GameForm.FullScreen := True;
  IniFile.Free;

  PollNetworkState;
  DelayedSettings.Enabled := True;
end;

function TGameForm.CheckNetworkState: Boolean;
begin
//Result := False;
//Exit;
  try
    if IdTCPClient1.Connected=False then
     begin
       IdTCPClient1.ReadTimeout := 2000;
       IdTCPClient1.ConnectTimeout := 2000;
       IdTCPClient1.Host := NETWORK_ADDRESS;
       IdTCPClient1.Port := 80;
       IdTCPClient1.Connect;
       IdTCPClient1.Disconnect;
     end;
    Result := True;
  except on E: Exception do
   begin
     Result := False;
   end;
  end;
end;

procedure TGameForm.PollNetworkState;
var
FThread: TAnonymousThread<Boolean>;
begin
  if NetworkChecking=False then
   begin
    NetworkChecking := True;

    FThread := TAnonymousThread<Boolean>.Create(
      function: Boolean
      begin
        Result := GameForm.CheckNetworkState;
      end,
      procedure(AResult: Boolean)
      begin
        // Runs in main thread
        if AResult=True then
          NetworkConnected := True
        else
          NetworkConnected := False;
        NetworkChecking := False;
      end,
      procedure(AException: Exception)
      begin
        // Runs in main thread
        // do something if there is an exception
        NetworkConnected := False;
        NetworkChecking := False;
      end,
    False);
    //Application.ProcessMessages;
   end;
end;

procedure TGameForm.ExitDialog(Sender: TObject);
begin
  TDialogServiceAsync.MessageDialog('Exit the game?', System.UITypes.TMsgDlgType.mtInformation,
    [
    System.UITypes.TMsgDlgBtn.mbYes, System.UITypes.TMsgDlgBtn.mbNo
    ], System.UITypes.TMsgDlgBtn.mbYes, 0,
    procedure(const AResult: TModalResult)
    begin
      case AResult of
        mrYes:
          begin
            TThread.Synchronize(nil,
                                procedure
                                begin
                                    LevelFail;
                                    CleanupGame(False);
                                    ShowMainMenu;
                                end);
          end;
        mrNo:
          begin
           // pressed no
          end;
      end;
    end
    );
end;

procedure TGameForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
{$IFNDEF DEBUG}
{$IF DEFINED(MSWINDOWS) OR (DEFINED(MACOS) AND NOT DEFINED(IOS))}
  case CurrentStage of
    GAMEPLAY:
      begin
        ExitDialog(Sender);
        CanClose := False;
      end;
  end;
{$ENDIF}
{$ENDIF}
end;

procedure TGameForm.FormCreate(Sender: TObject);
var
  FMXApplicationEventService: IFMXApplicationEventService;
begin
  AssetLayout.Visible := False;

{$IF DEFINED(MSWINDOWS)}
  SettingsFilePath := ExtractFilePath(ParamStr(0));
{$ELSE}
  SettingsFilePath := System.IOUtils.TPath.GetDocumentsPath +
    System.SysUtils.PathDelim;
{$ENDIF}
{$IF DEFINED(MSWINDOWS)}
  DataFilePath := ExtractFilePath(ParamStr(0));
{$ELSE}
{$IF (DEFINED(MACOS) AND NOT DEFINED(IOS))}
  DataFilePath := System.IOUtils.TPath.GetHomePath +
    System.SysUtils.PathDelim;
{$ELSE}
  DataFilePath := System.IOUtils.TPath.GetDocumentsPath +
    System.SysUtils.PathDelim;
{$ENDIF}
{$ENDIF}

  ScreenOrientation := GetScreenOrientation;

  OrientationChangedId := TMessageManager.DefaultManager.SubscribeToMessage
    (TOrientationChangedMessage, OrientationChanged);

  if TPlatformServices.Current.SupportsPlatformService
    (IFMXApplicationEventService, IInterface(FMXApplicationEventService)) then
    FMXApplicationEventService.SetApplicationEventHandler(HandleAppEvent);

  LoadSettings;

  AudioManager := TAudioManager.Create;
  RegisterSound(DataFilePath + 'thruster.wav');
  RegisterSound(DataFilePath + 'explosion.wav');
  RegisterSound(DataFilePath + 'fail.wav');
  RegisterSound(DataFilePath + 'win.wav');
  RegisterSound(DataFilePath + 'alien.wav');
  RegisterSound(DataFilePath + 'collect.wav');

  SetStage(MAIN_MENU);
end;

procedure TGameForm.FormDestroy(Sender: TObject);
begin
  // CleanupGame(False);
  SaveSettings;
  TMessageManager.DefaultManager.Unsubscribe(TOrientationChangedMessage,
    OrientationChangedId);
end;

procedure TGameForm.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  case CurrentStage of
    GAMEPLAY:
      begin
        case KeyChar of
          'Z', 'z', ' ':
            begin
              UpDownEvent(Sender);
            end;
          'W', 'w':
            begin
              UpDownEvent(Sender);
            end;
          'A', 'a':
            begin
              LeftDownEvent(Sender);
            end;
          'D', 'd':
            begin
              RightDownEvent(Sender);
            end;
        end;
        case Key of
          vkUp:
            begin
              UpDownEvent(Sender);
            end;
          vkLeft:
            begin
              LeftDownEvent(Sender);
            end;
          vkRight:
            begin
              RightDownEvent(Sender);
            end;
        end;
      if Key<>vkHardwareBack then Key := 0;
     end;
    GAMEPAD:
      GamepadFrame.HandleKeyDown(Sender, Key, KeyChar, Shift);
  end;
  case Key of
    vkHardwareBack:
      begin
        case CurrentStage of
          MAIN_MENU:
            begin
              // allow default functionality
            end;
          INSTRUCTIONS:
            begin
              Key := 0;
              ShowMainMenu;
            end;
          GAMEPLAY, LEVEL_COMPLETE:
            begin
              Key := 0;
              ExitDialog(Sender);
            end;
          GAME_OVER:
            begin
              Key := 0;
              CleanupGame(False);
              ShowMainMenu;
            end;
          HIGH_SCORES:
            begin
              Key := 0;
              CloseHighScores;
            end;
        end;
      end;
  end;
end;

procedure TGameForm.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  case CurrentStage of
    GAMEPLAY:
    begin
      case KeyChar of
        'Z', 'z', ' ':
          begin
            UpUpEvent(Sender);
          end;
        'W', 'w':
          begin
            UpUpEvent(Sender);
          end;
        'A', 'a':
          begin
            LeftUpEvent(Sender);
          end;
        'D', 'd':
          begin
            RightUpEvent(Sender);
          end;
      end;
      case Key of
        vkUp:
          begin
            UpUpEvent(Sender);
          end;
        vkLeft:
          begin
            LeftUpEvent(Sender);
          end;
        vkRight:
          begin
            RightUpEvent(Sender);
          end;
      end;
      if Key<>vkHardwareBack then Key := 0;
    end;
    GAMEPAD:
      GamepadFrame.HandleKeyUp(Sender, Key, KeyChar, Shift);
  end;
end;

procedure TGameForm.FormMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Single);
begin
  ClearButtons;
end;

procedure TGameForm.FormResize(Sender: TObject);
begin
ReOrientgame;
end;

procedure TGameForm.ClearButtons;
begin
  case CurrentStage of
   GAMEPAD:
    begin
      GamePadFrame.ClearButtons;
    end;
   else
    begin
      LeftButtonDown := False;
      RightButtonDown := False;
      FireButtonDown := False;
    end;
  end;
end;

procedure TGameForm.InstructionsFrameContinueBTNClick(Sender: TObject);
begin
  InitAndPlayGame;
end;

procedure TGameForm.InitAndPlayGame;
begin
  InitGame;
  InitPlayer;
  PlayGame;
end;

procedure TGameForm.LeftActionDownExecute(Sender: TObject);
begin
  // called from RunRemoteActionAsync. Requires TThread.Synchronize for UI changes.
  LeftDownEvent(Sender);
end;

procedure TGameForm.LeftActionUpExecute(Sender: TObject);
begin
  // called from RunRemoteActionAsync. Requires TThread.Synchronize for UI changes.
  LeftUpEvent(Sender);
end;

procedure TGameForm.LeftBTNMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  LeftDownEvent(Sender);
end;

procedure TGameForm.LeftBTNMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  LeftUpEvent(Sender);
end;

procedure TGameForm.MainMenuFrameGamepadBTNClick(Sender: TObject);
begin
 ShowGamePad;
end;

procedure TGameForm.MainMenuFrameHighScoresBTNClick(Sender: TObject);
begin
  ShowHighScores;
end;

procedure TGameForm.MainMenuFrameMoreGamesBTNClick(Sender: TObject);
begin
  ShowMoreGames;
end;

procedure TGameForm.MainMenuFramePlayBTNClick(Sender: TObject);
begin
  StartGame;
end;

procedure TGameForm.MainMenuFrameSettingsBTNClick(Sender: TObject);
begin
 ShowSettings;
end;

procedure TGameForm.MusicBTNClick(Sender: TObject);
begin
  MusicToggle;
end;

procedure TGameForm.MusicToggle;
begin
  if MusicEnabled then
  begin
    MusicEnabled := False;
    MusicOffLine.Visible := True;
    StopMusic;
  end
  else
  begin
    MusicEnabled := True;
    MusicOffLine.Visible := False;
    if CurrentStage <> SETTINGS then
      PlayMusic;
  end;
end;

procedure TGameForm.RightActionDownExecute(Sender: TObject);
begin
  // called from RunRemoteActionAsync. Requires TThread.Synchronize for UI changes.
  RightDownEvent(Sender);
end;

procedure TGameForm.RightActionUpExecute(Sender: TObject);
begin
  // called from RunRemoteActionAsync. Requires TThread.Synchronize for UI changes.
  RightUpEvent(Sender);
end;

procedure TGameForm.RightBTNMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  RightDownEvent(Sender);
end;

procedure TGameForm.RightBTNMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  RightUpEvent(Sender);
end;

procedure TGameForm.StartGame;
begin
 if NetworkConnected=True then
  begin
   ToggleAppTethering(True);
  end;

 try
   if GamepadFrame.GPTetheringManager.Enabled=True then
     GamepadFrame.GPTetheringManager.Enabled := False;
   if GamepadFrame.GPTetheringAppProfile.Enabled=True then
     GamepadFrame.GPTetheringAppProfile.Enabled := False;
 except on E: Exception do
  begin
  end;
 end;

  SetStage(INSTRUCTIONS);
end;

procedure TGameForm.StartGameLoop;
begin
  if GyroEnabled then
  begin
    MotionSensor.Active := True;
    GyroOffLine.Visible := False;
  end
  else
  begin
    GyroOffLine.Visible := True;
    AccFireButtonDown := False;
    AccThrustCircle.Visible := False;
    AccLeftButtonDown := False;
    AccLeft.Visible := False;
    AccLeftButtonDown := False;
    AccRight.Visible := False;
    AccRightButtonDown := False;
  end;
  GameLoop.Enabled := True;
end;

procedure TGameForm.StopGameLoop;
begin
  if GyroEnabled then
    MotionSensor.Active := False;
  GameLoop.Enabled := False;
  ClearButtons;
end;

procedure TGameForm.ShowMainMenu;
begin
  SetStage(MAIN_MENU);
end;

procedure TGameForm.ShowMoreGames;
begin
  // launch http://www.embarcadero.com/
end;

procedure TGameForm.ShowSettings;
begin
  PollNetworkState;

  SettingsFrame.HostSwitch.IsChecked := HostEnabled;
  SettingsFrame.GyroSwitch.IsChecked := GyroEnabled;
  SettingsFrame.FullScreenSwitch.IsChecked := FullScreenEnabled;
  SettingsFrame.SoundSwitch.IsChecked := SoundEnabled;
  SettingsFrame.MusicSwitch.IsChecked := MusicEnabled;
  SetStage(SETTINGS);
end;

procedure TGameForm.SoundBTNClick(Sender: TObject);
begin
  SoundToggle;
end;

procedure TGameForm.SoundToggle;
begin
  if SoundEnabled then
  begin
    SoundEnabled := False;
    SoundOffLine.Visible := True;
  end
  else
  begin
    SoundEnabled := True;
    SoundOffLine.Visible := False;
  end;
end;

procedure TGameForm.LevelComplete;
begin
  StopGameLoop;
  PlaySound(WIN_SFX);
  LevelCompleteFrame.InfoText.Text := 'Score: ' +
    IntToStr(PlayerData.Score) + #13#10;
  SetStage(LEVEL_COMPLETE);
end;

procedure TGameForm.LevelCompleteFrameContinueBTNClick(Sender: TObject);
begin
  ContinueGame;
end;

procedure TGameForm.InitGame;
begin
  GroundList := TStringList.Create;
  GroundPool := TStringList.Create;
  ExplosionList := TStringList.Create;
  ExplosionPool := TStringList.Create;
  CollectList := TStringList.Create;
  CollectPool := TStringList.Create;

  CleanedUp := False;

  Ship.RotationAngle := 0;
  CenterPlayer;
end;

procedure TGameForm.InitPlayer;
begin
  PlayerData := TPlayerData.Create(Self);
  PlayerData.Lives := PLAYER_LIVES;
  PlayerData.Stars := 0;
  PlayerData.SpeedX := 0;
  PlayerData.SpeedY := 0;
  PlayerData.Level := 1;
  PlayerData.Thrust := 0.5;

  PlayerData.Fuel := Trunc(ScreenLayout.Width * ScreenLayout.Height /
    PLAYER_FUEL);
  PlayerData.VerticalVelocity := 0;
  PlayerData.HorizontalVelocity := 10;
  PlayerData.Landed := False;

  if LivesSet = False then
  begin
    BitmapToRectangle(Ship.Fill.Bitmap.Bitmap, Lives1);
    BitmapToRectangle(Ship.Fill.Bitmap.Bitmap, Lives2);
    BitmapToRectangle(Ship.Fill.Bitmap.Bitmap, Lives3);
  end;

  DisplayLives(PlayerData.Lives);
  DisplayScore;

  CenterPlayer;

  Ship.TagObject := PlayerData;
  Ship.RotationAngle := 90;

end;

procedure TGameForm.BitmapToRectangle(B: TBitmap; R: TRectangle);
begin
  R.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
  R.Fill.Kind := TBrushKind.Bitmap;
  R.Fill.Bitmap.Bitmap.Assign(B);
end;

procedure TGameForm.AnimateExplosionFinish(Sender: TObject);
begin
  TRectangle(TBitmapListAnimation(Sender).Owner).Visible := False;
end;

procedure TGameForm.CenterPlayer;
begin
  Ship.Position.X := ScreenLayout.Position.X + 20;
  Ship.Position.Y := ScreenLayout.Position.Y + 20;
  Ship.Visible := True;
  RScreenOut:=False;
  LScreenOut:=False;
end;

procedure TGameForm.CleanupGame(Continue: Boolean);
begin
 if CleanedUp=False then
  begin
    GroundList.OwnsObjects := True;
    GroundPool.OwnsObjects := True;
    ExplosionList.OwnsObjects := True;
    ExplosionPool.OwnsObjects := True;
    CollectList.OwnsObjects := True;
    CollectPool.OwnsObjects := True;

    if (Continue = False) then
      FreeAndNil(PlayerData);
    FreeAndNil(GroundList);
    FreeAndNil(GroundPool);
    FreeAndNil(ExplosionList);
    FreeAndNil(ExplosionPool);
    FreeAndNil(CollectList);
    FreeAndNil(CollectPool);
    CleanedUp := True;
  end;
end;

procedure TGameForm.PlayGame;
var
  I: Integer;
  NextX, NextY: Single;
  R: TRectangle;
  GroundType: Integer;
begin
  SetStage(GAMEPLAY);

  // Spawn collectable
  for I := 1 to 3 do
  begin
    CollectList.AddObject('', SpawnCollectItem(RandomRange(1,
      Trunc(ScreenLayout.Width - CollectItem.Width)),
      RandomRange(1, Trunc(ScreenLayout.Height / 4)), 0));
  end;

  // spawn ground and landing pads
  for I := 0 to 100 do
  begin
    if GroundList.Count > 0 then
    begin
      R := TRectangle(GroundList.Objects[GroundList.Count - 1]);
      NextX := R.Position.X + R.Width;
      NextY := ScreenLayout.Position.Y + ScreenLayout.Height;

      if NextX > ScreenLayout.Width then
        Break;
    end
    else
    begin
      NextX := ScreenLayout.Position.X;
      NextY := ScreenLayout.Position.Y + ScreenLayout.Height;
    end;

    GroundType := Random(5);

    // ensure there is at least one landing pad
    if I = 4 then
      GroundType := 1;

    GroundList.AddObject('', SpawnGround(NextX, NextY, GroundType, Ship.Width));
  end;

  CliffRect.BringToFront;
  Ship.RotationAngle := 45;
  StartGameLoop;
end;

procedure TGameForm.ContinueGame;
begin
  CleanupGame(True);
  InitGame;
  PlayerData.SpeedX := 0;
  PlayerData.SpeedY := 0;
  PlayerData.Level := PlayerData.Level + 1;
  PlayerData.Stars := 0;

  PlayerData.HorizontalVelocity := 10;
  PlayerData.VerticalVelocity := 0;
  PlayerData.Fuel := Trunc(ScreenLayout.Width * ScreenLayout.Height /
    (PLAYER_FUEL + (PLAYER_FUEL * PlayerData.Level * 0.1)));
  PlayerData.Landed := False;
  PlayGame;
end;

procedure TGameForm.LevelFail;
begin
  StopGameLoop;
  PlayerData.SpeedX := 0;
  PlayerData.SpeedY := 0;
  PlaySound(FAIL_SFX);
end;

procedure TGameForm.GameOver;
begin
  SetStage(GAME_OVER);
  Application.ProcessMessages;
  HighScoresFrame.InitFrame;
  HighScoresFrame.AddScore('', PlayerData.Score);
  SetStage(HIGH_SCORES);
end;

procedure TGameForm.ResetGround;
var
  I: Integer;
  GroundObj: TRectangle;
begin
  if GroundList.Count > 0 then
    for I := 0 to GroundList.Count - 1 do
    begin
      GroundObj := TRectangle(GroundList.Objects[I]);
      GroundObj.Position.Y := (ScreenLayout.Position.Y + ScreenLayout.Height) -
        GroundObj.Height;
    end;
end;

procedure TGameForm.ReOrientgame;
begin
  if CurrentStage = GAMEPLAY then
  begin
    ResetGround;
    if Ship.Position.X > ScreenLayout.Width then
    begin
      Ship.Position.X := ScreenLayout.Width - Ship.Width - 25;
    end;
  end;
end;

procedure TGameForm.OrientationChanged(const Sender: TObject;
  const Msg: TMessage);
begin
 ReOrientgame;
end;

function TGameForm.GetScreenOrientation: TScreenOrientation;
begin
  Result := IFMXScreenService(TPlatformServices.Current.GetPlatformService
    (IFMXScreenService)).GetScreenOrientation;
end;

procedure RenderingSetupCallback(const Sender, Context: TObject;
  var ColorBits, DepthBits: Integer; var Stencil: Boolean;
  var Multisamples: Integer);
begin
  // Override OpenGL rendering setup to use custom values.
  ColorBits := 16; // default is 24
  DepthBits := 0; // default is 24
  Stencil := False; // default is True
  Multisamples := 0; // default depends on TForm.Quality or TForm3D.Multisample
end;

procedure RegisterRenderingSetup;
var
  SetupService: IFMXRenderingSetupService;
begin
  if TPlatformServices.Current.SupportsPlatformService
    (IFMXRenderingSetupService, IInterface(SetupService)) then
    SetupService.Subscribe(RenderingSetupCallback);
  // There is also SetupService.Unsubscribe, which removes the hook.
end;

initialization
// enables Metal API on iOS and macOS
FMX.Types.GlobalUseMetal := True;

// enables the GPU on Windows
// FMX.Types.GlobalUseGPUCanvas := True;
RegisterRenderingSetup;
Randomize;

end.
