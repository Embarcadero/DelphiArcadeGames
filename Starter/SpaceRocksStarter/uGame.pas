//---------------------------------------------------------------------------

// This software is Copyright (c) 2016 Embarcadero Technologies, Inc.
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
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.IniFiles,
  System.Messaging, System.IOUtils, FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Layouts, System.Math, FMX.StdCtrls, FMX.Ani, FMX.Media, FMX.Platform,
  FMX.Filter.Effects, FMX.Effects, AudioManager, uMainMenu, uInstructions, uLevelComplete, uGameOver,
  uHighScores, System.Sensors,  System.Sensors.Components, System.Actions, FMX.ActnList,
  uSettings, AnonThread, FMX.Controls.Presentation, FMX.DialogService.Async;

type
  TPlayerData = class(TFMXObject)
    public
     SpeedX: Single;
     SpeedY: Single;
     Thrust: Single;
     Decay: Single;
     CurrentSpeed: Single;
     MaxSpeed: Single;
     RotationSpeed: Single;
     Lives: Integer;
     Health: Integer;
     Invulnerable: Integer;
     InvulnerableInterval: Integer;
     Score: Integer;
     Level: Integer;
     FireSpeed: Integer;
     FireInterval: Integer;
     ProjDuration: Single;
  end;

  TGameForm = class(TForm)
    Ship: TRectangle;
    GameLoop: TTimer;
    ScreenLayout: TLayout;
    InstructionsFrame: TFrameInstructions;
    GameOverFrame: TFrameGameOver;
    LevelCompleteFrame: TFrameLevelComplete;
    InvulnerableTimer: TTimer;
    AssetLayout: TLayout;
    Asteroid1: TRectangle;
    Enemy: TRectangle;
    StyleBook: TStyleBook;
    Projectile: TRectangle;
    Explosion: TRectangle;
    AnimateExplosion: TBitmapListAnimation;
    EnemyProjectile: TRectangle;
    CollectItem: TRectangle;
    AnimateCollectItem: TBitmapListAnimation;
    LeftBTN: TRectangle;
    ForwardBTN: TRectangle;
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
    Health1: TRectangle;
    Health2: TRectangle;
    Health3: TRectangle;
    HealthBar: TRectangle;
    BitmapListAnimation1: TBitmapListAnimation;
    Asteroid2: TRectangle;
    Asteroid3: TRectangle;
    Thruster: TRectangle;
    SettingsFrame: TFrameSettings;
    MainMenuFrame: TFrameMainMenu;
    ActionList: TActionList;
    LeftActionDown: TAction;
    LeftActionUp: TAction;
    RightActionDown: TAction;
    RightActionUp: TAction;
    UpActionDown: TAction;
    UpActionUp: TAction;
    FireActionDown: TAction;
    FireActionUp: TAction;
    MotionSensor: TMotionSensor;
    GyroBTN: TRectangle;
    GyroOffLine: TLine;
    AccLeft: TCircle;
    AccRight: TCircle;
    AccUp: TCircle;
    HighScoresFrame: TFrameHighScores;
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
    procedure FireBTNClick(Sender: TObject);
    procedure MainMenuFramePlayBTNClick(Sender: TObject);
    procedure InstructionsFrameContinueBTNClick(Sender: TObject);
    procedure GameOverFramePlayAgainBTNClick(Sender: TObject);
    procedure GameOverFrameMainMenuBTNClick(Sender: TObject);
    procedure GameOverFrameMoreGamesBTNClick(Sender: TObject);
    procedure MainMenuFrameMoreGamesBTNClick(Sender: TObject);
    procedure InvulnerableTimerTimer(Sender: TObject);
    procedure ForwardBTNMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure ForwardBTNMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
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
    procedure GamepadFrameCloseBTNClick(Sender: TObject);
    procedure LeftActionDownExecute(Sender: TObject);
    procedure LeftActionUpExecute(Sender: TObject);
    procedure RightActionDownExecute(Sender: TObject);
    procedure RightActionUpExecute(Sender: TObject);
    procedure UpActionDownExecute(Sender: TObject);
    procedure UpActionUpExecute(Sender: TObject);
    procedure SettingsFrameMainMenuBTNClick(Sender: TObject);
    procedure FireActionDownExecute(Sender: TObject);
    procedure FireActionUpExecute(Sender: TObject);
    procedure SettingsFrameFullScreenSwitchSwitch(Sender: TObject);
    procedure SettingsFrameHostSwitchSwitch(Sender: TObject);
    procedure MainMenuFrameGamepadBTNClick(Sender: TObject);
    procedure MainMenuFrameSettingsBTNClick(Sender: TObject);
    procedure GyroBTNClick(Sender: TObject);
    procedure DelayedSettingsTimer(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
    LeftButtonDown: Boolean;
    RightButtonDown: Boolean;
    ForwardButtonDown: Boolean;
    FireButtonDown: Boolean;
    AccLeftButtonDown: Boolean;
    AccRightButtonDown: Boolean;
    AccUpButtonDown: Boolean;
    RockList: TStringList;
    ProjList: TStringList;
    ExplosionList: TStringList;
    EnemyList: TStringList;
    EnemyProjList: TStringList;
    CollectList: TStringList;
    PlayerData: TPlayerData;
    RockPool: TStringList;
    ProjPool: TStringList;
    ExplosionPool: TStringList;
    EnemyPool: TStringList;
    EnemyProjPool: TStringList;
    CollectPool: TStringList;
    AudioManager: TAudioManager;
    CurrentStage: Integer;
    LastStage: Integer;
    DataFilePath: String;
    MusicEnabled: Boolean;
    SoundEnabled: Boolean;
    GyroEnabled: Boolean;
    HostEnabled: Boolean;
    FullScreenEnabled: Boolean;
    LivesSet: Boolean;
    CanSwapXY: Boolean;
    CleanedUp: Boolean;

    ScreenOrientation: TScreenOrientation;
    OrientationChangedId: Integer;

    NetworkConnected: Boolean;
    NetworkChecking: Boolean;

    procedure SwapListXY(List: TStringList);
    procedure SwapXY;
    procedure OrientationChanged(const Sender: TObject; const Msg: TMessage);
    function GetScreenOrientation: TScreenOrientation;

    procedure StartGame;
    procedure PlayGame;
    procedure StartGameLoop;
    procedure StopGameLoop;
    procedure ShowMainMenu;
    procedure ShowHighScores;
    procedure ShowMoreGames;
    procedure ShowSettings;
    procedure ShowGamePad;
    procedure MusicToggle;
    procedure SoundToggle;
    procedure SaveAndExitSettings;
    procedure InitAndPlayGame;
    procedure InitPlayer;
    procedure LevelComplete;
    procedure LevelFail;
    procedure GameOver;
    procedure InitGame;
    procedure ShowHUD;
    procedure HideHUD;
    procedure ContinueGame;
    procedure CloseHighScores;
    procedure CleanupGame(Continue: Boolean);
    procedure ClearButtons;
    procedure FirePlayerProj;
    procedure FireDownEvent(Sender: TObject);
    procedure FireUpEvent(Sender: TObject);
    procedure LeftDownEvent(Sender: TObject);
    procedure LeftUpEvent(Sender: TObject);
    procedure RightDownEvent(Sender: TObject);
    procedure RightUpEvent(Sender: TObject);
    procedure UpDownEvent(Sender: TObject);
    procedure UpUpEvent(Sender: TObject);
    procedure CenterPlayer;
    procedure AddScore(I: Integer);
    procedure DisplayLives(Lives: Integer);
    procedure DisplayHealth(Health: Integer);
    procedure DisplayScore;
    procedure PlayerHit;
    function SpawnProj(Source: TRectangle; X,Y: Single; Angle: Single): TRectangle;
    function SpawnRock(X,Y: Single; Size: Integer): TRectangle;
    function SpawnCollectItem(X,Y: Single; Size: Integer): TRectangle;
    function SpawnEnemy(X,Y: Single; Angle: Single; Speed: Integer): TRectangle;
    function SpawnEnemyProj(Source: TRectangle; X, Y: Single; Angle: Single): TRectangle;
    function SpawnExplosion(X,Y: Single; Angle: Single): TRectangle;
    procedure CreateExplosion(X,Y:Single);

    procedure BitmapToRectangle(B: TBitmap; R: TRectangle);
    function IntersectCircle(R1,R2: TRectangle): Boolean;
    function GetTargetAngle(TargetX, TargetY, OriginX, OriginY: Single): Single;
    function GetPoolObj(Pool: TStringList): TRectangle;
    procedure SetPoolObj(Pool: TStringList; Name: String; Obj: TRectangle);
    procedure PauseBitmapListAnimations(Value: Boolean);

    procedure PlaySound(Index: Integer);
    function RegisterSound(Filename: String): Integer;
    procedure PlayMusic;
    procedure StopMusic;
    function GetMaxVolume: Single;

    procedure SetStage(Stage: Integer);
    procedure SaveSettings;
    procedure LoadSettings;
    procedure GyroToggle;
    procedure ProcessAccelerometer;
  public
    { Public declarations }
    SettingsFilePath: String;
    function HandleAppEvent(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
    procedure ShowMsgBox(S: String);
    procedure ExitDialog(Sender: TObject);
  end;
  const
    // Player
    PLAYER_HEALTH = 3;
    PLAYER_LIVES = 3;
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
    PROJ_SPEED = 12;
    PLAYER_SPEED = 5;
    ENEMYPROJ_SPEED = 10;
    ENEMY_SPEED = 7;
    ROCK_SPEED = 2;
    COLLECTITEM_SPEED = 1;
    COLLECTITEM_DURATION = 6;
    // Network Connections
    NETWORK_ADDRESS = 'www.embarcadero.com';
    NO_NETWORK_MESSAGE = 'A network connection is needed for this feature and it was not detected.';
    NO_GAMEPAD = 'Starter edition doesn''t support app tethering.';

var
  GameForm: TGameForm;

implementation

{$R *.fmx}

procedure TGameForm.ShowMsgBox(S: String);
begin
  TDialogServiceAsync.MessageDialog(S, System.UITypes.TMsgDlgType.mtInformation,[System.UITypes.TMsgDlgBtn.mbOk], System.UITypes.TMsgDlgBtn.mbOk, 0, procedure(const AResult: TModalResult)  begin end);
end;

function TGameForm.HandleAppEvent(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
begin
  case AAppEvent of
    //TApplicationEvent.FinishedLaunching: Log('Finished Launching');
    TApplicationEvent.BecameActive:
     begin
       if (CurrentStage=GAMEPLAY) AND (GameLoop.Enabled=False) then
        begin
          StartGameLoop;
          PauseBitmapListAnimations(False);
        end;
     end;
    //TApplicationEvent.WillBecomeInactive: Log('Will Become Inactive');
    TApplicationEvent.EnteredBackground:
     begin
       if (CurrentStage=GAMEPLAY) AND (GameLoop.Enabled=True) then
        begin
          PauseBitmapListAnimations(True);
          StopGameLoop;
        end;
     end;
    //TApplicationEvent.WillBecomeForeground: Log('Will Become Foreground');
    //TApplicationEvent.WillTerminate: Log('Will Terminate');
    //TApplicationEvent.LowMemory: Log('Low Memory');
    //TApplicationEvent.TimeChange: Log('Time Change');
    //TApplicationEvent.OpenURL: Log('Open URL');
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
      MusicPlayer.Play;
      if MusicEnabled then
        begin
	  if MusicPlayer.Volume <> GetMaxVolume then
            MusicPlayer.Volume := GetMaxVolume;
        end
       else
        begin
	  if MusicPlayer.Volume<>0 then
            MusicPlayer.Volume := 0;
        end;
    end;
end;

procedure TGameForm.StopMusic;
begin
if MusicPlayer.FileName<>'' then
 begin
   if MusicPlayer.Volume<>0 then
     MusicPlayer.Volume := 0;
 end;
end;

procedure TGameForm.ShowHighScores;
begin
  HighScoresFrame.InitFrame;
  SetStage(HIGH_SCORES);
end;

procedure TGameForm.ShowSettings;
begin
  SettingsFrame.HostSwitch.IsChecked := HostEnabled;
  SettingsFrame.GyroSwitch.IsChecked := GyroEnabled;
  SettingsFrame.FullScreenSwitch.IsChecked := FullScreenEnabled;
  SettingsFrame.SoundSwitch.IsChecked := SoundEnabled;
  SettingsFrame.MusicSwitch.IsChecked := MusicEnabled;
  SetStage(SETTINGS);
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

procedure TGameForm.AddScore(I: Integer);
begin
  PlayerData.Score := PlayerData.Score + I;
  ScoreLBL.Text := IntToStr(PlayerData.Score);
end;

function TGameForm.GetPoolObj(Pool: TStringList): TRectangle;
begin
  if Pool.Count>0 then
   begin
    Result := TRectangle(Pool.Objects[Pool.Count-1]);
    Pool.Delete(Pool.Count-1);
   end
  else
   begin
    Result := TRectangle.Create(nil);
   end;
end;

procedure TGameForm.SetPoolObj(Pool: TStringList; Name: String; Obj: TRectangle);
begin
  Pool.AddObject(Name,Obj);
  Obj.Parent := nil;
end;

function TGameForm.SpawnProj(Source: TRectangle; X, Y: Single; Angle: Single): TRectangle;
var
R: TRectangle;
RAngle: Single;
begin
 R := GetPoolObj(ProjPool);
 R.Width := 5;
 R.Height := 5;
 R.RotationAngle := Angle-90;
 R.Stroke.Kind := TBrushKind.None;
 R.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
 R.Fill.Kind := TBrushKind.Bitmap;
 R.Fill.Bitmap.Bitmap.Assign(Projectile.Fill.Bitmap.Bitmap);
 RAngle := R.RotationAngle * PI / 180;
 // set the center X,Y and then travel 25 out from the center on the angle
 R.Position.X := X+(Source.Width/2)-(R.Width/2) + 25 * Cos(RAngle);
 R.Position.Y := Y+(Source.Height/2)-(R.Height/2) + 25 * Sin(RAngle);
 R.Tag := PROJ_SPEED;
 R.TagFloat := 0;
 R.Parent := Self;
 R.SendToBack;
 Result := R;
end;

function TGameForm.SpawnEnemy(X,Y: Single; Angle: Single; Speed: Integer): TRectangle;
var
R: TRectangle;
begin
 R := GetPoolObj(EnemyPool);
 R.Width := 50;
 R.Height := 50;
 R.RotationAngle := Angle;
 R.Position.X := X;
 R.Position.Y := Y;
 R.Tag := Speed;
 R.TagFloat := 0;

 R.Stroke.Kind := TBrushKind.None;
 R.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
 R.Fill.Kind := TBrushKind.Bitmap;
 R.Fill.Bitmap.Bitmap.Assign(Enemy.Fill.Bitmap.Bitmap);
 if Speed>0 then
  R.Fill.Bitmap.Bitmap.FlipHorizontal;

 R.Parent := Self;
 R.SendToBack;
 Result := R;
end;

function TGameForm.SpawnEnemyProj(Source: TRectangle; X, Y: Single; Angle: Single): TRectangle;
var
R: TRectangle;
RAngle: Single;
begin
 R := GetPoolObj(ProjPool);
 R.Width := 5;
 R.Height := 5;
 R.RotationAngle := Angle-90;
 R.Stroke.Kind := TBrushKind.None;
 R.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
 R.Fill.Kind := TBrushKind.Bitmap;
 R.Fill.Bitmap.Bitmap.Assign(EnemyProjectile.Fill.Bitmap.Bitmap);
 RAngle := R.RotationAngle * PI / 180;
 // set the center X,Y and then travel 25 out from the center on the angle
 R.Position.X := X+(Source.Width/2)-(R.Width/2) + 25 * Cos(RAngle);
 R.Position.Y := Y+(Source.Height/2)-(R.Height/2) + 25 * Sin(RAngle);
 R.Tag := ENEMYPROJ_SPEED;
 R.TagFloat := 0;
 R.Parent := Self;
 R.SendToBack;
 Result := R;
end;

function TGameForm.SpawnRock(X,Y: Single; Size: Integer): TRectangle;
var
R: TRectangle;
RockGraphic: TRectangle;
begin
 R := GetPoolObj(RockPool);
 R.Stroke.Kind := TBrushKind.None;
 R.Fill.Kind := TBrushKind.None;
 R.RotationAngle := Random(360);
 case Size of
  1: begin
      R.Width := 100;
      R.Height := 100;
      R.TagString := 'l';
      R.Tag := ROCK_SPEED;
     end;
  2: begin
      R.Width := 50;
      R.Height := 50;
      R.TagString := 'm';
      R.Tag := ROCK_SPEED+1;
     end;
  3: begin
      R.Width := 20;
      R.Height := 20;
      R.TagString := 's';
      R.Tag := ROCK_SPEED+2;
     end;
 end;
 R.Position.X := X;
 R.Position.Y := Y;

 if not Assigned(R.TagObject) then
  begin
   RockGraphic := TRectangle.Create(R);
   RockGraphic.Parent := R;
  end
 else
  begin
   RockGraphic := TRectangle(R.TagObject);
  end;

   RockGraphic.Width := R.Width;
   RockGraphic.Height := R.Height;
   RockGraphic.Position.X := (R.Width/2)-(RockGraphic.Width/2);
   RockGraphic.Position.Y := (R.Height/2)-(RockGraphic.Height/2);

   RockGraphic.Stroke.Kind := TBrushKind.None;
   RockGraphic.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
   RockGraphic.Fill.Kind := TBrushKind.Bitmap;
   case Random(3) of
    0: RockGraphic.Fill.Bitmap.Bitmap.Assign(Asteroid1.Fill.Bitmap.Bitmap);
    1: RockGraphic.Fill.Bitmap.Bitmap.Assign(Asteroid2.Fill.Bitmap.Bitmap);
    2: RockGraphic.Fill.Bitmap.Bitmap.Assign(Asteroid3.Fill.Bitmap.Bitmap);
   end;

 if not Assigned(R.TagObject) then
  begin
   R.TagObject := RockGraphic;
  end;

 R.TagFloat := 0;
 R.Parent := Self;
 R.SendToBack;
 Result := R;
end;

function TGameForm.SpawnCollectItem(X,Y: Single; Size: Integer): TRectangle;
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
   CollectGraphic.Position.X := (R.Width/2)-(CollectGraphic.Width/2);
   CollectGraphic.Position.Y := (R.Height/2)-(CollectGraphic.Height/2);
   CollectGraphic.RotationAngle := (360-R.RotationAngle);

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


function TGameForm.SpawnExplosion(X,Y: Single; Angle: Single): TRectangle;
var
R: TRectangle;
AE: TBitmapListAnimation;
begin
 R := GetPoolObj(ExplosionPool);
 R.Parent := Self;
 R.Width := 75;
 R.Height := 75;
 R.RotationAngle := Angle-90;
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
 R.Position.X := X-(R.Width/2);
 R.Position.Y := Y-(R.Height/2);
 R.Tag := 2;
 R.TagFloat := 0;
 R.Visible := True;
 R.SendToBack;
 Result := R;
end;

procedure TGameForm.CreateExplosion(X,Y:Single);
begin
  ExplosionList.AddObject('',SpawnExplosion(X,Y,0));
  PlaySound(EXPLOSION_SFX);
end;

procedure TGameForm.ShowHUD;
begin
  Ship.Visible := True;
  HUDLayout.Visible := True;
  {$IFDEF DEBUG}
  FPSLBL.Visible := True;
  {$ENDIF}
  HUDLayout.BringToFront;
end;

procedure TGameForm.HideHUD;
begin
  Ship.Visible := False;
  Thruster.Visible := False;
  HUDLayout.Visible := False;
  FPSLBL.Visible := False;
end;

procedure TGameForm.SetStage(Stage: Integer);
begin
  case Stage of
   MAIN_MENU: begin
      MainMenuFrame.Visible := True;
      GameOverFrame.Visible := False;
      InstructionsFrame.Visible := False;
      HighScoresFrame.Visible := False;
      LevelCompleteFrame.Visible := False;
      SettingsFrame.Visible := False;
      HideHUD;
      StopMusic;
   end;
   INSTRUCTIONS: begin
      MainMenuFrame.Visible := False;
      GameOverFrame.Visible := False;
      InstructionsFrame.Visible := True;
      HideHUD;
      StopMusic;
   end;
   GAMEPLAY: begin
      InstructionsFrame.Visible := False;
      LevelCompleteFrame.Visible := False;
      ShowHUD;
      PlayMusic;
   end;
   LEVEL_COMPLETE: begin
      LevelCompleteFrame.Visible := True;
      HideHUD;
   end;
   GAME_OVER: begin
      HighScoresFrame.Visible := False;
      GameOverFrame.Visible := True;
      HideHUD;
   end;
   HIGH_SCORES: begin
      MainMenuFrame.Visible := False;
      GameOverFrame.Visible := False;
      HighScoresFrame.Visible := True;
      HighScoresFrame.BringToFront;
      HideHUD;
   end;
  SETTINGS:
    begin
      MainMenuFrame.Visible := False;
      SettingsFrame.Visible := True;
      HideHUD;
      StopMusic;
    end;
  GAMEPAD:
    begin
      MainMenuFrame.Visible := False;
      HideHUD;
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


procedure TGameForm.SettingsFrameHostSwitchSwitch(Sender: TObject);
begin
  if HostEnabled then
  begin
    HostEnabled := False;
  end
  else
  begin
    HostEnabled := True;
  end;
end;

procedure TGameForm.SettingsFrameMainMenuBTNClick(Sender: TObject);
begin
  SaveAndExitSettings;
end;

function TGameForm.IntersectCircle(R1,R2: TRectangle): Boolean;
var
  Distance: Single;
begin
  Result := False;
  Distance := R1.Position.Point.Distance(R2.Position.Point);
  if Distance<((R1.Width/2) + (R2.Width/2)) then
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

  if (AccY > -4) then
  begin
    AccUp.Visible := True;
    AccUpButtonDown := True;
  end
  else
  begin
    AccUp.Visible := False;
    AccUpButtonDown := False;
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
I, II: Integer;
ProjAngle, RockAngle, EnemyAngle, EnemyProjAngle, CollectAngle: Single;
ProjObj: TRectangle;
RockObj: TRectangle;
ExplosionObj: TRectangle;
RockObjCenterX, RockObjCenterY: Single;
EnemyObj: TRectangle;
EnemyProjObj: TRectangle;
CollectObj: TRectangle;
Time: Cardinal;
begin
  {$IFDEF DEBUG}
  Time := TThread.GetTickCount;
  {$ENDIF}

  // Check for game over
  if PlayerData.Lives<=0 then
   begin
    LevelFail;
    GameOver;
    Exit;
   end;

  // Check for level complete
  if (RockList.Count=0) AND (EnemyList.Count=0) then
   begin
     LevelComplete;
     Exit;
   end;

   if CanSwapXY=True then
    begin
     SwapXY;
     CanSwapXY := False;
    end;

  // process accelerometer data
  if MotionSensor.Active = True then
  begin
    ProcessAccelerometer;
  end;

  // Handle player movement and firing
  PlayerData.FireInterval := PlayerData.FireInterval+1;

  if (LeftButtonDown OR AccLeftButtonDown) then
    Ship.RotationAngle := Ship.RotationAngle-PlayerData.RotationSpeed;
  if (RightButtonDown OR AccRightButtonDown) then
    Ship.RotationAngle := Ship.RotationAngle+PlayerData.RotationSpeed;
  if (ForwardButtonDown OR AccUpButtonDown) then
   begin
     PlayerData.SpeedX := PlayerData.SpeedX + (PlayerData.Thrust * Sin(Ship.RotationAngle * (PI / 180)));
     PlayerData.SpeedY := PlayerData.SpeedY + (PlayerData.Thrust * Cos(Ship.RotationAngle * (PI / 180)));
   end
  else
   begin
     PlayerData.SpeedX := PlayerData.SpeedX * PlayerData.Decay;
     PlayerData.SpeedY := PlayerData.SpeedY * PlayerData.Decay;
   end;
  if FireButtonDown then
   begin
      FirePlayerProj;
   end;
 if (ForwardButtonDown OR AccUpButtonDown) then
  begin
    Thruster.Visible := True;
  end
 else
  begin
    Thruster.Visible := False;
  end;

  PlayerData.CurrentSpeed := Sqrt((PlayerData.SpeedX * PlayerData.SpeedX) + (PlayerData.SpeedY * PlayerData.SpeedY));

  if (PlayerData.CurrentSpeed > PlayerData.MaxSpeed) then
  begin
    PlayerData.SpeedX := PlayerData.SpeedX * (PlayerData.MaxSpeed/PlayerData.CurrentSpeed);
    PlayerData.SpeedY := PlayerData.SpeedY * (PlayerData.MaxSpeed/PlayerData.CurrentSpeed);
  end;

  Ship.Position.X := Ship.Position.X + PlayerData.SpeedX;
  Ship.Position.Y := Ship.Position.Y - PlayerData.SpeedY;

  if (Ship.ParentedRect.CenterPoint.X>=(ScreenLayout.Width+(Ship.Width/2))) then
   begin
    Ship.Position.X := (ScreenLayout.Position.X+1)-(Ship.Width/2);
   end;

  if (Ship.ParentedRect.CenterPoint.Y>=(ScreenLayout.Height+(Ship.Height/2))) then
   begin
     Ship.Position.Y := (ScreenLayout.Position.Y+1)-(Ship.Height/2);
   end;

  if (Ship.ParentedRect.CenterPoint.X<=(ScreenLayout.Position.X-(Ship.Width/2))) then
   begin
    Ship.Position.X := (ScreenLayout.Width-1);
   end;

  if (Ship.ParentedRect.CenterPoint.Y<=(ScreenLayout.Position.Y-(Ship.Height/2))) then
   begin
    Ship.Position.Y := (ScreenLayout.Height-1);
   end;

  // Handle explosions
  if ExplosionList.Count>0 then
    for I := ExplosionList.Count-1 downto 0 do
      begin
        ExplosionObj := TRectangle(ExplosionList.Objects[I]);

        ExplosionObj.TagFloat := ExplosionObj.TagFloat+0.1;

        if (ExplosionObj.TagFloat>ExplosionObj.Tag) then
         begin
          //ProjList.Objects[I] := nil;
          SetPoolObj(ExplosionPool,ExplosionList[I],ExplosionObj);
          ExplosionList.Delete(I);
         end;
      end;

  // Handle player projectiles
  if ProjList.Count>0 then
    for I := ProjList.Count-1 downto 0 do
      begin
        ProjObj := TRectangle(ProjList.Objects[I]);
        ProjAngle := ProjObj.RotationAngle * PI / 180;
        ProjObj.Position.X := ProjObj.Position.X + ProjObj.Tag * Cos(ProjAngle);
        ProjObj.Position.Y := ProjObj.Position.Y + ProjObj.Tag * Sin(ProjAngle);

        if RockList.Count>0 then
        for II := 0 to RockList.Count-1 do
         begin
          RockObj := TRectangle(RockList.Objects[II]);
          if IntersectRect(RockObj.ParentedRect,ProjObj.ParentedRect) then
           begin
             RockObj.TagFloat := RockObj.TagFloat+1;
             ProjObj.TagFloat := PlayerData.ProjDuration+1;
             Break;
           end;
         end;

        if EnemyList.Count>0 then
        for II := 0 to EnemyList.Count-1 do
         begin
          EnemyObj := TRectangle(EnemyList.Objects[II]);
          if IntersectRect(EnemyObj.ParentedRect,ProjObj.ParentedRect) then
           begin
             EnemyObj.TagFloat := EnemyObj.TagFloat+1;
             ProjObj.TagFloat := PlayerData.ProjDuration+1;
             Break;
           end;
         end;

        if (ProjObj.ParentedRect.CenterPoint.X>=(ScreenLayout.Width+(ProjObj.Width/2))) then
         begin
          ProjObj.Position.X := (ScreenLayout.Position.X+1)-(ProjObj.Width/2);
         end;

        if (ProjObj.ParentedRect.CenterPoint.Y>=(ScreenLayout.Height+(ProjObj.Height/2))) then
         begin
           ProjObj.Position.Y := (ScreenLayout.Position.Y+1)-(ProjObj.Height/2);
         end;

        if (ProjObj.ParentedRect.CenterPoint.X<=(ScreenLayout.Position.X-(ProjObj.Width/2))) then
         begin
          ProjObj.Position.X := (ScreenLayout.Width-1);
         end;

        if (ProjObj.ParentedRect.CenterPoint.Y<=(ScreenLayout.Position.Y-(ProjObj.Height/2))) then
         begin
          ProjObj.Position.Y := (ScreenLayout.Height-1);
         end;

        ProjObj.TagFloat := ProjObj.TagFloat+0.1;

        if (ProjObj.TagFloat>PlayerData.ProjDuration) then
         begin
          SetPoolObj(ProjPool,ProjList[I],ProjObj);
          ProjList.Delete(I);
         end;
      end;

  // Handle rocks
  if RockList.Count>0 then
    for I := RockList.Count-1 downto 0 do
      begin
        RockObj := TRectangle(RockList.Objects[I]);

        if PlayerData.Invulnerable=0 then
          if IntersectRect(RockObj.ParentedRect,Ship.ParentedRect) then
            if IntersectCircle(RockObj,Ship) then
             begin
              PlayerHit;
              RockObj.TagFloat := RockObj.TagFloat+1;
             end;

          if RockObj.TagFloat>0 then
           begin
            RockObjCenterX := RockObj.Position.X+(RockObj.Width/2);
            RockObjCenterY := RockObj.Position.Y+(RockObj.Height/2);
            CreateExplosion(RockObjCenterX,RockObjCenterY);
            if RockObj.TagString='l' then
             begin
              AddScore(100);
              RockList.AddObject('',SpawnRock(RockObjCenterX,RockObjCenterY,2));
              RockList.AddObject('',SpawnRock(RockObjCenterX,RockObjCenterY,2));
             end
            else if RockObj.TagString='m' then
             begin
              AddScore(50);
              RockList.AddObject('',SpawnRock(RockObjCenterX,RockObjCenterY,3));
              RockList.AddObject('',SpawnRock(RockObjCenterX,RockObjCenterY,3));
             end
            else
             begin
              AddScore(20);
             end;
            SetPoolObj(RockPool,RockList[I],RockObj);
            RockList.Delete(I);
           end
          else
           begin
            TRectangle(RockObj.TagObject).RotationAngle := TRectangle(RockObj.TagObject).RotationAngle+1;

            RockAngle := RockObj.RotationAngle * PI / 180;
            RockObj.Position.X := RockObj.Position.X + RockObj.Tag * Cos(RockAngle);
            RockObj.Position.Y := RockObj.Position.Y + RockObj.Tag * Sin(RockAngle);
            if (RockObj.ParentedRect.CenterPoint.X>=(ScreenLayout.Width+(RockObj.Width/2))) then
             begin
              RockObj.Position.X := (ScreenLayout.Position.X+1)-(RockObj.Width/2);
             end;

            if (RockObj.ParentedRect.CenterPoint.Y>=(ScreenLayout.Height+(RockObj.Height/2))) then
             begin
               RockObj.Position.Y := (ScreenLayout.Position.Y+1)-(RockObj.Height/2);
             end;

            if (RockObj.ParentedRect.CenterPoint.X<=(ScreenLayout.Position.X-(RockObj.Width/2))) then
             begin
              RockObj.Position.X := (ScreenLayout.Width-1);
             end;

            if (RockObj.ParentedRect.CenterPoint.Y<=(ScreenLayout.Position.Y-(RockObj.Height/2))) then
             begin
              RockObj.Position.Y := (ScreenLayout.Height-1);
             end;
           end;
      end;

   // Handle enemy movement and firing
   if EnemyList.Count>0 then
    for I := EnemyList.Count-1 downto 0 do
      begin
        EnemyObj := TRectangle(EnemyList.Objects[I]);
        EnemyAngle := EnemyObj.RotationAngle * PI / 180;
        EnemyObj.Position.X := EnemyObj.Position.X + EnemyObj.Tag * Cos(EnemyAngle);
        EnemyObj.Position.Y := EnemyObj.Position.Y + EnemyObj.Tag * Sin(EnemyAngle);

        if PlayerData.Invulnerable=0 then
          if IntersectRect(Ship.ParentedRect,EnemyObj.ParentedRect) then
           begin
             PlayerHit;
             EnemyObj.TagFloat := EnemyObj.TagFloat+1;
           end;

          if Random(150)=1 then
           begin
            EnemyProjList.AddObject('',SpawnEnemyProj(EnemyObj,EnemyObj.Position.X,EnemyObj.Position.Y,RandomRange(-180,180)));
            PlaySound(ALIEN_SFX);
           end;


        if (EnemyObj.TagFloat>0) OR
        (EnemyObj.Position.X>ScreenLayout.Width) OR
        (EnemyObj.Position.Y>ScreenLayout.Height) OR
        (EnemyObj.Position.X<ScreenLayout.Position.X) OR
        (EnemyObj.Position.Y<ScreenLayout.Position.Y) then
         begin
          if EnemyObj.TagFloat>0 then
           begin
            CreateExplosion(EnemyObj.Position.X+(EnemyObj.Width/2),EnemyObj.Position.Y+(EnemyObj.Height/2));
            AddScore(500);
           end;
          SetPoolObj(EnemyPool,EnemyList[I],EnemyObj);
          EnemyList.Delete(I);
         end;
      end;

  // Handle enemy projectiles
  if EnemyProjList.Count>0 then
    for I := EnemyProjList.Count-1 downto 0 do
      begin
        EnemyProjObj := TRectangle(EnemyProjList.Objects[I]);
        EnemyProjAngle := EnemyProjObj.RotationAngle * PI / 180;
        EnemyProjObj.Position.X := EnemyProjObj.Position.X + EnemyProjObj.Tag * Cos(EnemyProjAngle);
        EnemyProjObj.Position.Y := EnemyProjObj.Position.Y + EnemyProjObj.Tag * Sin(EnemyProjAngle);

        if PlayerData.Invulnerable=0 then
          if IntersectRect(Ship.ParentedRect,EnemyProjObj.ParentedRect) then
           begin
             PlayerHit;
             EnemyProjObj.TagFloat := EnemyProjObj.TagFloat+1;
           end;

        if (EnemyProjObj.TagFloat>0) OR
        (EnemyProjObj.Position.X>ScreenLayout.Width) OR
        (EnemyProjObj.Position.Y>ScreenLayout.Height) OR
        (EnemyProjObj.Position.X<ScreenLayout.Position.X) OR
        (EnemyProjObj.Position.Y<ScreenLayout.Position.Y) then
         begin
          SetPoolObj(EnemyProjPool,EnemyProjList[I],EnemyProjObj);
          EnemyProjList.Delete(I);
         end;
      end;

    // Spawn enemies
    if EnemyList.Count<2 then
     begin
      if Random(100)=1 then
       begin
        if Random(2)=1 then
         begin
          EnemyList.AddObject('',SpawnEnemy(ScreenLayout.Width-1,RandomRange(1,Trunc(ScreenLayout.Height)),0,ENEMY_SPEED*-1));
         end
        else
         begin
          EnemyList.AddObject('',SpawnEnemy(ScreenLayout.Position.X+1,RandomRange(1,Trunc(ScreenLayout.Height)),0,ENEMY_SPEED));
         end;
        PlaySound(ALIEN_SFX);
       end;
     end;

  // Handle collectable items
  if CollectList.Count>0 then
    for I := CollectList.Count-1 downto 0 do
      begin
        CollectObj := TRectangle(CollectList.Objects[I]);
        CollectAngle := CollectObj.RotationAngle * PI / 180;
        CollectObj.Position.X := CollectObj.Position.X + CollectObj.Tag * Cos(CollectAngle);
        CollectObj.Position.Y := CollectObj.Position.Y + CollectObj.Tag * Sin(CollectAngle);

        if (CollectObj.ParentedRect.CenterPoint.X>=(ScreenLayout.Width+(CollectObj.Width/2))) then
         begin
          CollectObj.Position.X := (ScreenLayout.Position.X+1)-(CollectObj.Width/2);
         end;

        if (CollectObj.ParentedRect.CenterPoint.Y>=(ScreenLayout.Height+(CollectObj.Height/2))) then
         begin
           CollectObj.Position.Y := (ScreenLayout.Position.Y+1)-(CollectObj.Height/2);
         end;

        if (CollectObj.ParentedRect.CenterPoint.X<=(ScreenLayout.Position.X-(CollectObj.Width/2))) then
         begin
          CollectObj.Position.X := (ScreenLayout.Width-1);
         end;

        if (CollectObj.ParentedRect.CenterPoint.Y<=(ScreenLayout.Position.Y-(CollectObj.Height/2))) then
         begin
          CollectObj.Position.Y := (ScreenLayout.Height-1);
         end;

        if IntersectRect(Ship.ParentedRect,CollectObj.ParentedRect) then
         begin
           AddScore(5000);
           CollectObj.TagFloat := COLLECTITEM_DURATION+1;
           PlaySound(COLLECT_SFX);
         end;

        CollectObj.TagFloat := CollectObj.TagFloat+0.1;

        if (CollectObj.TagFloat>COLLECTITEM_DURATION) then
         begin
          SetPoolObj(CollectPool,CollectList[I],CollectObj);
          CollectList.Delete(I);
         end;
      end;

    // Spawn collectable
    if CollectList.Count<1 then
     begin
      if Random(100)=1 then
       begin
         CollectList.AddObject('',SpawnCollectItem(RandomRange(1,Trunc(ScreenLayout.Width-1)),RandomRange(1,Trunc(ScreenLayout.Height-1)),0));
       end;
     end;


    // Handle music loop
    if (MusicPlayer.CurrentTime >= MusicPlayer.Duration) OR (MusicPlayer.State = TMediaState.Stopped) then
     begin
        MusicPlayer.CurrentTime := 0;
        //MusicPlayer.Stop;
        PlayMusic;
     end;

  {$IFDEF DEBUG}
  FPSLBL.Text := IntToStr(TThread.GetTickCount-Time) + ' ms';
  {$ENDIF}
end;

function TGameForm.GetTargetAngle(TargetX, TargetY, OriginX, OriginY: Single): Single;
var
  Radians: Single;
begin
  Radians  := ArcTan2(TargetY - OriginY, TargetX - OriginX);
  Result := radians / (PI / 180) + 90;
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
      AccUp.Visible := False;
      AccUpButtonDown := False;
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
  PlayerData.Health := PlayerData.Health-1;
  DisplayHealth(PlayerData.Health);
  CreateExplosion(Ship.Position.X+(Ship.Width/2),Ship.Position.Y+(Ship.Height/2));
  if PlayerData.Health<=0 then
   begin
    CreateExplosion(Ship.Position.X+RandomRange(1,Trunc(Ship.Width)),Ship.Position.Y+RandomRange(1,Trunc(Ship.Height)));
    PlayerData.Lives := PlayerData.Lives-1;
    PlayerData.Health := PLAYER_HEALTH;
    Health1.Visible := True;
    Health2.Visible := True;
    Health3.Visible := True;
    PlayerData.SpeedX := 0;
    PlayerData.SpeedY := 0;
    PlayerData.CurrentSpeed := 0;
    DisplayLives(PlayerData.Lives);
    if PlayerData.Lives>0 then
     begin
      PlayerData.Invulnerable := PlayerData.InvulnerableInterval;
      Ship.Opacity := Ship.Opacity-0.25;
      InvulnerableTimer.Enabled := True;
      CenterPlayer;
      PlaySound(FAIL_SFX);
     end;
   end;
end;

procedure TGameForm.DisplayLives(Lives: Integer);
begin
 if Lives>0 then
  Lives1.Visible := True
 else
  Lives1.Visible := False;
 if Lives>1 then
  Lives2.Visible := True
 else
  Lives2.Visible := False;
 if Lives>2 then
  Lives3.Visible := True
 else
  Lives3.Visible := False;
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

  DelayedSettings.Enabled := False;
end;

procedure TGameForm.DisplayHealth(Health: Integer);
begin
 if Health>0 then
  Health1.Visible := True
 else
  Health1.Visible := False;
 if Health>1 then
  Health2.Visible := True
 else
  Health2.Visible := False;
 if Health>2 then
  Health3.Visible := True
 else
  Health3.Visible := False;
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
  SetStage(MAIN_MENU);
end;

procedure TGameForm.CloseHighScores;
begin
 if LastStage=MAIN_MENU then
  SetStage(MAIN_MENU)
 else
  SetStage(GAME_OVER);
end;

procedure TGameForm.ShowGamePad;
begin
  ShowMsgBox(NO_GAMEPAD);
end;

procedure TGameForm.HighScoresFrameContinueBTNClick(Sender: TObject);
begin
 CloseHighScores;
end;

procedure TGameForm.FireBTNClick(Sender: TObject);
begin
 FirePlayerProj;
end;

procedure TGameForm.FireBTNMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  FireDownEvent(Sender);
end;

procedure TGameForm.FireBTNMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  FireUpEvent(Sender);
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
  GyroEnabled := IniFile.ReadBool('Settings', 'GyroEnabled', False);
  if GyroEnabled = False then
    GyroOffLine.Visible := True;
  HostEnabled := IniFile.ReadBool('Settings', 'HostEnabled', False);
  FullScreenEnabled := IniFile.ReadBool('Settings', 'FullScreenEnabled', False);
  if FullScreenEnabled = True then
    GameForm.FullScreen := True;
  IniFile.Free;

  DelayedSettings.Enabled := True;
end;

procedure TGameForm.ExitDialog(Sender: TObject);
begin
  TDialogServiceAsync.MessageDialog('Exit the game?', System.UITypes.TMsgDlgType.mtInformation,
    [
      System.UITypes.TMsgDlgBtn.mbYes, System.UITypes.TMsgDlgBtn.mbNo
    ], System.UITypes.TMsgDlgBtn.mbNo, 0,
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
  ScreenSvc: IFMXScreenService;
  Size: TPointF;
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

  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, IInterface(ScreenSvc)) then
  begin
    Size := ScreenSvc.GetScreenSize;
  end;
  if Round(Size.X) < Round(GameForm.Fill.Bitmap.Bitmap.Height) then
    GameForm.Fill.Bitmap.Bitmap.Resize(Round(Size.X*GameForm.Fill.Bitmap.Bitmap.Width/GameForm.Fill.Bitmap.Bitmap.Height), Round(Size.X));

  GameForm.Fill.Bitmap.WrapMode := TWrapMode.Tile;

  ScreenOrientation := GetScreenOrientation;
  if (ScreenOrientation=TScreenOrientation.Portrait) OR (ScreenOrientation=TScreenOrientation.InvertedPortrait) then
   begin
    GameForm.Fill.Bitmap.Bitmap.Rotate(-90);
   end;
  OrientationChangedId := TMessageManager.DefaultManager.SubscribeToMessage(TOrientationChangedMessage, OrientationChanged);

  if TPlatformServices.Current.SupportsPlatformService(IFMXApplicationEventService, IInterface(FMXApplicationEventService)) then
    FMXApplicationEventService.SetApplicationEventHandler(HandleAppEvent);

  LoadSettings;

  AudioManager := TAudioManager.Create;
  RegisterSound(DataFilePath + 'fire.wav');
  RegisterSound(DataFilePath + 'explosion.wav');
  RegisterSound(DataFilePath + 'fail.wav');
  RegisterSound(DataFilePath + 'win.wav');
  RegisterSound(DataFilePath + 'alien.wav');
  RegisterSound(DataFilePath + 'collect.wav');

  SetStage(MAIN_MENU);
end;

procedure TGameForm.FormDestroy(Sender: TObject);
begin
 SaveSettings;
 TMessageManager.DefaultManager.Unsubscribe(TOrientationChangedMessage, OrientationChangedId);
end;

procedure TGameForm.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  case CurrentStage of
    GAMEPLAY:
      begin
        case KeyChar of
          'Z','z',' ':
            begin
              FireDownEvent(Sender);
            end;
          'W','w':
            begin
              UpDownEvent(Sender);
            end;
          'A','a':
            begin
              LeftDownEvent(Sender);
            end;
          'D','d':
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
      //
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
        'Z','z',' ':
          begin
            FireUpEvent(Sender);
          end;
        'W','w':
          begin
            UpUpEvent(Sender);
          end;
        'A','a':
          begin
            LeftUpEvent(Sender);
          end;
        'D','d':
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
      //
  end;
end;

procedure TGameForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  ClearButtons;
end;

procedure TGameForm.ClearButtons;
begin
  case CurrentStage of
   GAMEPAD:
    begin
      //
    end;
   else
    begin
     LeftButtonDown := False;
     RightButtonDown := False;
     ForwardButtonDown := False;
     FireButtonDown := False;
    end;
  end;
end;

procedure TGameForm.ForwardBTNMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  UpDownEvent(Sender);
end;

procedure TGameForm.ForwardBTNMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  UpUpEvent(Sender);
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

procedure TGameForm.InvulnerableTimerTimer(Sender: TObject);
begin
PlayerData.Invulnerable := PlayerData.Invulnerable-1;
if PlayerData.Invulnerable<=0 then
  begin
    InvulnerableTimer.Enabled := False;
    Ship.Opacity := Ship.Opacity+0.25;
  end;
end;

procedure TGameform.FireDownEvent(Sender: TObject);
begin
  FireButtonDown := True;
end;

procedure TGameform.FireUpEvent(Sender: TObject);
begin
  FireButtonDown := False;
end;

procedure TGameform.LeftDownEvent(Sender: TObject);
begin
  RightButtonDown := False;
  LeftButtonDown := True;
end;

procedure TGameform.LeftUpEvent(Sender: TObject);
begin
  LeftButtonDown := False;
end;

procedure TGameform.RightDownEvent(Sender: TObject);
begin
  LeftButtonDown := False;
  RightButtonDown := True;
end;

procedure TGameform.RightUpEvent(Sender: TObject);
begin
  RightButtonDown := False;
end;

procedure TGameform.UpDownEvent(Sender: TObject);
begin
  ForwardButtonDown := True;
end;

procedure TGameform.UpUpEvent(Sender: TObject);
begin
  ForwardButtonDown := False;
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
          InitAndPlayGame
              end;
            GAMEPLAY:
              begin
          FireDownEvent(Sender);
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

procedure TGameForm.FireActionUpExecute(Sender: TObject);
begin
  // called from RunRemoteActionAsync. Requires TThread.Synchronize for UI changes.
  FireUpEvent(Sender);
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

procedure TGameForm.FirePlayerProj;
begin
   if PlayerData.FireInterval>PlayerData.FireSpeed then
    begin
      ProjList.AddObject('',SpawnProj(Ship,Ship.Position.X,Ship.Position.Y,Ship.RotationAngle));
      PlayerData.FireInterval := 0;
      PlaySound(FIRE_SFX);
    end;
end;

procedure TGameForm.StartGame;
begin
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
    AccUp.Visible := False;
    AccUpButtonDown := False;
    AccLeft.Visible := False;
    AccLeftButtonDown := False;
    AccRight.Visible := False;
    AccRightButtonDown := False;
  end;   
 if PlayerData.Invulnerable>0 then
   InvulnerableTimer.Enabled := True;
 GameLoop.Enabled := True;
end;

procedure TGameForm.StopGameLoop;
begin
 if GyroEnabled then
  MotionSensor.Active := False;
 InvulnerableTimer.Enabled := False;
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
 LevelCompleteFrame.InfoText.Text := 'Score: ' + IntToStr(PlayerData.Score) + #13#10;
 SetStage(LEVEL_COMPLETE);
end;

procedure TGameForm.LevelCompleteFrameContinueBTNClick(Sender: TObject);
begin
ContinueGame;
end;

procedure TGameForm.InitGame;
begin
 RockList := TStringList.Create;
 RockPool := TStringList.Create;
 ProjList := TStringList.Create;
 ProjPool := TStringList.Create;
 ExplosionList := TStringList.Create;
 ExplosionPool := TStringList.Create;
 EnemyList := TStringList.Create;
 EnemyPool := TStringList.Create;
 EnemyProjList := TStringList.Create;
 EnemyProjPool := TStringList.Create;
 CollectList := TStringList.Create;
 CollectPool := TStringList.Create;
 
 CleanedUp := False;

 Ship.RotationAngle := 0;
 CenterPlayer;
end;

procedure TGameForm.InitPlayer;
begin
 PlayerData := TPlayerData.Create(Self);
 PlayerData.Health := PLAYER_HEALTH;
 PlayerData.Lives := PLAYER_LIVES;
 PlayerData.SpeedX := 0;
 PlayerData.SpeedY := 0;
 PlayerData.Level := 1;
 PlayerData.FireSpeed := 5;
 PlayerData.FireInterval := PlayerData.FireSpeed;
 PlayerData.CurrentSpeed := 0;
 PlayerData.MaxSpeed := 10;
 PlayerData.Decay := 0.97;
 PlayerData.Thrust := 0.5;
 PlayerData.RotationSpeed := 10;
 PlayerData.InvulnerableInterval := 5;
 PlayerData.ProjDuration := ((Max(ScreenLayout.Width,ScreenLayout.Height)/2) / PROJ_SPEED)/10;

 if LivesSet=False then
  begin
   BitmapToRectangle(Ship.Fill.Bitmap.Bitmap,Lives1);
   BitmapToRectangle(Ship.Fill.Bitmap.Bitmap,Lives2);
   BitmapToRectangle(Ship.Fill.Bitmap.Bitmap,Lives3);

   BitmapToRectangle(HealthBar.Fill.Bitmap.Bitmap,Health1);
   BitmapToRectangle(HealthBar.Fill.Bitmap.Bitmap,Health2);
   BitmapToRectangle(HealthBar.Fill.Bitmap.Bitmap,Health3);
  end;

 DisplayLives(PlayerData.Lives);
 DisplayHealth(PlayerData.Health);
 DisplayScore;

 CenterPlayer;

 Ship.TagObject := PlayerData;

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
 Ship.Position.X := (ScreenLayout.Width/2)-(Ship.Width/2);
 Ship.Position.Y := (ScreenLayout.Height/2)-(Ship.Height/2);
end;

procedure TGameForm.CleanupGame(Continue: Boolean);
begin
 if CleanedUp=False then
  begin
    RockList.OwnsObjects := True;
    RockPool.OwnsObjects := True;
    ProjList.OwnsObjects := True;
    ProjPool.OwnsObjects := True;
    ExplosionList.OwnsObjects := True;
    ExplosionPool.OwnsObjects := True;
    EnemyList.OwnsObjects := True;
    EnemyPool.OwnsObjects := True;
    EnemyProjList.OwnsObjects := True;
    EnemyProjPool.OwnsObjects := True;
    CollectList.OwnsObjects := True;
    CollectPool.OwnsObjects := True;

    if (Continue = False) then
      FreeAndNil(PlayerData);
    FreeAndNil(ProjList);
    FreeAndNil(ProjPool);
    FreeAndNil(ExplosionList);
    FreeAndNil(ExplosionPool);
    FreeAndNil(RockList);
    FreeAndNil(RockPool);
    FreeAndNil(EnemyList);
    FreeAndNil(EnemyPool);
    FreeAndNil(EnemyProjList);
    FreeAndNil(EnemyProjPool);
    FreeAndNil(CollectList);
    FreeAndNil(CollectPool);

   CleanedUp := True;
  end;
end;

procedure TGameForm.PlayGame;
var
I: Integer;
begin
 SetStage(GAMEPLAY);
 for I := 1 to PlayerData.Level+2 do
   begin
     case RandomRange(1,4) of
      1: RockList.AddObject('',SpawnRock(Random(Trunc(ScreenLayout.Width)),0,1));
      2: RockList.AddObject('',SpawnRock(0,Random(Trunc(ScreenLayout.Height)),1));
      3: RockList.AddObject('',SpawnRock(Random(Trunc(ScreenLayout.Width)),ScreenLayout.Height,1));
      4: RockList.AddObject('',SpawnRock(ScreenLayout.Width,Random(Trunc(ScreenLayout.Height)),1));
     end;
   end;
 StartGameLoop;
end;

procedure TGameForm.ContinueGame;
begin
 CleanupGame(True);
 InitGame;
 PlayerData.SpeedX := 0;
 PlayerData.SpeedY := 0;
 PlayerData.CurrentSpeed := 0;
 PlayerData.Level := PlayerData.Level+1;
 PlayGame;
end;

procedure TGameForm.LevelFail;
begin
 StopGameLoop;
 PlayerData.SpeedX := 0;
 PlayerData.SpeedY := 0;
 PlayerData.CurrentSpeed := 0;
 if PlayerData.Invulnerable>0 then
  begin
    Ship.Opacity := Ship.Opacity+0.25;
    PlayerData.Invulnerable := 0;
  end;
 PlaySound(FAIL_SFX);
end;

procedure TGameForm.GameOver;
begin
 SetStage(GAME_OVER);
 Application.ProcessMessages;
 HighScoresFrame.InitFrame;
 HighScoresFrame.AddScore('',PlayerData.Score);
 SetStage(HIGH_SCORES);
end;

procedure TGameForm.SwapListXY(List: TStringList);
var
I: Integer;
TempX, TempY: Single;
begin
 if List.Count>0 then
   for I := List.Count-1 downto 0 do
    begin
     TempX := TRectangle(List.Objects[I]).Position.X;
     TempY := TRectangle(List.Objects[I]).Position.Y;
     TRectangle(List.Objects[I]).Position.X := TempY;
     TRectangle(List.Objects[I]).Position.Y := TempX;
    end;
end;

procedure TGameForm.SwapXY;
var
TempX, TempY: Single;
begin
 Ship.Anchors := [];
 TempX := Ship.Position.X;
 TempY := Ship.Position.Y;
 Ship.Position.X := TempY;
 Ship.Position.Y := TempX;
 Ship.Align := TAlignLayout.None;
 Ship.Anchors := [TAnchorKind.akLeft,TAnchorKind.akTop];

 SwapListXY(ProjList);
 SwapListXY(RockList);
 SwapListXY(EnemyList);
 SwapListXY(EnemyProjList);
 SwapListXY(ExplosionList);
 SwapListXY(CollectList);
end;

procedure TGameForm.OrientationChanged(const Sender: TObject; const Msg: TMessage);
var
 NewScreenOrientation: TScreenOrientation;
begin
 if CurrentStage = GAMEPLAY then
  begin
   NewScreenOrientation := GetScreenOrientation;

   if ((NewScreenOrientation=TScreenOrientation.Portrait) OR (NewScreenOrientation=TScreenOrientation.InvertedPortrait)) AND ((ScreenOrientation=TScreenOrientation.LandScape) OR (ScreenOrientation=TScreenOrientation.InvertedLandScape)) then
    begin
     CanSwapXY := True;
     GameForm.Fill.Bitmap.Bitmap.Rotate(90);
    end
   else
   if ((NewScreenOrientation=TScreenOrientation.LandScape) OR (NewScreenOrientation=TScreenOrientation.InvertedLandScape)) AND ((ScreenOrientation=TScreenOrientation.Portrait) OR (ScreenOrientation=TScreenOrientation.InvertedPortrait)) then
    begin
     CanSwapXY := True;
     GameForm.Fill.Bitmap.Bitmap.Rotate(-90);
    end;
  end;
end;

function TGameForm.GetScreenOrientation: TScreenOrientation;
begin
  Result := IFMXScreenService(TPlatformServices.Current.GetPlatformService(IFMXScreenService)).GetScreenOrientation;
end;

procedure RenderingSetupCallback(const Sender, Context: TObject; var ColorBits, DepthBits: Integer;
var Stencil: Boolean; var Multisamples: Integer);
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
    if TPlatformServices.Current.SupportsPlatformService(IFMXRenderingSetupService, IInterface(SetupService)) then
    SetupService.Subscribe(RenderingSetupCallback);
    // There is also SetupService.Unsubscribe, which removes the hook.
end;

initialization

//enable the GPU on Windows
//FMX.Types.GlobalUseGPUCanvas := True;
RegisterRenderingSetup;
Randomize;

end.
