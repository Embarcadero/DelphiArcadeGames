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
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.IniFiles,
  System.Messaging, System.IOUtils, FMX.Types, FMX.Controls, FMX.Forms,
  FMX.Graphics, FMX.Dialogs, FMX.Objects, FMX.Layouts, System.Math, FMX.StdCtrls,
  FMX.Ani, FMX.Media, FMX.Platform, FMX.Filter.Effects, FMX.Effects, AudioManager,
  uMainMenu, uInstructions, uLevelComplete, uGameOver, uHighScores, FMX.Edit,
  System.Sensors, System.Sensors.Components, System.Actions, FMX.ActnList, uSettings,
  AnonThread, FMX.Controls.Presentation, FMX.DialogService.Async;

type
  TPlayerData = class(TFMXObject)
  public
    SpeedX: Single;
    SpeedY: Single;
    MaxSpeedX: Single;
    MaxSpeedY: Single;
    MaxReverseSpeedX: Single;
    MaxReverseSpeedY: Single;
    AccelerationX: Single;
    AccelerationY: Single;
    EnemiesDestroyed: Integer;
    EnemiesSpawned: Integer;
    CanWarp: Boolean;

    Lives: Integer;
    Health: Integer;
    Bombs: Integer;
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
    HighScoresFrame: TFrameHighScores;
    AssetLayout: TLayout;
    StyleBook: TStyleBook;
    Projectile1: TRectangle;
    Explosion: TRectangle;
    AnimateExplosion: TBitmapListAnimation;
    EnemyProjectile: TRectangle;
    CollectItem: TRectangle;
    LeftBTN: TRectangle;
    UpBTN: TRectangle;
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
    Thruster: TRectangle;
    DownBTN: TRectangle;
    MapLayout1: TLayout;
    Rectangle1: TRectangle;
    MapLayout2: TLayout;
    Rectangle2: TRectangle;
    MapLayout3: TLayout;
    Rectangle3: TRectangle;
    BombBTN: TRectangle;
    WarpBTN: TRectangle;
    Person: TRectangle;
    MiniMapGrid: TGridPanelLayout;
    Enemy1: TRectangle;
    AnimateEnemy1: TBitmapListAnimation;
    Enemy2: TRectangle;
    AnimateEnemy2: TBitmapListAnimation;
    Enemy3: TRectangle;
    AnimateEnemy3: TBitmapListAnimation;
    GyroBTN: TRectangle;
    GyroOffLine: TLine;
    MotionSensor: TMotionSensor;
    AccRight: TCircle;
    AccLeft: TCircle;
    AccUp: TCircle;
    AccDown: TCircle;
    ActionList: TActionList;
    LeftActionDown: TAction;
    RightActionDown: TAction;
    UpActionDown: TAction;
    DownActionDown: TAction;
    LeftActionUp: TAction;
    RightActionUp: TAction;
    UpActionUp: TAction;
    DownActionUp: TAction;
    FireActionDown: TAction;
    FireActionUp: TAction;
    WarpActionClick: TAction;
    BombActionClick: TAction;
    Bomb: TRectangle;
    Bombs1: TRectangle;
    Bombs2: TRectangle;
    Bombs3: TRectangle;
    SettingsFrame: TFrameSettings;
    MountainsRect: TRectangle;
    MiniMapScaledLayout: TLayout;
    MiniMapShadow: TRectangle;
    ShipMini: TRectangle;
    MiniMountains: TRectangle;
    PlasmaFence: TRectangle;
    Portal: TRectangle;
    Projectile2: TRectangle;
    Projectile3: TRectangle;
    Projectile4: TRectangle;
    Projectile5: TRectangle;
    Projectile6: TRectangle;
    MainMenuFrame: TFrameMainMenu;
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
    procedure UpBTNMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure UpBTNMouseUp(Sender: TObject; Button: TMouseButton;
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
    procedure DownBTNMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure DownBTNMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure BombBTNMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure WarpBTNClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure GyroBTNClick(Sender: TObject);
    procedure LeftActionDownExecute(Sender: TObject);
    procedure LeftActionUpExecute(Sender: TObject);
    procedure RightActionDownExecute(Sender: TObject);
    procedure RightActionUpExecute(Sender: TObject);
    procedure UpActionDownExecute(Sender: TObject);
    procedure UpActionUpExecute(Sender: TObject);
    procedure DownActionDownExecute(Sender: TObject);
    procedure DownActionUpExecute(Sender: TObject);
    procedure SettingsFrameMainMenuBTNClick(Sender: TObject);
    procedure FireActionDownExecute(Sender: TObject);
    procedure FireActionUpExecute(Sender: TObject);
    procedure BombActionClickExecute(Sender: TObject);
    procedure WarpActionClickExecute(Sender: TObject);
    procedure MainMenuFrameSettingsBTNClick(Sender: TObject);
    procedure SettingsFrameHostSwitchSwitch(Sender: TObject);
    procedure SettingsFrameFullScreenSwitchSwitch(Sender: TObject);
    procedure MainMenuFrameGamepadBTNClick(Sender: TObject);
    procedure GamepadFrameCloseBTNClick(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure HUDLayoutMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure DelayedSettingsTimer(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
    LeftButtonDown: Boolean;
    RightButtonDown: Boolean;
    UpButtonDown: Boolean;
    DownButtonDown: Boolean;
    FireButtonDown: Boolean;
    AccLeftButtonDown: Boolean;
    AccRightButtonDown: Boolean;
    AccUpButtonDown: Boolean;
    AccDownButtonDown: Boolean;
    CanLaunchBomb: Boolean;
    ProjList: TStringList;
    ExplosionList: TStringList;
    EnemyList: TStringList;
    EnemyListForMinimap: TStringList;
    EnemyProjList: TStringList;
    CollectList: TStringList;
    CollectPool: TStringList;
    PeopleList: TStringList;
    PeopleListForMinimap: TStringList;
    PeoplePool: TStringList;
    PeoplePoolForMinimap: TStringList;
    PlayerData: TPlayerData;
    ProjPool: TStringList;
    ExplosionPool: TStringList;
    EnemyPool: TStringList;
    EnemyPoolForMinimap: TStringList;
    EnemyProjPool: TStringList;
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
    CanResetLayout: Boolean;
    CleanedUp: Boolean;

    Map1Width, Map1Height: Single;
    Map2Width, Map2Height: Single;
    Map3Width, Map3Height: Single;

    PlasmaFence1, PlasmaFence2: TRectangle;
    Portal1, Portal2: TRectangle;

    ScreenOrientation: TScreenOrientation;
    OrientationChangedId: Integer;

    NetworkConnected: Boolean;
    NetworkChecking: Boolean;

    procedure ResetPeople;
    procedure ResetPlasmaFence;
    procedure ResetLayout;
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
    procedure DownDownEvent(Sender: TObject);
    procedure DownUpEvent(Sender: TObject);
    procedure LeftDownEvent(Sender: TObject);
    procedure LeftUpEvent(Sender: TObject);
    procedure RightDownEvent(Sender: TObject);
    procedure RightUpEvent(Sender: TObject);
    procedure UpDownEvent(Sender: TObject);
    procedure UpUpEvent(Sender: TObject);
    procedure LaunchPlayerBomb;
    procedure CenterPlayer;
    procedure CenterPlayerScreen;
    procedure AddScore(I: Integer);
    procedure DisplayLives(Lives: Integer);
    procedure DisplayHealth(Health: Integer);
    procedure DisplayScore;
    procedure DisplayBombs(Bombs: Integer);
    procedure PlayerHit;
    function IsAttackDistance(R1, R2: TRectangle; Range: Single): Boolean;
    function SpawnProj(Source: TRectangle; X, Y: Single; Angle: Single)
      : TRectangle;

    function SpawnCollectItem(X, Y: Single; Size: Integer): TRectangle;
    function SpawnPerson(X, Y: Single; Angle: Single; Speed: Integer)
      : TRectangle;
    function SpawnPersonOnMinimap(Rect: TRectangle): TRectangle;
    function SpawnEnemy(X, Y: Single; Angle: Single; Speed: Integer)
      : TRectangle;
    function SpawnEnemyOnMinimap(Rect: TRectangle): TRectangle;
    procedure UpgradeEnemy(R: TRectangle; Speed: Integer);
    function SpawnEnemyProj(Source: TRectangle; X, Y: Single; Angle: Single)
      : TRectangle;
    function SpawnExplosion(X, Y: Single; Angle: Single): TRectangle;
    procedure CreateExplosion(X, Y: Single);
    procedure CreateEnemies;
    procedure CreateExplosions;
    procedure CreateEnemyProj;
    procedure CreateFencesAndPortals;
    procedure CreatePeople;

    procedure BitmapToRectangle(B: TBitmap; R: TRectangle);
    function IntersectCircle(R1, R2: TRectangle): Boolean;
    function GetTargetAngle(TargetX, TargetY, OriginX, OriginY: Single): Single;
    function GetPoolObj(Pool: TStringList): TShape;
    procedure SetPoolObj(Pool: TStringList; Name: String; Obj: TShape);
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
    function EnemySearchAndTargetPeople(EnemyObj: TRectangle): Single;
    function EnemyGetPlayerAngle(EnemyObj: TRectangle): Single;
    procedure WarpToObject(R: TRectangle);
    procedure PlayerWarp;
    procedure ProcessAccelerometer;
  public
    { Public declarations }
    SettingsFilePath: String;
    function HandleAppEvent(AAppEvent: TApplicationEvent;
      AContext: TObject): Boolean;
    procedure ShowMsgBox(S: String);
    procedure ExitDialog(Sender: TObject);
  end;

const
  // Player
  PLAYER_HEALTH = 3;
  PLAYER_LIVES = 3;
  PLAYER_BOMBS = 3;
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
  WARP_SFX = 6;
  // Music
  MUSIC_FILENAME = 'music.mp3';
  // Movement Speeds
  PROJ_SPEED = 36;
  PLAYER_SPEED = 10;
  ENEMYPROJ_SPEED = 10;
  ENEMY_SPEED = 3;
  COLLECTITEM_SPEED = 1;
  COLLECTITEM_DURATION = 6;
  // Gameplay
  PEOPLE_COUNT = 15;
  ENEMY_COUNT = 20;
  // People States
  PEOPLE_STATE_NONE = '';
  PEOPLE_STATE_CAPTURED = 'captured';
  // Enemy AI States
  ENEMY_AI_NONE = '';
  ENEMY_AI_RUN = 'run';
  ENEMY_AI_RUNNING = 'running';
  ENEMY_AI_TRAVEL = 'travel';
  ENEMY_AI_SEARCH = 'search';
  ENEMY_AI_TARGET = 'target';
  ENEMY_AI_UPGRADE = 'upgrade';
  ENEMY_AI_ATTACK = 'attack';
  // Enemy Levels
  ENEMY_LEVEL1 = 'level1';
  ENEMY_LEVEL2 = 'level2';
  ENEMY_LEVEL3 = 'level3';
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
 EnemyObj: TRectangle;
 BLAObj: TRectangle;
begin
    for I := 0 to EnemyList.Count - 1 do
    begin
      EnemyObj := TRectangle(EnemyList.Objects[I]);
      TBitmapListAnimation(EnemyObj.TagObject).Pause := Value;
    end;
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

procedure TGameForm.AddScore(I: Integer);
begin
  PlayerData.Score := PlayerData.Score + I;
  ScoreLBL.Text := IntToStr(PlayerData.Score);
end;

function TGameForm.GetPoolObj(Pool: TStringList): TShape;
begin
  if Pool.Count > 0 then
  begin
    Result := TRectangle(Pool.Objects[Pool.Count - 1]);
    Pool.Delete(Pool.Count - 1);
  end
  else
  begin
    Result := TRectangle.Create(nil);
  end;
end;

procedure TGameForm.SetPoolObj(Pool: TStringList; Name: String; Obj: TShape);
begin
  Pool.AddObject(Name, Obj);
  Obj.Parent := nil;
end;

function TGameForm.SpawnProj(Source: TRectangle; X, Y: Single; Angle: Single)
  : TRectangle;
var
  R: TRectangle;
  RAngle: Single;
  Projectile: TRectangle;
begin
  R := TRectangle(GetPoolObj(ProjPool));
  R.Width := 200;
  R.Height := 3;
  R.RotationAngle := Angle - 90;
  R.Stroke.Kind := TBrushKind.None;
  R.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
  R.Fill.Kind := TBrushKind.Bitmap;
  case RandomRange(1, 7) of
    1:
      Projectile := Projectile1;
    2:
      Projectile := Projectile2;
    3:
      Projectile := Projectile3;
    4:
      Projectile := Projectile4;
    5:
      Projectile := Projectile5;
    6:
      Projectile := Projectile6;
    else
      Projectile := Projectile1;
  end;
  R.Fill.Bitmap.Bitmap.Assign(Projectile.Fill.Bitmap.Bitmap);
  RAngle := R.RotationAngle * PI / 180;
  // set the center X,Y and then travel 100 out from the center on the angle
  R.Position.X := X + (Source.Width / 2) - (R.Width / 2) + 100 * Cos(RAngle);
  R.Position.Y := Y + (Source.Height / 2) - (R.Height / 2) + 100 * Sin(RAngle);
  R.Tag := PROJ_SPEED;
  R.TagFloat := 0;
  R.Parent := MapLayout1;
  R.BringToFront;
  Result := R;
end;

function TGameForm.SpawnPerson(X, Y: Single; Angle: Single; Speed: Integer)
  : TRectangle;
var
  R: TRectangle;
begin
  R := TRectangle(GetPoolObj(PeoplePool));
  R.Width := Person.Width;
  R.Height := Person.Height;
  R.RotationAngle := Angle;
  R.Position.X := X;
  R.Position.Y := Y - R.Height - 5;
  R.Tag := Speed;
  R.TagFloat := 0;

  R.Stroke.Kind := TBrushKind.None;
  R.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
  R.Fill.Kind := TBrushKind.Bitmap;
  R.Fill.Bitmap.Bitmap.Assign(Person.Fill.Bitmap.Bitmap);

  R.Parent := MapLayout1;
  R.BringToFront;
  Result := R;
end;

function TGameForm.SpawnPersonOnMinimap(Rect: TRectangle): TRectangle;
var
  R: TRectangle;
begin
  R := TRectangle(GetPoolObj(PeoplePoolForMinimap));
  R.Fill.Color := $FFFFFF00;
  R.Fill.Kind := TBrushKind.Solid;
  R.Width := 6;
  R.Height := 6;
  R.Position.X := Rect.Position.X /
    (MapLayout1.Width / MiniMapScaledLayout.Width);
  R.Position.Y := Rect.Position.Y /
    (MapLayout1.Height / MiniMapScaledLayout.Height);
  R.TagFloat := 0;
  R.Opacity := 1;
  R.Parent := MiniMapScaledLayout;
  R.BringToFront;
  Result := R;
end;

function TGameForm.SpawnEnemy(X, Y: Single; Angle: Single; Speed: Integer)
  : TRectangle;
var
  R: TRectangle;
  EnemyGraphic: TRectangle;
  AE: TBitmapListAnimation;
  AnimateEnemy: TBitmapListAnimation;
begin
  R := TRectangle(GetPoolObj(EnemyPool));
  R.Parent := MapLayout1;
  R.Width := 50;
  R.Height := 50;
  R.RotationAngle := 0;
  R.Position.X := X;
  R.Position.Y := Y;
  R.Tag := Speed;
  R.TagFloat := 0;
  R.TagString := ENEMY_AI_TRAVEL;

  R.Stroke.Kind := TBrushKind.None;
  R.Fill.Kind := TBrushKind.None;

  if not Assigned(R.TagObject) then
  begin
    EnemyGraphic := TRectangle.Create(R);
    EnemyGraphic.Parent := R;
  end
  else
  begin
    EnemyGraphic := TRectangle(R.TagObject);
  end;

  EnemyGraphic.TagString := ENEMY_LEVEL1;
  EnemyGraphic.TagFloat := Angle;
  EnemyGraphic.Width := R.Width;
  EnemyGraphic.Height := R.Height;
  EnemyGraphic.Position.X := (R.Width / 2) - (EnemyGraphic.Width / 2);
  EnemyGraphic.Position.Y := (R.Height / 2) - (EnemyGraphic.Height / 2);
  EnemyGraphic.RotationAngle := (360 - R.RotationAngle);

  EnemyGraphic.Stroke.Kind := TBrushKind.None;
  EnemyGraphic.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
  EnemyGraphic.Fill.Kind := TBrushKind.Bitmap;
  EnemyGraphic.Fill.Bitmap.Bitmap.Assign(Enemy1.Fill.Bitmap.Bitmap);

  if not Assigned(R.TagObject) then
  begin
    R.TagObject := EnemyGraphic;
  end;

  if not Assigned(EnemyGraphic.TagObject) then
  begin
    AE := TBitmapListAnimation.Create(EnemyGraphic);
    EnemyGraphic.TagObject := AE;
    AE.Parent := EnemyGraphic;
  end
  else
  begin
    AE := TBitmapListAnimation(EnemyGraphic.TagObject);
  end;

  AnimateEnemy := AnimateEnemy1;
  AE.AnimationBitmap.Assign(AnimateEnemy.AnimationBitmap);
  AE.AnimationCount := AnimateEnemy.AnimationCount;
  AE.AnimationRowCount := AnimateEnemy.AnimationRowCount;
  AE.PropertyName := AnimateEnemy.PropertyName;
  AE.Duration := AnimateEnemy.Duration;
  AE.Loop := AnimateEnemy.Loop;

  AE.Start;

  R.BringToFront;
  Result := R;
end;

function TGameForm.SpawnEnemyOnMinimap(Rect: TRectangle): TRectangle;
var
  R: TRectangle;
begin
  R := TRectangle(GetPoolObj(EnemyPoolForMinimap));
  R.Fill.Color := $FFADD8E6;
  R.Fill.Kind := TBrushKind.Solid;
  R.Width := 5;
  R.Height := 5;
  R.Position.X := Rect.Position.X /
    (MapLayout1.Width / MiniMapScaledLayout.Width);
  R.Position.Y := Rect.Position.Y /
    (MapLayout1.Height / MiniMapScaledLayout.Height);
  R.TagFloat := 0;
  R.Opacity := 1;
  R.Parent := MiniMapScaledLayout;
  R.BringToFront;
  Result := R;
end;

procedure TGameForm.UpgradeEnemy(R: TRectangle; Speed: Integer);
var
  EnemyGraphic: TRectangle;
  AE: TBitmapListAnimation;
  AnimateEnemy: TBitmapListAnimation;
begin
  EnemyGraphic := TRectangle(R.TagObject);

  EnemyGraphic.Fill.Bitmap.Bitmap.Assign(Enemy2.Fill.Bitmap.Bitmap);

  R.Tag := Speed;

  AE := TBitmapListAnimation(EnemyGraphic.TagObject);

  if EnemyGraphic.TagString = ENEMY_LEVEL1 then
  begin
    AnimateEnemy := AnimateEnemy2;
    EnemyGraphic.TagString := ENEMY_LEVEL2;
  end
  else if EnemyGraphic.TagString = ENEMY_LEVEL2 then
  begin
    R.Tag := Speed * 2;
    AnimateEnemy := AnimateEnemy3;
    EnemyGraphic.TagString := ENEMY_LEVEL3;
  end
  else
  begin
    AnimateEnemy := AnimateEnemy3;
  end;
  AE.AnimationBitmap.Assign(AnimateEnemy.AnimationBitmap);
  AE.AnimationCount := AnimateEnemy.AnimationCount;
  AE.AnimationRowCount := AnimateEnemy.AnimationRowCount;
  AE.PropertyName := AnimateEnemy.PropertyName;
  AE.Duration := AnimateEnemy.Duration;
  AE.Loop := AnimateEnemy.Loop;

  AE.Start;
end;

function TGameForm.SpawnEnemyProj(Source: TRectangle; X, Y: Single;
  Angle: Single): TRectangle;
var
  R: TRectangle;
  RAngle: Single;
begin
  R := TRectangle(GetPoolObj(ProjPool));
  R.Width := 5;
  R.Height := 5;
  R.RotationAngle := Angle - 90;
  R.Stroke.Kind := TBrushKind.None;
  R.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
  R.Fill.Kind := TBrushKind.Bitmap;
  R.Fill.Bitmap.Bitmap.Assign(EnemyProjectile.Fill.Bitmap.Bitmap);
  RAngle := R.RotationAngle * PI / 180;
  // set the center X,Y and then travel 25 out from the center on the angle
  R.Position.X := X + (Source.Width / 2) - (R.Width / 2) + 25 * Cos(RAngle);
  R.Position.Y := Y + (Source.Height / 2) - (R.Height / 2) + 25 * Sin(RAngle);
  R.Tag := ENEMYPROJ_SPEED;
  R.TagFloat := 0;
  R.Parent := MapLayout1;
  R.BringToFront;
  Result := R;
end;

function TGameForm.SpawnCollectItem(X, Y: Single; Size: Integer): TRectangle;
var
  R: TRectangle;
  CollectGraphic: TRectangle;
begin
  R := TRectangle(GetPoolObj(CollectPool));
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
  R.Parent := MapLayout1;
  R.BringToFront;
  Result := R;
end;

function TGameForm.SpawnExplosion(X, Y: Single; Angle: Single): TRectangle;
var
  R: TRectangle;
  AE: TBitmapListAnimation;
begin
  R := TRectangle(GetPoolObj(ExplosionPool));
  R.Parent := MapLayout1;
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
  R.BringToFront;
  Result := R;
end;

procedure TGameForm.CreateExplosion(X, Y: Single);
begin
  ExplosionList.AddObject('', SpawnExplosion(X, Y, 0));
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
    MAIN_MENU:
      begin
        MainMenuFrame.Visible := True;
        GameOverFrame.Visible := False;
        InstructionsFrame.Visible := False;
        HighScoresFrame.Visible := False;
        LevelCompleteFrame.Visible := False;
        SettingsFrame.Visible := False;
        HideHUD;
        StopMusic;
      end;
    INSTRUCTIONS:
      begin
        MainMenuFrame.Visible := False;
        GameOverFrame.Visible := False;
        InstructionsFrame.Visible := True;
        HideHUD;
        StopMusic;
      end;
    GAMEPLAY:
      begin
        InstructionsFrame.Visible := False;
        LevelCompleteFrame.Visible := False;
        ShowHUD;
        PlayMusic;
      end;
    LEVEL_COMPLETE:
      begin
        LevelCompleteFrame.Visible := True;
        HideHUD;
      end;
    GAME_OVER:
      begin
        HighScoresFrame.Visible := False;
        GameOverFrame.Visible := True;
        HideHUD;
      end;
    HIGH_SCORES:
      begin
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

function TGameForm.IsAttackDistance(R1, R2: TRectangle; Range: Single): Boolean;
var
  Distance: Single;
begin
  Result := False;
  Distance := R1.Position.Point.Distance(R2.Position.Point);
  if Distance < Range then
  begin
    Result := True;
  end;
end;

function TGameForm.EnemySearchAndTargetPeople(EnemyObj: TRectangle): Single;
var
  PersonObj: TRectangle;
  I: Integer;
  TmpDistance, ClosestDistance: Single;
begin
  Result := EnemyObj.RotationAngle;
  if (PeopleList.Count > 0) then
  begin
    ClosestDistance := MapLayout1.Width;
    for I := PeopleList.Count - 1 downto 0 do
    begin
      TmpDistance := EnemyObj.Position.Point.Distance
        (TRectangle(PeopleList.Objects[I]).Position.Point);
      if TmpDistance < ClosestDistance then
      begin
        ClosestDistance := TmpDistance;
        PersonObj := TRectangle(PeopleList.Objects[I]);
      end;
    end;
    if Assigned(PersonObj) then
      Result := GetTargetAngle(PersonObj.Position.X, PersonObj.Position.Y,
        EnemyObj.Position.X, EnemyObj.Position.Y)
    else
      Result := Random(360);
  end;
end;

function TGameForm.EnemyGetPlayerAngle(EnemyObj: TRectangle): Single;
begin
  Result := GetTargetAngle(Ship.Position.X, Ship.Position.Y,
    EnemyObj.Position.X, EnemyObj.Position.Y)
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

  if (AccY < -7) then
  begin
    AccDown.Visible := True;
    AccDownButtonDown := True;
  end
  else
  begin
    AccDown.Visible := False;
    AccDownButtonDown := False;
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
  MoveX, MoveY: Single;
  ProjAngle, EnemyAngle, EnemyProjAngle, CollectAngle, PersonAngle: Single;
  ProjObj: TRectangle;
  PersonObj: TRectangle;
  PersonObjMini: TRectangle;
  ExplosionObj: TRectangle;
  EnemyObj: TRectangle;
  EnemyObjMini: TRectangle;
  EnemyProjObj: TRectangle;
  CollectObj: TRectangle;
  Time: Cardinal;
  ScreenLayoutWidthDiv2, ScreenLayoutWidthDiv4: Single;
  RndWarp: Single;
begin
{$IFDEF DEBUG}
  Time := TThread.GetTickCount;
{$ENDIF}
  // Check for game over
  if (PlayerData.Lives <= 0) OR (PeopleList.Count = 0) then
  begin
    LevelFail;
    GameOver;
    Exit;
  end;

  // Check for level complete
  if (PlayerData.EnemiesDestroyed > (ENEMY_COUNT * PlayerData.Level)) then
  begin
    LevelComplete;
    Exit;
  end;

  if CanResetLayout = True then
  begin
    ResetLayout;
    CanResetLayout := False;
  end;

  ScreenLayoutWidthDiv2 := ScreenLayout.Width / 2;
  ScreenLayoutWidthDiv4 := ScreenLayout.Width / 4;

  // process accelerometer data
  if MotionSensor.Active = True then
  begin
    ProcessAccelerometer;
  end;

  // Handle player movement and firing
  PlayerData.FireInterval := PlayerData.FireInterval + 1;

  if (UpButtonDown OR AccUpButtonDown) then
  begin
    if PlayerData.SpeedY > PlayerData.MaxReverseSpeedY then
    begin
      PlayerData.SpeedY := PlayerData.SpeedY - PlayerData.AccelerationY;
    end;
  end;
  if (DownButtonDown OR AccDownButtonDown) then
  begin
    if PlayerData.SpeedY < PlayerData.MaxSpeedY then
    begin
      PlayerData.SpeedY := PlayerData.SpeedY + PlayerData.AccelerationY;
    end;
  end;
  if (RightButtonDown OR AccRightButtonDown) then
  begin
    if PlayerData.SpeedX < PlayerData.MaxSpeedX then
    begin
      PlayerData.SpeedX := PlayerData.SpeedX + PlayerData.AccelerationX;
    end;

    Ship.RotationAngle := 90;
  end;
  if (LeftButtonDown OR AccLeftButtonDown) then
  begin
    if PlayerData.SpeedX > PlayerData.MaxReverseSpeedX then
    begin
      PlayerData.SpeedX := PlayerData.SpeedX - PlayerData.AccelerationX;
    end;

    Ship.RotationAngle := -90;
  end;
  if FireButtonDown then
  begin
    FirePlayerProj;
  end;
 if (UpButtonDown OR AccUpButtonDown OR DownButtonDown OR AccDownButtonDown OR RightButtonDown OR AccRightButtonDown OR LeftButtonDown OR AccLeftButtonDown) then
  begin
    Thruster.Visible := True;
  end
 else
  begin
    Thruster.Visible := False;
  end;

  if PlayerData.CanWarp = True then
  begin
    RndWarp := RandomRange(Trunc(ScreenLayoutWidthDiv2),
      Trunc(Map1Width - (ScreenLayoutWidthDiv2)));
    MapLayout3.Position.X := 0 - ((RndWarp / 4) - (ScreenLayoutWidthDiv2) + 5);
    MapLayout2.Position.X := 0 - ((RndWarp / 2) - (ScreenLayoutWidthDiv2) + 5);
    MapLayout1.Position.X := (0 - (RndWarp - (ScreenLayoutWidthDiv2) + 5));
    Ship.Position.X := Abs(MapLayout1.Position.X) +
      (ScreenLayoutWidthDiv2 - (Ship.Width / 2));
    PlaySound(WARP_SFX);
    PlayerData.CanWarp := False;
  end;

  PlayerData.SpeedX := PlayerData.SpeedX * 0.98;
  PlayerData.SpeedY := PlayerData.SpeedY * 0.98;
  MoveX := PlayerData.SpeedX;
  MoveY := PlayerData.SpeedY;

  if Ship.Position.X < (MapLayout1.Position.X + ScreenLayoutWidthDiv2) then
  begin
    MoveX := MoveX + 15;
    PlayerData.SpeedX := 0;
  end
  else if Ship.Position.X > (MapLayout1.Width - ScreenLayoutWidthDiv2 - 25) then
  begin
    MoveX := MoveX - 15;
    PlayerData.SpeedX := 0;
  end;

  if Ship.Position.Y < 0 then
  begin
    Ship.Position.Y := 0;
  end;

  if Ship.Position.Y > (ScreenLayout.Height - Ship.Height) then
  begin
    Ship.Position.Y := ScreenLayout.Height - Ship.Height;
  end;

  ShipMini.Position.X := Ship.Position.X /
    (MapLayout1.Width / MiniMapScaledLayout.Width);
  ShipMini.Position.Y := Ship.Position.Y /
    (MapLayout1.Height / MiniMapScaledLayout.Height);

  Ship.Position.X := Ship.Position.X + MoveX;
  Ship.Position.Y := Ship.Position.Y + MoveY;

  // update map
  MapLayout1.Position.X := MapLayout1.Position.X - MoveX;
  MapLayout2.Position.X := MapLayout2.Position.X - (MoveX / 2);
  MapLayout3.Position.X := MapLayout3.Position.X - (MoveX / 4);

  // Handle explosions
  if ExplosionList.Count > 0 then
    for I := ExplosionList.Count - 1 downto 0 do
    begin
      ExplosionObj := TRectangle(ExplosionList.Objects[I]);

      ExplosionObj.TagFloat := ExplosionObj.TagFloat + 0.1;

      if (ExplosionObj.TagFloat > ExplosionObj.Tag) then
      begin
        SetPoolObj(ExplosionPool, ExplosionList[I], ExplosionObj);
        ExplosionList.Delete(I);
      end;
    end;

  // Handle player projectiles
  if ProjList.Count > 0 then
    for I := ProjList.Count - 1 downto 0 do
    begin
      ProjObj := TRectangle(ProjList.Objects[I]);
      if (ProjObj.TagFloat > PlayerData.ProjDuration) then
      begin
        SetPoolObj(ProjPool, ProjList[I], ProjObj);
        ProjList.Delete(I);
      end
      else
      begin
        ProjAngle := ProjObj.RotationAngle * PI / 180;
        ProjObj.Position.X := ProjObj.Position.X + ProjObj.Tag * Cos(ProjAngle);
        ProjObj.Position.Y := ProjObj.Position.Y + ProjObj.Tag * Sin(ProjAngle);

        if EnemyList.Count > 0 then
          for II := 0 to EnemyList.Count - 1 do
          begin
            EnemyObj := TRectangle(EnemyList.Objects[II]);
            if IntersectRect(EnemyObj.ParentedRect, ProjObj.ParentedRect) then
            begin
              EnemyObj.TagFloat := EnemyObj.TagFloat + 1;
              ProjObj.TagFloat := PlayerData.ProjDuration + 1;
              Break;
            end;
          end;

        if PeopleList.Count > 0 then
          for II := 0 to PeopleList.Count - 1 do
          begin
            PersonObj := TRectangle(PeopleList.Objects[II]);

            if IntersectRect(PersonObj.ParentedRect, ProjObj.ParentedRect) then
            begin
              PersonObj.TagFloat := PersonObj.TagFloat + 1;
              ProjObj.TagFloat := PlayerData.ProjDuration + 1;
              Break;
            end;
          end;

        ProjObj.TagFloat := ProjObj.TagFloat + 0.1;
      end;

    end;

  // Handle enemy movement and firing
  if EnemyList.Count > 0 then
    for I := EnemyList.Count - 1 downto 0 do
    begin
      EnemyObj := TRectangle(EnemyList.Objects[I]);
      EnemyObjMini := TRectangle(EnemyListForMinimap.Objects[I]);

      if EnemyObj.TagString = ENEMY_AI_RUN then
      begin
        TRectangle(EnemyObj.TagObject).TagFloat := 90;
        EnemyObj.TagString := ENEMY_AI_RUNNING;
      end
      else if EnemyObj.TagString = ENEMY_AI_TRAVEL then
      begin
        if IsAttackDistance(EnemyObj, Ship, ScreenLayoutWidthDiv4) then
        begin
          EnemyObj.TagString := ENEMY_AI_ATTACK;
        end
        else if Random(150) = 1 then
          EnemyObj.TagString := ENEMY_AI_SEARCH;
      end
      else if EnemyObj.TagString = ENEMY_AI_SEARCH then
      begin
        TRectangle(EnemyObj.TagObject).TagFloat :=
          EnemySearchAndTargetPeople(EnemyObj) - 90;
        EnemyObj.TagString := ENEMY_AI_TARGET;
      end
      else if EnemyObj.TagString = ENEMY_AI_UPGRADE then
      begin
        EnemyObj.TagString := ENEMY_AI_ATTACK;
      end
      else if EnemyObj.TagString = ENEMY_AI_ATTACK then
      begin
        if not IsAttackDistance(EnemyObj, Ship, ScreenLayout.Width) then
        begin
          EnemyObj.TagString := ENEMY_AI_TRAVEL;
        end
        else
          TRectangle(EnemyObj.TagObject).TagFloat :=
            EnemyGetPlayerAngle(EnemyObj) - 90;
      end;

      EnemyAngle := TRectangle(EnemyObj.TagObject).TagFloat * PI / 180;
      EnemyObj.Position.X := EnemyObj.Position.X + EnemyObj.Tag *
        Cos(EnemyAngle);
      EnemyObj.Position.Y := EnemyObj.Position.Y + EnemyObj.Tag *
        Sin(EnemyAngle);

      if PlayerData.Invulnerable = 0 then
        if IntersectRect(Ship.ParentedRect, EnemyObj.ParentedRect) then
        begin
          PlayerHit;
          EnemyObj.TagFloat := EnemyObj.TagFloat + 1;
        end;

      if (EnemyObj.TagString = ENEMY_AI_TRAVEL) OR
        (EnemyObj.TagString = ENEMY_AI_SEARCH) OR
        (EnemyObj.TagString = ENEMY_AI_TARGET) then
        if PeopleList.Count > 0 then
          for II := 0 to PeopleList.Count - 1 do
          begin
            PersonObj := TRectangle(PeopleList.Objects[II]);
            if IntersectRect(PersonObj.ParentedRect, EnemyObj.ParentedRect) AND
              (PersonObj.TagString = PEOPLE_STATE_NONE) then
            begin
              PersonObj.TagObject := EnemyObj;
              PersonObj.TagString := PEOPLE_STATE_CAPTURED;
              EnemyObj.TagString := ENEMY_AI_RUN;
              Break;
            end;
          end;

      if EnemyObj.Position.Point.Distance(Ship.Position.Point) <
        (ScreenLayoutWidthDiv2) then
      begin
        if Random(150) = 1 then
        begin
          EnemyProjList.AddObject('', SpawnEnemyProj(EnemyObj,
            EnemyObj.Position.X, EnemyObj.Position.Y,
            GetTargetAngle(Ship.Position.X, Ship.Position.Y,
            EnemyObj.Position.X, EnemyObj.Position.Y)));
          PlaySound(ALIEN_SFX);
        end;
      end;

      if CanLaunchBomb then
        if IntersectRect(EnemyObj.AbsoluteRect, ScreenLayout.AbsoluteRect) then
        begin
          EnemyObj.TagFloat := EnemyObj.TagFloat + 1;
        end;

      if (EnemyObj.Position.X < (ScreenLayoutWidthDiv2) - 10) then
      begin
        EnemyObj.Position.X := EnemyObj.Position.X + 5;
      end;

      if (EnemyObj.Position.X > Map1Width - (ScreenLayoutWidthDiv2) + 10) then
      begin
        EnemyObj.Position.X := EnemyObj.Position.X - 5;
      end;

      if (EnemyObj.Position.X > MapLayout1.Width) then
      begin
        EnemyObj.Position.X := MapLayout1.Width - EnemyObj.Width - 10;
      end;

      if (EnemyObj.Position.Y > MapLayout1.Height) then
      begin
        EnemyObj.Position.Y := MapLayout1.Height - EnemyObj.Height - 10;
      end;

      EnemyObjMini.Position.X := EnemyObj.Position.X /
        (MapLayout1.Width / MiniMapScaledLayout.Width);
      EnemyObjMini.Position.Y := EnemyObj.Position.Y /
        (MapLayout1.Height / MiniMapScaledLayout.Height);

      if (EnemyObj.Position.Y >= (MapLayout1.Height - (EnemyObj.Height + 10)))
        OR (EnemyObj.Position.Y <= MapLayout1.Position.Y) then
      begin
        TRectangle(EnemyObj.TagObject).TagFloat :=
          TRectangle(EnemyObj.TagObject).TagFloat + 180;
        if (EnemyObj.Position.Y <= MapLayout1.Position.Y) AND
          (EnemyObj.TagString = ENEMY_AI_RUNNING) then
        begin
          EnemyObj.TagString := ENEMY_AI_UPGRADE;
          UpgradeEnemy(EnemyObj, Trunc(ENEMY_SPEED * 1.5));
        end;
      end;

      if (EnemyObj.TagFloat > 0) then
      begin
        if EnemyObj.TagFloat > 0 then
        begin
          PlayerData.EnemiesDestroyed := PlayerData.EnemiesDestroyed + 1;
          CreateExplosion(EnemyObj.Position.X + (EnemyObj.Width / 2),
            EnemyObj.Position.Y + (EnemyObj.Height / 2));
          AddScore(150);
        end;
        EnemyObj.TagString := ENEMY_AI_NONE;
        SetPoolObj(EnemyPool, EnemyList[I], EnemyObj);
        SetPoolObj(EnemyPoolForMinimap, EnemyListForMinimap[I], EnemyObjMini);
        EnemyList.Delete(I);
        EnemyListForMinimap.Delete(I);
      end;
    end;

  // Handle enemy projectiles
  if EnemyProjList.Count > 0 then
    for I := EnemyProjList.Count - 1 downto 0 do
    begin
      EnemyProjObj := TRectangle(EnemyProjList.Objects[I]);
      EnemyProjAngle := EnemyProjObj.RotationAngle * PI / 180;
      EnemyProjObj.Position.X := EnemyProjObj.Position.X + EnemyProjObj.Tag *
        Cos(EnemyProjAngle);
      EnemyProjObj.Position.Y := EnemyProjObj.Position.Y + EnemyProjObj.Tag *
        Sin(EnemyProjAngle);

      if PlayerData.Invulnerable = 0 then
        if IntersectRect(Ship.ParentedRect, EnemyProjObj.ParentedRect) then
        begin
          PlayerHit;
          EnemyProjObj.TagFloat := EnemyProjObj.TagFloat + 1;
        end;

      if (EnemyProjObj.Position.X < ScreenLayout.Width - 10) then
      begin
        EnemyProjObj.TagFloat := EnemyProjObj.TagFloat + 1;
      end;

      if (EnemyProjObj.Position.X > Map1Width - ScreenLayout.Width + 10) then
      begin
        EnemyProjObj.TagFloat := EnemyProjObj.TagFloat + 1;
      end;

      if (EnemyProjObj.TagFloat > 0) OR
        (EnemyProjObj.Position.X > MapLayout1.Width) OR
        (EnemyProjObj.Position.Y > MapLayout1.Height) OR
        (EnemyProjObj.Position.X < MapLayout1.Position.X) OR
        (EnemyProjObj.Position.Y < MapLayout1.Position.Y) then
      begin
        SetPoolObj(EnemyProjPool, EnemyProjList[I], EnemyProjObj);
        EnemyProjList.Delete(I);
      end;
    end;

  // Spawn enemies
  if EnemyList.Count < 10 then
  begin
    if Random(30) = 1 then
    begin
      if PlayerData.EnemiesSpawned <= (ENEMY_COUNT * PlayerData.Level) then
      begin
        EnemyList.AddObject('',
          SpawnEnemy(RandomRange(Trunc(ScreenLayoutWidthDiv2),
          Trunc(MapLayout1.Width - ScreenLayoutWidthDiv2)),
          RandomRange(1, Trunc(MapLayout1.Height / 2)), RandomRange(1, 360),
          ENEMY_SPEED));
        EnemyListForMinimap.AddObject('',
          SpawnEnemyOnMinimap(TRectangle(EnemyList.Objects
          [EnemyList.Count - 1])));
        PlaySound(ALIEN_SFX);
        PlayerData.EnemiesSpawned := PlayerData.EnemiesSpawned + 1;
      end;
    end;
  end;

  // Handle people
  if PeopleList.Count > 0 then
    for I := PeopleList.Count - 1 downto 0 do
    begin
      PersonObj := TRectangle(PeopleList.Objects[I]);
      PersonObjMini := TRectangle(PeopleListForMinimap.Objects[I]);
      if PersonObj.TagString = PEOPLE_STATE_CAPTURED then
      begin
        EnemyObj := TRectangle(PersonObj.TagObject);
        if (EnemyObj.TagString = ENEMY_AI_RUN) OR
          (EnemyObj.TagString = ENEMY_AI_RUNNING) then
        begin
          PersonObj.Position.X := EnemyObj.Position.X +
            (EnemyObj.Width / 2 - (PersonObj.Width / 2));
          PersonObj.Position.Y := EnemyObj.Position.Y + EnemyObj.Height;
        end
        else if (EnemyObj.TagString = ENEMY_AI_UPGRADE) then
        begin
          PersonObj.TagString := PEOPLE_STATE_NONE;
          PersonObj.TagObject := nil;
          PersonObj.TagFloat := PersonObj.TagFloat + 1;
        end
        else
        begin
          PersonObj.TagString := PEOPLE_STATE_NONE;
          PersonObj.TagObject := nil;
        end;
      end
      else
      begin
        PersonAngle := 90 * PI / 180;
        PersonObj.Position.X := PersonObj.Position.X + PersonObj.Tag *
          Cos(PersonAngle);
        PersonObj.Position.Y := PersonObj.Position.Y + PersonObj.Tag *
          Sin(PersonAngle);
      end;

      if (PersonObj.Position.Y > (MapLayout1.Height - (PersonObj.Height))) then
      begin
        PersonObj.Position.Y := (MapLayout1.Height - (PersonObj.Height));
      end;

      if (PersonObj.Position.X < (ScreenLayoutWidthDiv2) - 10) then
      begin
        PersonObj.Position.X := PersonObj.Position.X + 5;
      end;

      if (PersonObj.Position.X > Map1Width - (ScreenLayoutWidthDiv2) + 10) then
      begin
        PersonObj.Position.X := PersonObj.Position.X - 5;
      end;

      if (PersonObj.Position.X > MapLayout1.Width) then
      begin
        PersonObj.Position.X := MapLayout1.Width - PersonObj.Width - 10;
      end;

      if (PersonObj.Position.Y > MapLayout1.Height) then
      begin
        PersonObj.Position.Y := MapLayout1.Height - PersonObj.Height - 10;
      end;

      PersonObjMini.Position.X := PersonObj.Position.X /
        (MapLayout1.Width / MiniMapScaledLayout.Width);
      PersonObjMini.Position.Y := PersonObj.Position.Y /
        (MapLayout1.Height / MiniMapScaledLayout.Height);

      if (PersonObj.TagFloat > 0) then
      begin
        SetPoolObj(PeoplePool, PeopleList[I], PersonObj);
        SetPoolObj(PeoplePoolForMinimap, PeopleListForMinimap[I],
          PersonObjMini);
        PeopleList.Delete(I);
        PeopleListForMinimap.Delete(I);
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

      if (CollectObj.ParentedRect.CenterPoint.X >=
        (MapLayout1.Width + (CollectObj.Width / 2))) then
      begin
        CollectObj.Position.X := (MapLayout1.Position.X + 1) -
          (CollectObj.Width / 2);
      end;

      if (CollectObj.ParentedRect.CenterPoint.Y >=
        (MapLayout1.Height + (CollectObj.Height / 2))) then
      begin
        CollectObj.Position.Y := (MapLayout1.Position.Y + 1) -
          (CollectObj.Height / 2);
      end;

      if (CollectObj.ParentedRect.CenterPoint.X <=
        (MapLayout1.Position.X - (CollectObj.Width / 2))) then
      begin
        CollectObj.Position.X := (MapLayout1.Width - 1);
      end;

      if (CollectObj.ParentedRect.CenterPoint.Y <=
        (MapLayout1.Position.Y - (CollectObj.Height / 2))) then
      begin
        CollectObj.Position.Y := (MapLayout1.Height - 1);
      end;

      if IntersectRect(Ship.ParentedRect, CollectObj.ParentedRect) then
      begin
        AddScore(5000);
        CollectObj.TagFloat := COLLECTITEM_DURATION + 1;
        PlaySound(COLLECT_SFX);
      end;

      if (CollectObj.Position.X < (ScreenLayoutWidthDiv2) - 10) then
      begin
        CollectObj.Position.X := CollectObj.Position.X + 5;
      end;

      if (CollectObj.Position.X > Map1Width - (ScreenLayoutWidthDiv2) + 10) then
      begin
        CollectObj.Position.X := CollectObj.Position.X - 5;
      end;

      if (CollectObj.Position.X > MapLayout1.Width) then
      begin
        CollectObj.Position.X := MapLayout1.Width - CollectObj.Width - 10;
      end;

      if (CollectObj.Position.Y > MapLayout1.Height) then
      begin
        CollectObj.Position.Y := MapLayout1.Height - CollectObj.Height - 10;
      end;

      CollectObj.TagFloat := CollectObj.TagFloat + 0.1;

      if (CollectObj.TagFloat > COLLECTITEM_DURATION) then
      begin
        SetPoolObj(CollectPool, CollectList[I], CollectObj);
        CollectList.Delete(I);
      end;
    end;

  // Spawn collectable
  if CollectList.Count < 1 then
  begin
    if Random(100) = 1 then
    begin
      CollectList.AddObject('',
        SpawnCollectItem(RandomRange(Trunc(ScreenLayoutWidthDiv2),
        Trunc(MapLayout1.Width - ScreenLayoutWidthDiv2)),
        RandomRange(1, Trunc(MapLayout1.Height - 1)), 0));
    end;
  end;

  // warp between portals
  if (Assigned(Portal1) = True) AND (Assigned(Portal2) = True) then
  begin

    if Portal1.TagFloat > 0 then
      Portal1.TagFloat := Portal1.TagFloat - 1;
    if Portal2.TagFloat > 0 then
      Portal2.TagFloat := Portal2.TagFloat - 1;

    if IntersectRect(Ship.ParentedRect, Portal1.ParentedRect) then
    begin
      if Portal1.TagFloat = 0 then
      begin
        Portal2.TagFloat := 30;
        WarpToObject(Portal2);
      end;
    end
    else if IntersectRect(Ship.ParentedRect, Portal2.ParentedRect) then
    begin
      if Portal2.TagFloat = 0 then
      begin
        Portal1.TagFloat := 30;
        WarpToObject(Portal1);
      end;
    end;
  end;

  // bomb fire complete
  if CanLaunchBomb = True then
  begin
    CanLaunchBomb := False;
    PlayerData.Bombs := PlayerData.Bombs - 1;
    DisplayBombs(PlayerData.Bombs);
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
    AccUp.Visible := False;
    AccUpButtonDown := False;
    AccDown.Visible := False;
    AccDownButtonDown := False;
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
  PlayerData.Health := PlayerData.Health - 1;
  DisplayHealth(PlayerData.Health);
  CreateExplosion(Ship.Position.X + (Ship.Width / 2),
    Ship.Position.Y + (Ship.Height / 2));
  if PlayerData.Health <= 0 then
  begin
    CreateExplosion(Ship.Position.X + RandomRange(1, Trunc(Ship.Width)),
      Ship.Position.Y + RandomRange(1, Trunc(Ship.Height)));
    PlayerData.Lives := PlayerData.Lives - 1;
    PlayerData.Health := PLAYER_HEALTH;
    Health1.Visible := True;
    Health2.Visible := True;
    Health3.Visible := True;
    PlayerData.SpeedX := 0;
    PlayerData.SpeedY := 0;
    DisplayLives(PlayerData.Lives);
    if PlayerData.Lives > 0 then
    begin
      PlayerData.Invulnerable := PlayerData.InvulnerableInterval;
      Ship.Opacity := Ship.Opacity - 0.25;
      InvulnerableTimer.Enabled := True;
      // CenterPlayerScreen;
      PlaySound(FAIL_SFX);
    end;
  end;
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

procedure TGameForm.DisplayBombs(Bombs: Integer);
begin
  if Bombs > 0 then
    Bombs1.Visible := True
  else
    Bombs1.Visible := False;
  if Bombs > 1 then
    Bombs2.Visible := True
  else
    Bombs2.Visible := False;
  if Bombs > 2 then
    Bombs3.Visible := True
  else
    Bombs3.Visible := False;
end;

procedure TGameform.FireDownEvent(Sender: TObject);
begin
  FireButtonDown := True;
end;

procedure TGameform.FireUpEvent(Sender: TObject);
begin
  FireButtonDown := False;
end;

procedure TGameform.DownDownEvent(Sender: TObject);
begin
  UpButtonDown := False;
  DownButtonDown := True;
end;

procedure TGameform.DownUpEvent(Sender: TObject);
begin
  DownButtonDown := False;
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
  DownButtonDown := False;
  UpButtonDown := True;
end;

procedure TGameform.UpUpEvent(Sender: TObject);
begin
  UpButtonDown := False;
end;

procedure TGameForm.DownActionDownExecute(Sender: TObject);
begin
  // called from RunRemoteActionAsync. Requires TThread.Synchronize for UI changes.
  DownDownEvent(Sender);
end;

procedure TGameForm.DownActionUpExecute(Sender: TObject);
begin
  // called from RunRemoteActionAsync. Requires TThread.Synchronize for UI changes.
  DownUpEvent(Sender);
end;

procedure TGameForm.DownBTNMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  DownDownEvent(Sender);
end;

procedure TGameForm.DownBTNMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  DownUpEvent(Sender);
end;

procedure TGameForm.DisplayHealth(Health: Integer);
begin
  if Health > 0 then
    Health1.Visible := True
  else
    Health1.Visible := False;
  if Health > 1 then
    Health2.Visible := True
  else
    Health2.Visible := False;
  if Health > 2 then
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
  HighScoresFrame.CloseInputBox;
  if LastStage = MAIN_MENU then
    SetStage(MAIN_MENU)
  else
    SetStage(GAME_OVER);
end;

procedure TGameForm.HighScoresFrameContinueBTNClick(Sender: TObject);
begin
  CloseHighScores;
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

procedure TGameForm.FireActionUpExecute(Sender: TObject);
begin
  // called from RunRemoteActionAsync. Requires TThread.Synchronize for UI changes.
  FireUpEvent(Sender);
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
  TDialogServiceAsync.MessageDialog('Exit the game?',
    System.UITypes.TMsgDlgType.mtInformation,
    [System.UITypes.TMsgDlgBtn.mbYes,
    System.UITypes.TMsgDlgBtn.mbNo], System.UITypes.TMsgDlgBtn.mbYes, 0,
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
    end);
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
  RegisterSound(DataFilePath + 'fire.wav');
  RegisterSound(DataFilePath + 'explosion.wav');
  RegisterSound(DataFilePath + 'fail.wav');
  RegisterSound(DataFilePath + 'win.wav');
  RegisterSound(DataFilePath + 'alien.wav');
  RegisterSound(DataFilePath + 'collect.wav');
  RegisterSound(DataFilePath + 'warp.wav');

  SetStage(MAIN_MENU);
end;

procedure TGameForm.FormDestroy(Sender: TObject);
begin
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
              FireDownEvent(Sender);
            end;
          'X', 'x':
            begin
              LaunchPlayerBomb;
            end;
          'C', 'c':
            begin
              PlayerWarp;
            end;
          'W', 'w':
            begin
              UpDownEvent(Sender);
            end;
          'S', 's':
            begin
              DownDownEvent(Sender);
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
          vkDown:
            begin
              DownDownEvent(Sender);
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
          INSTRUCTIONS, SETTINGS:
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
              FireUpEvent(Sender);
            end;
          'X', 'x':
            begin
              // LaunchPlayerBomb;
            end;
          'C', 'c':
            begin
              // PlayerWarp;
            end;
          'W', 'w':
            begin
              UpUpEvent(Sender);
            end;
          'S', 's':
            begin
              DownUpEvent(Sender);
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
          vkDown:
            begin
              DownUpEvent(Sender);
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

procedure TGameForm.FormResize(Sender: TObject);
begin
  MapLayout1.Height := ScreenLayout.Height;
  CanResetLayout := True;
end;

procedure TGameForm.ResetPlasmaFence;
begin
  if Assigned(PlasmaFence1) then
  begin
    PlasmaFence1.Position.X := (ScreenLayout.Width / 2) - PlasmaFence1.Width;
    PlasmaFence1.Height := MapLayout1.Height;
  end;
  if Assigned(PlasmaFence2) then
  begin
    PlasmaFence2.Position.X := MapLayout1.Width - (ScreenLayout.Width / 2);
    PlasmaFence2.Height := MapLayout1.Height;
  end;
  if Assigned(Portal1) then
  begin
    Portal1.Position.X := (ScreenLayout.Width / 2) + 100;
    Portal1.Position.Y := (ScreenLayout.Height / 2) - (Portal1.Height / 2);
  end;
  if Assigned(Portal2) then
  begin
    Portal2.Position.X := MapLayout1.Width - (ScreenLayout.Width / 2) - 100;
    Portal2.Position.Y := (ScreenLayout.Height / 2) - (Portal2.Height / 2);
  end;
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
      UpButtonDown := False;
      DownButtonDown := False;
      FireButtonDown := False;
      Thruster.Visible := False;
    end;
  end;
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

procedure TGameForm.UpBTNMouseDown(Sender: TObject; Button: TMouseButton;
Shift: TShiftState; X, Y: Single);
begin
  UpDownEvent(Sender);
end;

procedure TGameForm.UpBTNMouseUp(Sender: TObject; Button: TMouseButton;
Shift: TShiftState; X, Y: Single);
begin
  UpUpEvent(Sender);
end;

procedure TGameForm.WarpActionClickExecute(Sender: TObject);
begin
  // called from RunRemoteActionAsync. Requires TThread.Synchronize for UI changes.
  PlayerWarp;
end;

procedure TGameForm.WarpBTNClick(Sender: TObject);
begin
  PlayerWarp;
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
  PlayerData.Invulnerable := PlayerData.Invulnerable - 1;
  if PlayerData.Invulnerable <= 0 then
  begin
    InvulnerableTimer.Enabled := False;
    Ship.Opacity := Ship.Opacity + 0.25;
  end;
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

procedure TGameForm.ShowGamePad;
begin
  ShowMsgBox(NO_GAMEPAD);
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

procedure TGameForm.LaunchPlayerBomb;
begin
  if PlayerData.Bombs > 0 then
    CanLaunchBomb := True;
end;

procedure TGameForm.PlayerWarp;
begin
  if PlayerData.CanWarp = False then
    PlayerData.CanWarp := True;
end;

procedure TGameForm.FirePlayerProj;
begin
  if PlayerData.FireInterval > PlayerData.FireSpeed then
  begin
    ProjList.AddObject('', SpawnProj(Ship, Ship.Position.X, Ship.Position.Y,
      Ship.RotationAngle));
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
    AccDown.Visible := False;
    AccDownButtonDown := False;
    AccLeft.Visible := False;
    AccLeftButtonDown := False;
    AccRight.Visible := False;
    AccRightButtonDown := False;
  end;    
  if PlayerData.Invulnerable > 0 then
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
  LevelCompleteFrame.InfoText.Text := 'Score: ' +
    IntToStr(PlayerData.Score) + #13#10;
  SetStage(LEVEL_COMPLETE);
end;

procedure TGameForm.LevelCompleteFrameContinueBTNClick(Sender: TObject);
begin
  ContinueGame;
end;

procedure TGameForm.CreateEnemies;
var
  I: Integer;
begin
  for I := 0 to 10 do
  begin
    EnemyPool.AddObject('', TRectangle.Create(Self));
  end;
end;

procedure TGameForm.CreateExplosions;
var
  I: Integer;
begin
  for I := 0 to 10 do
  begin
    ExplosionPool.AddObject('', TRectangle.Create(Self));
  end;
end;

procedure TGameForm.CreateEnemyProj;
var
  I: Integer;
begin
  for I := 0 to 10 do
  begin
    EnemyProjPool.AddObject('', TRectangle.Create(Self));
  end;
end;

procedure TGameForm.CreateFencesAndPortals;
begin
  PlasmaFence1 := TRectangle.Create(nil);
  PlasmaFence1.Parent := MapLayout1;
  PlasmaFence1.Height := MapLayout1.Height;
  PlasmaFence1.Position.X := (ScreenLayout.Width / 2) - PlasmaFence1.Width;
  PlasmaFence1.Stroke.Kind := TBrushKind.None;
  BitmapToRectangle(PlasmaFence.Fill.Bitmap.Bitmap, PlasmaFence1);

  PlasmaFence2 := TRectangle.Create(nil);
  PlasmaFence2.Parent := MapLayout1;
  PlasmaFence2.Height := MapLayout1.Height;
  PlasmaFence2.Position.X := MapLayout1.Width - (ScreenLayout.Width / 2);
  PlasmaFence2.Stroke.Kind := TBrushKind.None;
  BitmapToRectangle(PlasmaFence.Fill.Bitmap.Bitmap, PlasmaFence2);

  Portal1 := TRectangle.Create(nil);
  Portal1.Parent := MapLayout1;
  Portal1.Width := Portal.Fill.Bitmap.Bitmap.Width;
  Portal1.Height := Portal.Fill.Bitmap.Bitmap.Height;
  Portal1.Position.X := (ScreenLayout.Width / 2) + 100;
  Portal1.Position.Y := (ScreenLayout.Height / 2) - (Portal1.Height / 2);
  Portal1.Opacity := 0.75;
  Portal1.Stroke.Kind := TBrushKind.None;
  BitmapToRectangle(Portal.Fill.Bitmap.Bitmap, Portal1);

  Portal2 := TRectangle.Create(nil);
  Portal2.Parent := MapLayout1;
  Portal2.Width := Portal.Fill.Bitmap.Bitmap.Width;
  Portal2.Height := Portal.Fill.Bitmap.Bitmap.Height;
  Portal2.Position.X := MapLayout1.Width - (ScreenLayout.Width / 2) - 100;
  Portal2.Position.Y := (ScreenLayout.Height / 2) - (Portal2.Height / 2);
  Portal2.Opacity := 0.75;
  Portal2.Stroke.Kind := TBrushKind.None;
  BitmapToRectangle(Portal.Fill.Bitmap.Bitmap, Portal2);
end;

procedure TGameForm.CreatePeople;
var
  I: Integer;
begin
  for I := 0 to PEOPLE_COUNT do
  begin
    PeopleList.AddObject('',
      SpawnPerson(RandomRange(Trunc(ScreenLayout.Width / 2),
      Trunc(MapLayout1.Width - (ScreenLayout.Width / 2))),
      MapLayout1.Height, 0, 1));
    PeopleListForMinimap.AddObject('',
      SpawnPersonOnMinimap(TRectangle(PeopleList.Objects[I])));
  end;
end;

procedure TGameForm.InitGame;
begin
  ProjList := TStringList.Create;
  ProjPool := TStringList.Create;
  ExplosionList := TStringList.Create;
  ExplosionPool := TStringList.Create;
  EnemyList := TStringList.Create;
  EnemyListForMinimap := TStringList.Create;
  EnemyPool := TStringList.Create;
  EnemyPoolForMinimap := TStringList.Create;
  EnemyProjList := TStringList.Create;
  EnemyProjPool := TStringList.Create;
  CollectList := TStringList.Create;
  CollectPool := TStringList.Create;
  PeopleList := TStringList.Create;
  PeopleListForMinimap := TStringList.Create;
  PeoplePool := TStringList.Create;
  PeoplePoolForMinimap := TStringList.Create;

  CreateEnemies;
  CreateExplosions;
  CreateEnemyProj;
  CreatePeople;

  Map3Width := MapLayout3.Width;
  Map3Height := MapLayout3.Height;
  Map2Width := MapLayout2.Width;
  Map2Height := MapLayout2.Height;
  Map1Width := MapLayout1.Width;
  Map1Height := ScreenLayout.Height;

  MapLayout3.Position.X := 0 - ((Map3Width / 2) - (ScreenLayout.Width / 2) + 5);
  MapLayout3.Position.Y := 0 -
    ((Map3Height / 2) - (ScreenLayout.Height / 2) + 5);
  MapLayout2.Position.X := 0 - ((Map2Width / 2) - (ScreenLayout.Width / 2) + 5);
  MapLayout2.Position.Y := 0 -
    ((Map2Height / 2) - (ScreenLayout.Height / 2) + 5);
  MapLayout1.Position.X := 0 - ((Map1Width / 2) - (ScreenLayout.Width / 2) + 5);
  MapLayout1.Position.Y := 0 -
    ((Map1Height / 2) - (ScreenLayout.Height / 2) + 5);

  CreateFencesAndPortals;

  Ship.Parent := MapLayout1;

  CleanedUp := False;

  Ship.RotationAngle := 90;
  CenterPlayer;
end;

procedure TGameForm.InitPlayer;
begin
  PlayerData := TPlayerData.Create(Self);
  PlayerData.Health := PLAYER_HEALTH;
  PlayerData.Lives := PLAYER_LIVES;
  PlayerData.Bombs := PLAYER_BOMBS;
  PlayerData.SpeedX := 0;
  PlayerData.SpeedY := 0;

  PlayerData.EnemiesDestroyed := 0;
  PlayerData.EnemiesSpawned := 0;

  PlayerData.MaxSpeedX := PLAYER_SPEED;
  PlayerData.MaxSpeedY := PLAYER_SPEED;
  PlayerData.MaxReverseSpeedX := PLAYER_SPEED * -1;
  PlayerData.MaxReverseSpeedY := PLAYER_SPEED * -1;
  PlayerData.AccelerationX := 1;
  PlayerData.AccelerationY := 0.5;

  PlayerData.Level := 1;
  PlayerData.FireSpeed := 5;
  PlayerData.FireInterval := PlayerData.FireSpeed;
  PlayerData.InvulnerableInterval := 5;
  PlayerData.ProjDuration := ((Max(ScreenLayout.Width, ScreenLayout.Height) / 2)
    / PROJ_SPEED) / 10;

  if LivesSet = False then
  begin
    BitmapToRectangle(Ship.Fill.Bitmap.Bitmap, Lives1);
    BitmapToRectangle(Ship.Fill.Bitmap.Bitmap, Lives2);
    BitmapToRectangle(Ship.Fill.Bitmap.Bitmap, Lives3);

    BitmapToRectangle(HealthBar.Fill.Bitmap.Bitmap, Health1);
    BitmapToRectangle(HealthBar.Fill.Bitmap.Bitmap, Health2);
    BitmapToRectangle(HealthBar.Fill.Bitmap.Bitmap, Health3);

    BitmapToRectangle(Bomb.Fill.Bitmap.Bitmap, Bombs1);
    BitmapToRectangle(Bomb.Fill.Bitmap.Bitmap, Bombs2);
    BitmapToRectangle(Bomb.Fill.Bitmap.Bitmap, Bombs3);
  end;

  DisplayLives(PlayerData.Lives);
  DisplayHealth(PlayerData.Health);
  DisplayScore;
  DisplayBombs(PlayerData.Bombs);

  MapLayout3.Position.Y := 0 -
    ((Map3Height / 2) - (ScreenLayout.Height / 2) + 5);
  MapLayout2.Position.Y := 0 -
    ((Map2Height / 2) - (ScreenLayout.Height / 2) + 5);
  MapLayout1.Height := ScreenLayout.Height;

  CenterPlayer;

  Ship.TagObject := PlayerData;

end;

procedure TGameForm.BitmapToRectangle(B: TBitmap; R: TRectangle);
begin
  R.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
  R.Fill.Kind := TBrushKind.Bitmap;
  R.Fill.Bitmap.Bitmap.Assign(B);
end;

procedure TGameForm.BombActionClickExecute(Sender: TObject);
begin
  // called from RunRemoteActionAsync. Requires TThread.Synchronize for UI changes.
  LaunchPlayerBomb;
end;

procedure TGameForm.BombBTNMouseDown(Sender: TObject; Button: TMouseButton;
Shift: TShiftState; X, Y: Single);
begin
  LaunchPlayerBomb;
end;

procedure TGameForm.AnimateExplosionFinish(Sender: TObject);
begin
  TRectangle(TBitmapListAnimation(Sender).Owner).Visible := False;
end;

procedure TGameForm.CenterPlayer;
begin
  Ship.Position.X := (MapLayout1.Width / 2) - (Ship.Width / 2);
  Ship.Position.Y := (MapLayout1.Height / 2) - (Ship.Height / 2);
  ShipMini.Position.X := (MiniMapScaledLayout.Width / 2) - (ShipMini.Width / 2);
  ShipMini.Position.Y := (MiniMapScaledLayout.Height / 2) -
    (ShipMini.Height / 2);
end;

procedure TGameForm.WarpToObject(R: TRectangle);
begin
  if Assigned(R) then
  begin
    MapLayout3.Position.X := 0 -
      ((R.Position.X / 4) - (ScreenLayout.Width / 2) + 5);
    MapLayout2.Position.X := 0 -
      ((R.Position.X / 2) - (ScreenLayout.Width / 2) + 5);
    MapLayout1.Position.X :=
      (0 - (R.Position.X - (ScreenLayout.Width / 2) + 5));
    Ship.Position.X := Abs(MapLayout1.Position.X) +
      (ScreenLayout.Width / 2 - (Ship.Width / 2));
    Ship.Position.Y := R.Position.Y + (R.Height / 2) - (Ship.Height / 2);
    PlaySound(WARP_SFX);
  end;
end;

procedure TGameForm.CenterPlayerScreen;
begin
  MapLayout3.Position.X := 0 - ((Map3Width / 2) - (ScreenLayout.Width / 2) + 5);
  MapLayout2.Position.X := 0 - ((Map2Width / 2) - (ScreenLayout.Width / 2) + 5);
  MapLayout1.Position.X := 0 - ((Map1Width / 2) - (ScreenLayout.Width / 2) + 5);
  Ship.Position.X := (MapLayout1.Width / 2) - (Ship.Width / 2);
  Ship.Position.Y := (MapLayout1.Height / 2) - (Ship.Height / 2);
end;

procedure TGameForm.CleanupGame(Continue: Boolean);
begin
 if CleanedUp=False then
  begin 
    ProjList.OwnsObjects := True;
    ProjPool.OwnsObjects := True;
    ExplosionList.OwnsObjects := True;
    ExplosionPool.OwnsObjects := True;
    EnemyList.OwnsObjects := True;
    EnemyListForMinimap.OwnsObjects := True;
    EnemyPool.OwnsObjects := True;
    EnemyPoolForMinimap.OwnsObjects := True;
    EnemyProjList.OwnsObjects := True;
    EnemyProjPool.OwnsObjects := True;
    CollectList.OwnsObjects := True;
    CollectPool.OwnsObjects := True;
    PeopleList.OwnsObjects := True;
    PeopleListForMinimap.OwnsObjects := True;
    PeoplePool.OwnsObjects := True;
    PeoplePoolForMinimap.OwnsObjects := True;

    PlasmaFence1.DisposeOf;
    PlasmaFence1 := nil;
    PlasmaFence2.DisposeOf;
    PlasmaFence2 := nil;

    Portal1.DisposeOf;
    Portal1 := nil;
    Portal2.DisposeOf;
    Portal2 := nil;

    if (Continue = False) then
      FreeAndNil(PlayerData);
    FreeAndNil(ProjList);
    FreeAndNil(ProjPool);
    FreeAndNil(ExplosionList);
    FreeAndNil(ExplosionPool);
    FreeAndNil(EnemyList);
    FreeAndNil(EnemyListForMinimap);
    FreeAndNil(EnemyPool);
    FreeAndNil(EnemyPoolForMinimap);
    FreeAndNil(EnemyProjList);
    FreeAndNil(EnemyProjPool);
    FreeAndNil(CollectList);
    FreeAndNil(CollectPool);
    FreeAndNil(PeopleList);
    FreeAndNil(PeopleListForMinimap);
    FreeAndNil(PeoplePool);
    FreeAndNil(PeoplePoolForMinimap);
    CleanedUp := True;
  end;
end;

procedure TGameForm.PlayGame;
begin
  SetStage(GAMEPLAY);
  StartGameLoop;
end;

procedure TGameForm.ContinueGame;
begin
  CleanupGame(True);
  InitGame;
  PlayerData.SpeedX := 0;
  PlayerData.SpeedY := 0;
  PlayerData.Level := PlayerData.Level + 1;
  PlayerData.EnemiesDestroyed := 0;
  PlayerData.EnemiesSpawned := 0;
  PlayGame;
end;

procedure TGameForm.LevelFail;
begin
  StopGameLoop;
  PlayerData.SpeedX := 0;
  PlayerData.SpeedY := 0;
  if PlayerData.Invulnerable > 0 then
  begin
    Ship.Opacity := Ship.Opacity + 0.25;
    PlayerData.Invulnerable := 0;
  end;
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

procedure TGameForm.ResetLayout;
begin

  MapLayout3.Position.Y := 0 -
    ((Map3Height / 2) - (ScreenLayout.Height / 2) + 5);
  MapLayout2.Position.Y := 0 -
    ((Map2Height / 2) - (ScreenLayout.Height / 2) + 5);
  MapLayout1.Height := ScreenLayout.Height;
  CenterPlayerScreen;
  ResetPeople;
  ResetPlasmaFence;

end;

procedure TGameForm.ResetPeople;
var
  I: Integer;
  PersonObj: TRectangle;
begin
  if Assigned(PeopleList) then
    if PeopleList.Count > 0 then
      for I := 0 to PeopleList.Count - 1 do
      begin
        PersonObj := TRectangle(PeopleList.Objects[I]);
        PersonObj.Position.Y := (ScreenLayout.Position.Y + ScreenLayout.Height)
          - PersonObj.Height;
      end;
end;

procedure TGameForm.OrientationChanged(const Sender: TObject;
const Msg: TMessage);
begin
  if CurrentStage = GAMEPLAY then
  begin

    MapLayout3.Position.Y := 0 -
      ((Map3Height / 2) - (ScreenLayout.Height / 2) + 5);
    MapLayout2.Position.Y := 0 -
      ((Map2Height / 2) - (ScreenLayout.Height / 2) + 5);
    MapLayout1.Height := ScreenLayout.Height;

    CanResetLayout := True;
  end;
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

// enable the GPU on Windows
// FMX.Types.GlobalUseGPUCanvas := True;
RegisterRenderingSetup;
Randomize;

end.
