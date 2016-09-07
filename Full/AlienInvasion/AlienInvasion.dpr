program AlienInvasion;

uses
  System.StartUpCopy,
  FMX.Forms,
  uGame in 'uGame.pas' {GameForm},
  uGameOver in 'uGameOver.pas' {FrameGameOver: TFrame},
  uGamepad in 'uGamepad.pas' {FrameGamepad: TFrame},
  uHighScores in 'uHighScores.pas' {FrameHighScores: TFrame},
  uInstructions in 'uInstructions.pas' {FrameInstructions: TFrame},
  uLevelComplete in 'uLevelComplete.pas' {FrameLevelComplete: TFrame},
  uMainMenu in 'uMainMenu.pas' {FrameMainMenu: TFrame},
  uSettings in 'uSettings.pas' {FrameSettings: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGameForm, GameForm);
  Application.Run;
end.
