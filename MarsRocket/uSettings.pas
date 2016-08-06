//---------------------------------------------------------------------------

// This software is Copyright (c) 2016 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit uSettings;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Objects, FMX.Controls.Presentation;

type
  TFrameSettings = class(TFrame)
    BackgroundRect: TRectangle;
    HeaderLBL: TLabel;
    MainMenuBTN: TButton;
    ControllerHostLayout: TLayout;
    HostSwitch: TSwitch;
    HostLBL: TLabel;
    GyroLayout: TLayout;
    GyroSwitch: TSwitch;
    GyroLBL: TLabel;
    MusicLayout: TLayout;
    MusicSwitch: TSwitch;
    MusicLBL: TLabel;
    SoundLayout: TLayout;
    SoundSwitch: TSwitch;
    SoundLBL: TLabel;
    FullScreenLayout: TLayout;
    FullScreenSwitch: TSwitch;
    FullScreenLBL: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

end.
