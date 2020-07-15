//---------------------------------------------------------------------------

// This software is Copyright (c) 2016-2020 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit uHighScores;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.IniFiles,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.ListBox,
  FMX.Objects, FMX.ListView.Types, FMX.ListView,
  FMX.Controls.Presentation, FMX.Edit, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base;

type
  TFrameHighScores = class(TFrame)
    ContinueBTN: TButton;
    Rectangle1: TRectangle;
    ListView1: TListView;
    Label1: TLabel;
    InputBGRect: TRectangle;
    InputBoxRect: TRectangle;
    Label2: TLabel;
    InputEdit: TEdit;
    GridPanelLayout1: TGridPanelLayout;
    OkayBTN: TButton;
    CancelBTN: TButton;
    Line1: TLine;
    procedure OkayBTNClick(Sender: TObject);
    procedure CancelBTNClick(Sender: TObject);
  private
    { Private declarations }
    Loaded: Boolean;
  public
    { Public declarations }
    procedure AddScore(Name: String; Score: Integer);
    procedure SaveScore(Name: String; Score: Integer);
    procedure InitFrame;
    procedure CloseInputBox;
    procedure PopulateHighScores;
  end;

implementation

uses
  uGame, IOUtils;

{$R *.fmx}

procedure TFrameHighScores.CancelBTNClick(Sender: TObject);
begin
  CloseInputBox;
end;

procedure TFrameHighScores.AddScore(Name: String; Score: Integer);
begin
  if Name.Trim <> '' then
  begin
    SaveScore(Name, Score);
  end
  else
  begin
    InputEdit.Tag := Score;
    InputBGRect.Visible := True;
    InputBGRect.BringToFront;
    InputBoxRect.Visible := True;
    InputBoxRect.BringToFront;
  end;
end;

procedure TFrameHighScores.SaveScore(Name: String; Score: Integer);
var
IniFile: TMemIniFile;
begin
  if (Name.Trim <> '') then
  begin
	IniFile := TMemIniFile.Create(GameForm.SettingsFilePath + 'HighScores.ini');
	IniFile.WriteInteger('HighScores', Name + ' [' + System.Sysutils.DateTimeToStr(Now) + ']', Score );
	IniFile.UpdateFile;
	IniFile.Free;
	PopulateHighScores;
  end;
end;

function StringListSortCompareV(List: TStringList; Index1,Index2: Integer): Integer;
var
A,B: Integer;
begin
 A := StrToIntDef(List.ValueFromIndex[Index1], -1);
 B := StrToIntDef(List.ValueFromIndex[Index2], -1);
 Result := B - A;
end;

procedure TFrameHighScores.PopulateHighScores;
var
I: Integer;
SL: TStringList;
IniFile: TMemIniFile;
LItem: TListViewItem;
begin
  SL := TStringList.Create;
	IniFile := TMemIniFile.Create(GameForm.SettingsFilePath + 'HighScores.ini');
	if IniFile.SectionExists('HighScores')=True then
   begin
 	   ListView1.Items.Clear;
		 IniFile.ReadSectionValues('HighScores', SL);
		 SL.CustomSort(StringListSortCompareV);
		 for I := 0 to SL.Count-1 do
       begin
		     LItem := ListView1.Items.Add();
		   	 LItem.Text := SL.Names[I];
		  	 LItem.Detail := SL.ValueFromIndex[I];
		   end;
	end;
end;

procedure TFrameHighScores.InitFrame;
begin
  if Loaded = False then
  begin
    try
      PopulateHighScores;
    except
      on e: Exception do
      begin
        ShowMessage(e.Message);
      end;
    end;
    Loaded := True;
  end;
end;

procedure TFrameHighScores.CloseInputBox;
begin
  InputBGRect.Visible := False;
  InputBoxRect.Visible := False;
end;

procedure TFrameHighScores.OkayBTNClick(Sender: TObject);
begin
  CloseInputBox;
  SaveScore(InputEdit.Text, InputEdit.Tag);
end;

end.
