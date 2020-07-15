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
  System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.Phys.SQLite, FireDAC.Stan.ExprFuncs, FireDAC.Stan.Param,
  FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.FMXUI.Wait,
  Data.Bind.EngExt, FMX.Bind.DBEngExt, System.Rtti, System.Bindings.Outputs,
  FMX.Bind.Editors, Data.Bind.Components, Data.Bind.DBScope, FireDAC.Comp.UI,
  FireDAC.Comp.Client, Data.DB, FireDAC.Comp.DataSet, FMX.Layouts, FMX.ListBox,
  FMX.Objects, FMX.ListView.Types, FMX.ListView, FireDAC.Phys.SQLiteDef,
  FMX.Controls.Presentation, FMX.Edit, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FireDAC.Phys.SQLiteWrapper.Stat;

type
  TFrameHighScores = class(TFrame)
    FireScoresList: TFDConnection;
    FDTableHighScores: TFDTable;
    FDQueryDelete: TFDQuery;
    FDQueryInsert: TFDQuery;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    BindSourceDB1: TBindSourceDB;
    BindingsList1: TBindingsList;
    ContinueBTN: TButton;
    Rectangle1: TRectangle;
    ListView1: TListView;
    LinkFillControlToField2: TLinkFillControlToField;
    FDTableHighScoresName: TWideMemoField;
    FDTableHighScoresScore: TIntegerField;
    Label1: TLabel;
    InputBGRect: TRectangle;
    InputBoxRect: TRectangle;
    Label2: TLabel;
    InputEdit: TEdit;
    GridPanelLayout1: TGridPanelLayout;
    OkayBTN: TButton;
    CancelBTN: TButton;
    Line1: TLine;
    procedure FireScoresListAfterConnect(Sender: TObject);
    procedure FireScoresListBeforeConnect(Sender: TObject);
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
  end;

implementation

uses
  IOUtils;

{$R *.fmx}

procedure TFrameHighScores.CancelBTNClick(Sender: TObject);
begin
  CloseInputBox;
end;

procedure TFrameHighScores.FireScoresListAfterConnect(Sender: TObject);
begin
  FireScoresList.ExecSQL
    ('CREATE TABLE IF NOT EXISTS HighScores (Name TEXT NOT NULL, Score INTEGER)');
end;

procedure TFrameHighScores.FireScoresListBeforeConnect(Sender: TObject);
begin
  FireScoresList.Params.Values['Database'] := TPath.GetDocumentsPath + PathDelim
    + 'HighScores.s3db';
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
begin
  if (Name.Trim <> '') then
  begin
    FDQueryInsert.ParamByName('Name').AsString := Name;
    FDQueryInsert.ParamByName('Score').AsInteger := Score;
    FDQueryInsert.ExecSQL();
    FDTableHighScores.Refresh;
    LinkFillControlToField2.BindList.FillList;
  end;
end;

procedure TFrameHighScores.InitFrame;
begin
  if Loaded = False then
  begin
    try
      // For unidirectional dataset, don't refill automatically when dataset is activated
      // because dataset is reactivated everytime use DataSet.First.
      LinkFillControlToField2.AutoActivate := False;
      LinkFillControlToField2.AutoFill := False;
      FireScoresList.Connected := True;
      FDTableHighScores.Active := True;
      LinkFillControlToField2.BindList.FillList;
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
