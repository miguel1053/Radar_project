program RadarApp;

{$mode objfpc}{$H+}

uses
  Interfaces,
  Forms,
  GuiAppUnit
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

