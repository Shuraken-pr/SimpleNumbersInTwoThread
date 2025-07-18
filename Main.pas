unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, SyncObjs, System.Generics.Collections;

type
  TForm1 = class(TForm)
    pMain: TPanel;
    pThread1: TPanel;
    pThread2: TPanel;
    pResult: TPanel;
    lbThread1: TLabel;
    lbThread2: TLabel;
    lbResult: TLabel;
    mThread1: TMemo;
    mThread2: TMemo;
    mResult: TMemo;
    lbNum: TLabel;
    edNum: TEdit;
    btnStart: TButton;
    procedure edNumKeyPress(Sender: TObject; var Key: Char);
    procedure btnStartClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  var
  Form1: TForm1;

implementation

{$R *.dfm}
uses
  Math, System.Threading;

var
  ResultFile: TextFile;
  Thread1File, Thread2File: TextFile;
  CriticalSection: TCriticalSection;
  PrimesList: TList<Integer>;
  max_number: integer;

function IsPrime(n: Integer): Boolean;
var
  i: Integer;
begin
  if n < 2 then
    Exit(False);
  for i := 2 to Trunc(Sqrt(n)) do
  begin
    if n mod i = 0 then
      Exit(False);
  end;
  Result := True;
end;

procedure GeneratePrimesFromList(IntList, localList: TList<integer>; const ALocalFile: TextFile);
var
  curNum: integer;
begin
  while true do
  begin
    CriticalSection.Enter;
    try
      if IntList.Count = 0 then
        break;
      curNum := IntList[0];
      IntList.Delete(0);
    finally
      CriticalSection.Leave;
    end;

    if isPrime(curNum) then
    begin
      CriticalSection.Enter;
      try
        WriteLn(ALocalFile, curNum);
        WriteLn(ResultFile, curNum);
        localList.Add(curNum);
        PrimesList.Add(curNum);
      finally
        CriticalSection.Leave;
      end;
    end;
  end;
end;

function CheckOpenFile(const AFile: TextFile; AFileName: string): boolean;
begin
  Result := true;
  AssignFile(AFile, AFileName);
  try
    Rewrite(AFile); // Создаем (или очищаем) файл
  except
    Result := false;
  end;
end;

procedure TForm1.btnStartClick(Sender: TObject);
const
  fmtMessage = 'Вычисление простых чисел от 2 до %d завершено. Время выполнения: %s';
var
  delta: TDateTime;
  local1, local2, NumList: TList<Integer>;
  i: integer;
begin
  if TryStrToInt(edNum.Text, max_number) then
  begin
    if max_number <= 2 then
      exit;
    delta := Now;
    NumList := TList<integer>.Create;
    try
      for i := 2 to max_number do
        if i div 2 <> 0 then
        NumList.Add(i);
      mThread1.Lines.Clear;
      mThread2.Lines.Clear;
      mResult.Lines.Clear;

      if not CheckOpenFile(ResultFile, 'Result.txt') then
        exit;
      if not CheckOpenFile(Thread1File, 'Thread1.txt') then
        exit;
      if not CheckOpenFile(Thread2File, 'Thread2.txt') then
        exit;

      PrimesList := TList<Integer>.Create;
      local1 := TList<Integer>.Create;
      local2 := TList<Integer>.Create;
      CriticalSection := TCriticalSection.Create;
      try
        // Запускаем два потока
        var Task1 := TTask.Run(
          procedure
          begin
            GeneratePrimesFromList(NumList, local1, Thread1File);
          end
        );

        var Task2 := TTask.Run(
          procedure
          begin
            GeneratePrimesFromList(NumList, local2, Thread2File);
          end
        );

        // Ожидаем завершения задач
        Task1.Wait;
        Task2.Wait;

      finally
        CriticalSection.Free;
        local1.Free;
        local2.Free;
        PrimesList.Free;
        CloseFile(ResultFile);
        CloseFile(Thread1File);
        CloseFile(Thread2File);
      end;
    finally
      mResult.Lines.LoadFromFile('Result.txt');
      mThread1.Lines.LoadFromFile('Thread1.txt');
      mThread2.Lines.LoadFromFile('Thread2.txt');
      FreeAndNil(NumList);
      ShowMessage(Format(fmtMessage, [max_number, FormatDateTime('hh:nn:ss.zzz', Now - delta)]));
    end;
  end;
end;

procedure TForm1.edNumKeyPress(Sender: TObject; var Key: Char);
var
  CurrentNum: integer;
begin
  if TryStrToInt(edNum.Text + Key, CurrentNum) then
  begin
    if CurrentNum > 1000000 then
      key := #0;
  end
    else
    key := #0;
end;

end.
