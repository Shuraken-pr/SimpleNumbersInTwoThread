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

procedure GeneratePrimes(ThreadID: Integer; LocalPrimes: TList<Integer>);
var
  i: Integer;
begin
    // Сохраняем уникальные простые числа в общий список
  for i := 2 to max_number do
  begin
    if IsPrime(i) then
    begin
      CriticalSection.Enter;
      try
      //дадим поработать второму потоку.
        if ThreadID = 1 then
          sleep(10);
        if not PrimesList.Contains(i) then
        begin
          LocalPrimes.Add(i);
          PrimesList.Add(i);
        end;
      finally
        CriticalSection.Leave;
      end;
    end;
  end;
end;

procedure GeneratePrimesFromList(IntList, localList: TList<integer>);
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
        localList.Add(curNum);
        PrimesList.Add(curNum);
      finally
        CriticalSection.Leave;
      end;
    end;
  end;
end;

procedure TForm1.btnStartClick(Sender: TObject);
var
  local1, local2, NumList: TList<Integer>;
  i, num: integer;
begin
  if TryStrToInt(edNum.Text, max_number) then
  begin
    NumList := TList<integer>.Create;
    try
      for i := 2 to max_number do
        NumList.Add(i);
      mThread1.Lines.Clear;
      mThread2.Lines.Clear;
      mResult.Lines.Clear;

      AssignFile(ResultFile, 'Result.txt');
      Rewrite(ResultFile); // Создаем (или очищаем) файл
      AssignFile(Thread1File, 'Thread1.txt');
      Rewrite(Thread1File); // Создаем (или очищаем) файл
      AssignFile(Thread2File, 'Thread2.txt');
      Rewrite(Thread2File); // Создаем (или очищаем) файл

      PrimesList := TList<Integer>.Create;
      local1 := TList<Integer>.Create;
      local2 := TList<Integer>.Create;
      CriticalSection := TCriticalSection.Create;
      try
        // Запускаем два потока
        var Task1 := TTask.Run(
          procedure
          begin
            GeneratePrimesFromList(NumList, local1);
          end
        );

        var Task2 := TTask.Run(
          procedure
          begin
            GeneratePrimesFromList(NumList, local2);
          end
        );

        // Ожидаем завершения задач
        Task1.Wait;
        Task2.Wait;

        // Записываем уникальные простые числа в файл
        for num in PrimesList do
        begin
          mResult.Lines.Add(IntToStr(num));
          WriteLn(ResultFile, num);
        end;

        for num in Local1 do
        begin
          mThread1.Lines.Add(IntToStr(num));
          WriteLn(Thread1File, num);
        end;
        for num in Local2 do
        begin
          mThread2.Lines.Add(IntToStr(num));
          WriteLn(Thread2File, num);
        end;
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
      FreeAndNil(NumList);
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
