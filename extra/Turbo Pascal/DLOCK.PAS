{+-------------------------------------------------------------+}
{: Program : dLock.pas :}
{: Author : Joseph L. Cousins :}
{: for : Sierra Consultants :}
{: 11700 Old Columbia Pike Suite 1119 :}
{: Silver Spring, MD 20904 :}
{: (301) 680-9625 :}
{: Date: July 01, 1989 :}
{+-------------------------------------------------------------+}
{: Purpose : To open a dBASE IV database and LOCK-IT :}
{: (make it READ-ONLY both for dBASE IV and DOS) :}
{+-------------------------------------------------------------+}
{Version 2.1 01/06/90 }
{Added Errors procedure}
{Added processing to lock associated Memo database if found}
PROGRAM dLock;
  USES Dos,Crt;
  TYPE
  {+-----------------------------------------------------------+}
  {: dBASE IV Header Structure :}
  {+-----------------------------------------------------------+}
  DB4Head = Record
    DBType : Byte;
     {Bit 7 = dBASE III+/IV Memo File 1=YES, 0=NO}
     {Bit 6 = SQL Table indicator 1=YES, 0=NO}
     {Bit 5 = Read-Only indicator 1=YES, 0=NO}
     {Bit 4 = SQL Table indicator 1=YES, 0=NO}
     {Bit 3 = dBASE IV Memo File 1=YES, 0=NO}
     {Bits 2-0 = dBASE Version Number 2=2, 3=3, 4=3}
    LastUpdateYear : Byte;
    LastUpdateMonth : Byte;
    LastUpdateDay : Byte;
    NumberOfRecords : Longint;
    PosFirstRec : Integer;
    RecordSize : Integer;
    Rserved1 : Integer;
    Transaction : Byte;
    Encryption : Byte;
    dBASELan : Packed Array[1..12] of Byte;
    MDXFileFlag : Byte;
    Reserved2 : Packed Array[1..3] of Byte;
  END;

  Str80 = String[80];
  {}
  VAR
    DirPath : String[67];
    Fname : String[8];
    Fext : String[4];
    Ver : Word;
    DBF_File, DBT_File : PathStr;
    DB_Type, DB_Ver : Byte;
    Header4 : DB4Head;
    DosDBFLock, DosDBTLock,
    DBLock, DB4_Memo, DB3_Memo : Boolean;
  {}
  Procedure Errors(Stuff: Str80);
  Begin
    Write(Chr(07)); { Write bell char ( sound BEEP ) }
    WriteLN(Stuff); { Write out error line }
    Halt; { Stop processing and quit }
  End;
  {}
  Procedure Process_DBF_File;
  Var
    F1 : File;
    FileLine : PathStr;
    DirString : String[128];
    Attr : Word;
  Begin
    DirString := GetEnv('PATH'); { get path string for search }
    FileLine := FSearch(DBF_File,DirString); { search for DBF file }
    If FileLine = '' Then
      Errors('Cant find '+DBF_File) { cannot find the file }
    else
    Begin
      FileLine := FExpand(FileLine); { found file..expand path }
      DBF_File := FileLine;
      FSplit(DBF_File,DirPath,Fname,Fext);
      Assign(F1,DBF_File);
      GetFAttr(F1, Attr);
      If (Attr and ReadOnly) = 1 then
        DosDBFLock := True;
    End;
  End;
  {}
  Procedure Process_DBT_File;
  Var
    F1 : File;
    FileLine : PathStr;
    DirString : String[128];
    Attr : Word;
  Begin
    DirString := GetEnv('PATH');
    FileLine := FSearch(DBT_File,DirString);
    If FileLine = '' Then
    Errors('Cant find '+DBT_File)
  else
    Begin
      FileLine := FExpand(FileLine);
      DBT_File := FileLine;
      FSplit(DBT_File,DirPath,Fname,Fext);
      Assign(F1,DBT_File);
      GetFAttr(F1, Attr);
      If (Attr and ReadOnly) = 1 then
        DosDBTLock := True;
    End;
  End;
  {}
  Procedure Process_Header;
  Var
    F1 : File;
  Begin
    Assign(F1,DBF_File); { assign DBF file }
    FileMode := 0; { mode = 0 = readonly }
    Reset(F1,32); { position to beginning blocksize = 32 }
    BlockRead(F1,Header4,1); { read block into Header4 record area }
    DB_Type := Header4.dbtype; { Set DB_Type to type in header }
    Close(F1); { close file access }
    FileMode := 2; { mode = 2 = read/write }
    DB_Ver := ( DB_Type and 7 ); { Version is just 3 bits }
    If (DB_Type and $20) = $20 then
      DBLock := True; { True if already locked }
    If (DB_Type and $10) = $10 Then
      DB4_Memo := True; { dBASE IV type memo file }
    If (DB_Type and $80) = $80 Then
      DB3_Memo := True; { dBASE III+ type memo file }
  End;
  {}
  Procedure Lockit;
  Var
    F1 : File;
    Attr : Word;
  Begin
    If DBLock = False then { Is DBF file already locked ? }
    Begin
      Assign(F1,DBF_File); { NO... assign file }
      GetFAttr(F1, Attr);
      If DosDBFLock then { Is file locked by DOS ? }
      Begin
        Attr := Attr and ReadOnly;
        SetFAttr(F1,Attr);
      End;
      Reset(F1,32); { reset file to start & blocksize=32 }
      BlockRead(F1,Header4,1); { get first record in work area }
      Reset(F1,32); { reset file again }
      With Header4 do
        DBType := DBType or $20; { set ReadOnly bit in header }
        BlockWrite(F1,Header4,1); { Write block back to file }
        DBLock := True;
        Close(F1); { close file }
        Assign(F1,DBF_File); { reopen file again }
        Attr := Attr or ReadOnly;
      SetFAttr(F1,Attr);{ set DOS attribute to ReadOnly }
    End
    Else
    If DosDBFLock = False then
    Begin
      Assign(F1,DBF_File);
      GetFAttr(F1, Attr);
      Attr := Attr or ReadOnly;
      SetFAttr(F1,Attr);
      DosDBFLock := True;
    End;

    If DB3_Memo or DB4_Memo then
    Begin
      Assign(F1,DBT_File);
      GetFAttr(F1, Attr);
      Attr := Attr or ReadOnly;
      SetFAttr(F1,Attr);
    End;
  End;
  {}
  Begin {Procedure dLock}
    DB_Type := 0;
    DirPath := '';
    Fname := '';
    Fext := '';
    DosDBFLock := False;
    DosDBTLock := False;
    DBLock := False;
    DB4_Memo := False;
    DB3_Memo := False;
    Ver := DosVersion;
    If Lo(DosVersion) < 3 then
      Errors('I cannot execute unless DOS is Ver 3.0 or above');
    FSplit(ParamStr(0),DirPath,Fname,Fext);
    If Fname <> 'DLOCK' then
      Errors('Rename Program back to "DLOCK" please.....!!!');
    If ParamCount < 1 then
      Errors('Format: dLock inputfilespec ');
    DBF_File := ParamStr(1);
    Process_DBF_File;
    If Fext <> '.DBF' Then
      Errors('File = '+DBF_File+' missing .dbf extension !');
    Process_Header;
    If DB3_Memo or DB4_Memo then
    Begin
      DBT_File := DirPath+Fname+'.DBT';
      Process_DBT_File;
    End;
    If DB_Ver > 2 Then
    Begin
      Lockit;
      WriteLN(' ');
      WriteLN('dLock Ver 2.1 Copyright (c) 1989,1990');
      WriteLN(' by');
      WriteLN(' Sierra Consultants');
      WriteLN(' 11700 Old Columbia Pike Suite 1119');
      WriteLN(' Silver Spring, MD 20904');
      WriteLN(' (301) 680-0763');
      WriteLN(' ');
      WriteLN('Its dLocked ');
    End
    Else
      Errors('dBASE II Databases must be converted to dBASE III/IV before using');
    Halt;
END.