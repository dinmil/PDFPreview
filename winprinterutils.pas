unit WinPrinterUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  {$IF Defined(MSWINDOWS)}
  ,windows, winspool
  {$ENDIF}
  ;

{$IF Defined(MSWINDOWS)}

function GetPrinterDuplexParam(InPrinterName: String): Boolean;
function SetPrinterDuplexObsolete(hWnd: HWND; InDevice: String; InIsItDuplex: Boolean): boolean;
function SetPrinterDuplexGlobally(InPrinterName: String; InIsItDuplex: Boolean; InBroadcastChangeMessage: Boolean): boolean;

{$ENDIF}


implementation

{$IF Defined(MSWINDOWS)}

function GetPrinterDuplexParam(InPrinterName: String): Boolean;
var hPrinter: THANDLE;
    MyPDevMode: LPDEVMODE;
    dwNeeded, dwRet: LONG;
    MySet: DWORD;
    MyResult: DWORD;
begin
  MyResult := DMDUP_SIMPLEX;

  MyPDevMode := nil;
  hPrinter := 0;
  // Start by opening the printer
  if (OpenPrinter(PChar(InPrinterName), @hPrinter, nil)) then begin
    // Step 1:
    // Allocate a buffer of the correct size.
    dwNeeded := DocumentProperties(0,
       hPrinter,       // Handle to our printer.
       PChar(InPrinterName),// Name of the printer.
       nil,            // Asking for size, so
       nil,            // these are not used.
       0);             // Zero returns buffer size.
    GetMem(MyPDevMode, dwNeeded);

    // Step 2:
    // Get the default DevMode for the printer and
    // modify it for your needs.
    dwRet := DocumentProperties(0,
       hPrinter,
       PChar(InPrinterName),
       MyPDevMode,    // The address of the buffer to fill.
       nil,           // Not using the input buffer.
       DM_OUT_BUFFER); // Have the output buffer filled.
    if (dwRet <> IDOK) then begin
      // If failure, cleanup and return failure.
      ClosePrinter(hPrinter);
      if MyPDevMode <> nil then begin
        freemem(MyPDevMode);
        MyPDevMode := nil;
      end;
      Exit(False);
    end;

    MyResult := MyPDevMode^.dmDuplex;

    // Finished with the printer
    ClosePrinter(hPrinter);

    // Return the modified DevMode structure.
    if MyPDevMode <> nil then begin
      freemem(MyPDevMode);
      MyPDevMode := nil;
    end;
  end;
  Result := MyResult <> DMDUP_SIMPLEX;
end;

function SetPrinterDuplexObsolete(hWnd: HWND; InDevice: String; InIsItDuplex: Boolean): boolean;
var hPrinter: THANDLE;
    MyPDevMode: LPDEVMODE;
    MySize: Integer;
    dwNeeded, dwRet: LONG;
    MyResult: Boolean;
    MySet: DWORD;
    pd:PRINTER_DEFAULTS;
    pi2: PPRINTER_INFO_2A;
begin
  // This function does not work correctly
  // I think that SetPrinter function is missing
  // Anyhow - this is rewritten from microsoft web site
  // Check this https://support.microsoft.com/en-us/kb/167345
  MyResult := False;
  MyPDevMode := nil;
  hPrinter := 0;
  hWnd := 0;
   // Start by opening the printer
   FillByte(pd,sizeof(PRINTER_DEFAULTS), 0);
   //#define PRINTER_ALL_ACCESS	(STANDARD_RIGHTS_REQUIRED|PRINTER_ACCESS_ADMINISTER|PRINTER_ACCESS_USE)
   pd.DesiredAccess:=STANDARD_RIGHTS_REQUIRED or 4 or 8;

   if (OpenPrinter(PChar(InDevice), @hPrinter, @pd)) then begin
     // Step 1:
     // Allocate a buffer of the correct size.
     dwNeeded := DocumentProperties(hWnd,
         hPrinter,       // Handle to our printer.
         PChar(InDevice),// Name of the printer.
         nil,            // Asking for size, so
         nil,            // these are not used.
         0);             // Zero returns buffer size.
     GetMem(MyPDevMode, dwNeeded);

     // Step 2:
     // Get the default DevMode for the printer and
     // modify it for your needs.
     dwRet := DocumentProperties(hWnd,
         hPrinter,
         PChar(InDevice),
         MyPDevMode,    // The address of the buffer to fill.
         nil,           // Not using the input buffer.
         DM_OUT_BUFFER); // Have the output buffer filled.
     if (dwRet <> IDOK) then begin
       // If failure, cleanup and return failure.
       ClosePrinter(hPrinter);
       if MyPDevMode <> nil then begin
         freemem(MyPDevMode);
         MyPDevMode := nil;
       end;
       Exit(MyResult);
     end;

     if InIsItDuplex then begin
       if ((MyPDevMode^.dmFields and DM_DUPLEX) <> 0) then begin
         MyPDevMode^.dmDuplex := DMDUP_VERTICAL;  // DMDUP_HORIZONTAL;
         MyPDevMode^.dmFields := DM_DUPLEX;
       end;
     end
     else begin
        if ((MyPDevMode^.dmFields and DM_DUPLEX) <> 0) then begin
          MyPDevMode^.dmDuplex := DMDUP_SIMPLEX;
          MyPDevMode^.dmFields := DM_DUPLEX;
        end;
     end;

     //
     // Step 3:
     // Merge the new settings with the old.
     // This gives the driver an opportunity to update any private
     // portions of the DevMode structure.
     MySet := DM_IN_BUFFER or  // Commands to Merge our changes and
               DM_OUT_BUFFER;
      dwRet := DocumentProperties(hWnd,
         hPrinter,
         PChar(InDevice),
         MyPDevMode,        // Reuse our buffer for output.
         MyPDevMode,       // Pass the driver our changes.
         MySet
         ); // write the result.

     // Finished with the printer
     ClosePrinter(hPrinter);

     if (dwRet <> IDOK) then begin
       // If failure, cleanup and return failure.
       if MyPDevMode <> nil then begin
         Freemem(MyPDevMode, dwNeeded);
         MyPDevMode := nil;
       end;
       Exit(MyResult);
     end;

     // Return the modified DevMode structure.
     if MyPDevMode <> nil then begin
       freemem(MyPDevMode);
       MyPDevMode := nil;
       Result := True;
     end;
   end
   else begin
     Result := False;
   end;
end;

function SetPrinterDuplexGlobally(InPrinterName: String; InIsItDuplex: Boolean; InBroadcastChangeMessage: Boolean): boolean;
var MyHandle:THANDLE;
    MyPrinterDefaults:PRINTER_DEFAULTS;
    MyDWNeeded: DWORD;
    MyPPRINTER_INFO_2A: PPRINTER_INFO_2A;
    MyPDevMode: PDEVMODE;
    MyLastErrorCode: DWORD;
    MyDocumentPropertiesRetCode: LONG;
begin
  MyLastErrorCode := 0;
  result:=false;
  fillchar(MyPrinterDefaults, sizeof(PRINTER_DEFAULTSA), #0);
  //#define PRINTER_ALL_ACCESS	(STANDARD_RIGHTS_REQUIRED|PRINTER_ACCESS_ADMINISTER|PRINTER_ACCESS_USE)
  MyPrinterDefaults.DesiredAccess:=STANDARD_RIGHTS_REQUIRED or 4 or 8;
//  MyPrinterDefaults.DesiredAccess := PRINTER_NORMAL_ACCESS;
  OpenPrinter(pchar(InPrinterName),@MyHandle,@MyPrinterDefaults);
  if (MyHandle = 0) then begin
    // Cannot get printer hanlde
    MyLastErrorCode:=GetLastError;
    Exit;
  end;

  GetPrinter(MyHandle, 2, nil, 0, @MyDWNeeded);
  MyPPRINTER_INFO_2A:=PPRINTER_INFO_2A(GlobalAlloc(GPTR, MyDWNeeded));
  try
    if GetPrinter(MyHandle, 2, pbyte(MyPPRINTER_INFO_2A), MyDWNeeded, @MyDWNeeded) then begin
      if ((MyPPRINTER_INFO_2A^.pDevMode^.dmFields and DM_DUPLEX) <> 0) then begin
        if InIsItDuplex then begin
          MyPPRINTER_INFO_2A^.pDevMode^.dmDuplex := DMDUP_VERTICAL;
          MyPPRINTER_INFO_2A^.pDevMode^.dmFields := DM_DUPLEX;
        end
        else begin
          MyPPRINTER_INFO_2A^.pDevMode^.dmDuplex := DMDUP_SIMPLEX;
          MyPPRINTER_INFO_2A^.pDevMode^.dmFields := DM_DUPLEX;
        end;
        MyPPRINTER_INFO_2A^.pSecurityDescriptor:=nil;
        MyDocumentPropertiesRetCode := DocumentProperties(0, MyHandle, pchar(InPrinterName), MyPPRINTER_INFO_2A^.pDevMode, MyPPRINTER_INFO_2A^.pDevMode, DM_IN_BUFFER or DM_OUT_BUFFER);
        if MyDocumentPropertiesRetCode = IDOK then begin
          if SetPrinter(MyHandle, 2, pbyte(MyPPRINTER_INFO_2A), 0) then begin
            result := true;
            // Broadcast change printer message to all windows application
            if InBroadcastChangeMessage then begin
              SendMessageTimeout(HWND_BROADCAST, WM_DEVMODECHANGE, 0, LPARAM(pchar(InPrinterName)), SMTO_NORMAL, 1000, MyDWNeeded);
            end;
          end
          else begin
            MyLastErrorCode:=GetLastError;
          end;
        end
        else begin
          // Unable to set printer properties
          MyLastErrorCode := -1;
        end;
      end;
    end;
  finally
    GlobalFree(LongWord(MyPPRINTER_INFO_2A));
    ClosePrinter(MyHandle);
  end;
end;

{$ENDIF}

end.

