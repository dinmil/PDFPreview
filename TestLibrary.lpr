program TestLibrary;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, libmupdf18
  { you can add units after this };

begin
  fz_new_context_imp(nil, nil, 0, PChar(FZ_VERSION))
end.

