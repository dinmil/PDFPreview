unit PDFDoc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, BGRAVirtualScreen, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Buttons, ComCtrls, types, BGRABitmap, BCTypes,
  libmupdf18, Printers, ctypes, Math
  {$IF Defined(MSWINDOWS)}
  ,WinPrinterUtils
  {$ENDIF}
  ;

type

  TPrintCallback = procedure (InJobName: String; InCurrentPage, InPageCount: Integer) of object;

  { TPDFDoc }

  TPDFDoc = class
  private
  public
    FFileName: String;
    FCTX: pfz_context;
    FDoc: pfz_document;
    FPage: pfz_page;
    FPageNum: Integer;

    FContainerWidth: Integer;
    FContainerHeight: Integer;

    FPrintCallback: TPrintCallback;

    constructor Create;
    destructor Destroy; override;

    procedure FreeContext;
    procedure FreeDocument;
    procedure FreePage;
    procedure FreeAll;

    procedure OpenDocument(InFileName: String);
    function  PrintPages(InPrintJobName: String; InPrinterName: String; InPrinterPages: String; InPrintDuplex: Boolean): Integer;
    function  GetPageCount: Integer;
    function  ShouldIPrintPage(InPageNumber: Integer; InPrintPages: String): Boolean;

    function LoadPage(InPageNum: Integer): Integer;

    procedure RedrawBitmap(InBitmap: TBGRABitmap);
    procedure ResizeBitmap(InVirtualScreen: TBGRAVirtualScreen; InZoom: Double;
      InHScrollBar, InVScrollBar: TScrollBar);
    procedure CenterPreview(InVirtualScreen: TBGRAVirtualScreen; InParentHolder: TPanel);
    procedure WritePreviewStatus(InLabel: TLabel);


    procedure SetContainerWidthAndHeight(InWidth, InHeight: Integer);
    function  IsPrinterSetInDuplexMode(InPrinterName: String): Boolean;
    procedure SetPrinterDuplex(InPrinterName: String; InDuplex: Boolean);
    function  CalculateZoom(InWidth, InHeight, InPageNo: Integer): Double;
  end;

procedure AssignListOfPrinters(InComboBox: TComboBox; InDefaultPrinter: String);

implementation

procedure AssignListOfPrinters(InComboBox: TComboBox;
  InDefaultPrinter: String);
var MyItemIndex: Integer;
begin
  Printer.Refresh;
  InComboBox.Items.Assign(Printer.Printers);
  if InDefaultPrinter = '' then begin
    InDefaultPrinter := Printer.PrinterName;
  end;
  MyItemIndex := InComboBox.Items.IndexOf(InDefaultPrinter);
  InComboBox.ItemIndex := MyItemIndex;
end;

{ TPDFDoc }

constructor TPDFDoc.Create;
begin
  FCTX := nil;
  FDoc := nil;
  FPage := nil;
  FPageNum := -1;
end;

destructor TPDFDoc.Destroy;
begin
  FreeAll;
  FPrintCallback := nil;
  inherited Destroy;
end;

procedure TPDFDoc.FreeContext;
begin
  if FCTX <> nil then begin
    try
      fz_drop_context_my(FCTX);
    except
    end;
    FCTX := nil;
  end;
end;

procedure TPDFDoc.FreeDocument;
begin
  if FDoc <> nil then begin
    try
      fz_drop_document(FCTX, FDoc);
    finally
    end;
  end;
end;

procedure TPDFDoc.FreePage;
begin
  if (FPage <> nil) and (FDoc <> nil) and (FCTX <> nil) then begin
    FPageNum := -1;
    try
      fz_drop_page(FCTX, FPage);
    except
    end;
    FPage := nil;
  end;
end;

procedure TPDFDoc.FreeAll;
begin
  FreePage;
  FreeDocument;
  FreeContext;
end;

procedure TPDFDoc.OpenDocument(InFileName: String);
begin
  FFileName := InFileName;

  FreeAll;

  if FileExists(InFileName) then begin
    // Create context
    FCTX := fz_new_context(nil, nil, FZ_STORE_UNLIMITED);

    if FCTX <> nil then begin
      // Create document
      FDoc := fz_open_document(FCTX, PChar(InFileName));
      if FDoc = nil then begin
        raise Exception.Create('Unable to open pdf document');
      end;
    end
    else begin
      raise Exception.Create('Unable to create context. Maybe version is wrong. Expected version is "' + FZ_VERSION + '"');
    end;
  end
  else begin
    raise Exception.Create('File "'  + InFileName + '" does not exists');
  end;
end;

function TPDFDoc.PrintPages(InPrintJobName: String; InPrinterName: String;
  InPrinterPages: String; InPrintDuplex: Boolean): Integer;
var MyPageCount: Integer;
    F: Integer;
    MyPrintedPageCount: Integer;
    MyBitmap: TBGRABitmap;
    MyWidth, MyHeight: Integer;

    MyFzPage: pfz_page;
    MyFZRectangle1, MyFZRectangle2: fz_rect;
    MyFZMatrix: fz_matrix;
    MyFzColorSpace: pfz_colorspace;
    MyFzPixmap: pfz_pixmap;
    MyFzDevice: pfz_device;
    MyFzBox   : fz_irect;
    MyFzCtmMatrix: fz_matrix;


//    MyPrintDensity: Integer;
//    MyRect: TRect;

    MyPrinter: TPrinter;

    transform: fz_matrix;
    my100: cfloat;

    MyFzRect: fz_rect;
    MyInt1, MyInt2: cint;

    MyWorkRect, MyPhysicalRect: TRect;

    MyRotation: Integer;

    MyPrinterPageWidth, MyPrinterPageHeight: Integer;
    MyPrintDensityWidth, MyPrintDensityheight: Double;

    MyIsPrinterSetInDuplexMode: Boolean;
    MyShouldIChangeDuplexMode: Boolean;

begin
  MyBitmap := nil;
  MyFzPage := nil;
  MyFzDevice := nil;
  MyFzPixmap := nil;
  MyFzColorSpace := nil;
  MyRotation := 0;
  MyPrinterPageWidth := 0;
  MyPrinterPageheight := 0;

  MyPrintedPageCount := 0;
  MyPageCount := GetPageCount;

  MyPrinter := Printer;
  MyPrinter.Refresh;

  MyShouldIChangeDuplexMode := False;

  {$IF Defined(MSWINDOWS)}
    MyIsPrinterSetInDuplexMode := IsPrinterSetInDuplexMode(InPrinterName);

    // This flag will tell me sould I chane printer mode
    if MyIsPrinterSetInDuplexMode then begin
      if InPrintDuplex = False then begin
        MyShouldIChangeDuplexMode := True;
      end;
    end
    else begin
      if InPrintDuplex = True then begin
        MyShouldIChangeDuplexMode := True;
      end;
    end;

    if MyShouldIChangeDuplexMode then begin
      SetPrinterDuplex(InPrinterName, InPrintDuplex);
    end;
  {$ENDIF}

  if InPrinterName <> '' then begin
    // If I do not change printer then settings stay the same
    try
      MyPrinter.SetPrinter('*');
    except
    end;
    MyPrinter.SetPrinter(InPrinterName);
  end;

  MyPrinter.Title := InPrintJobName;
  MyWorkRect := MyPrinter.PaperSize.PaperRect.WorkRect;
  MyPhysicalRect := MyPrinter.PaperSize.PaperRect.PhysicalRect;
  MyPrinter.BeginDoc;


  for F := 0 to MyPageCount-1 do begin
    if ShouldIPrintPage(F+1, InPrinterPages) then begin
      try
        if MyPrintedPageCount > 0 then begin
          MyPrinter.NewPage;
        end;
        // Load page
        MyFzPage := fz_load_page(FCTX, FDoc, F);

      	// Calculate a transform to use when rendering. This transform
      	// contains the scale and rotation. Convert zoom percentage to a
      	// scaling factor. Without scaling the resolution is 72 dpi.

(*
        ShowMessage(Format('SizeOf: %d\n%8.2f\n%8.2f\n', [SizeOf(MyFzRect), MyFzRect.x0, MyFzRect.x1]));

        MyInt1 := 34;
        MyInt2 := 52;
        MyFzRect.x0 := 1.1;
        MyFzRect.x1 := 2.2;
        MyFzRect.y0 := 3.3;
        MyFzRect.y1 := 4.4;
//        MyFzRect := fz_dinko(MyInt1, MyInt2, MyFzRect);
        fz_dinko(MyInt1, MyInt2, MyFzRect);
        ShowMessage(IntToStr(MyInt2));

        ShowMessage(Format('SizeOf: %d\n%8.2f\n%8.2f\n', [SizeOf(MyFzRect), MyFzRect.x0, MyFzRect.x1]));
        Exit;
*)
        transform := Init_fz_matrix;


      	fz_rotate(transform, MyRotation);
        my100 := 100/100;
      	fz_pre_scale(transform, 1, 1);
        (*
        transform.a := transform.a * 100;
        transform.b := transform.b * 100;
        transform.c := transform.c * 100;
        transform.d := transform.d * 100;
        *)

        // Get dimesions of page - resolution is 72 dots per inch
        MyFZRectangle1 := Init_fz_rectangle;
        fz_bound_page(FCTX, MyFzPage, MyFZRectangle1);
        fz_transform_rect(MyFZRectangle1, transform);

        // MyFZRectangle1.x1 = 595.276 points = 595 / 72 = 8.26772 inch * 2,54 = 21 cm;
        // MyFZRectangle1.y1 = 841.890 points = 841 / 72 = 11.6929 inch * 2.54 = 29.7 cm;
                      (*
        MyFZRectangle1.x0 := 0;
        MyFZRectangle1.y0 := 0;
        MyFZRectangle1.x1 := 595.276;
        MyFZRectangle1.y1 := 841.890;
        *)

        // Get width and height of printer paper
//        MyPrintDensity := 600; // 96 is normal, 300 dots per inch, 600

        MyPrinterPageWidth:=MyPhysicalRect.Right - MyPhysicalRect.Left;
        MyPrinterPageHeight:=MyPhysicalRect.Bottom - MyPhysicalRect.Top;

        if ((MyFZRectangle1.x1 - MyFZRectangle1.x0) > 0) then begin
          MyPrintDensityWidth := 72 * (MyPrinterPageWidth / (MyFZRectangle1.x1 - MyFZRectangle1.x0));
        end
        else begin
          MyPrintDensityHeight := 72;
        end;
        if ((MyFZRectangle1.y1 - MyFZRectangle1.y0) > 0) then begin
          MyPrintDensityHeight := 72 * (MyPrinterPageHeight / (MyFZRectangle1.y1 - MyFZRectangle1.y0))
        end
        else begin
          MyPrintDensityHeight := 72;
        end;


        //MyPrintDensityWidth := MyPrinter.XDPI;
        // MyPrintDensityHeight := MyPrinter.YDPI;

        MyWidth  := round((MyFZRectangle1.x1 - MyFZRectangle1.x0) * MyPrintDensityWidth / 72);
        MyHeight := round((MyFZRectangle1.y1 - MyFZRectangle1.y0) * MyPrintDensityheight / 72);
        // for xdpi = 600 and ydpi = 600 result is MyWidth = 4961, MyHeight = 7016
        // for xdpi = 300 and ydpi = 300 result is MyWidth = 2480, MyHeight = 3507

        //MyWidth  := MyPrinterPageWidth;
        //MyHeight := MyPrinterPageHeight;

        // Create bitmap which will hold pdf picture
        if MyBitmap = nil then begin
          MyBitmap := TBGRABitmap.Create(MyWidth, MyHeight, clWhite);
        end;

        // Find colorspace routine
        MyFzColorSpace := fz_lookup_device_colorspace(FCTX, Pchar('DeviceBGR'));

        with MyFzBox do begin
           x0:=0; y0:=0; x1:=MyBitmap.Width; y1:=MyBitmap.Height;
         end;
         with MyFzCtmMatrix do begin
           a:=MyPrintDensityWidth/72; b:=0; c:=0; d:=MyPrintDensityheight/72; e:=0; f:=0;
         end;

        // Create new box
        MyFzPixmap := fz_new_pixmap_with_bbox_and_data(FCTX, MyFzColorSpace, MyFzBox, MyBitmap.Data);

        // create new device
        MyFzDevice := fz_new_draw_device(FCTX, MyFzPixmap);

        // Print page
        fz_run_page(FCTX, MyFzPage, MyFzDevice, MyFzCtmMatrix, nil);

        // Flip the page for Windows
        {$IF Defined(MSWINDOWS)}
        MyBitmap.VerticalFlip;
        {$ENDIF}

        // Copy bitmap to printer canvas
        // w = 4960, h = 7015 for 600 dpi
//        MyBitmap.SaveToFile('PrintBitmap' + IntToStr(F) + '.png');

{$IF Defined(MSWINDOWS)}
//       Commented line works on windows but it make problems on linux
//       MyBitmap.Draw(MyPrinter.Canvas, -MyWorkRect.Left, -MyWorkRect.Top, True);
       MyPrinter.Canvas.Draw(-MyWorkRect.Left, -MyWorkRect.Top, MyBitmap.Bitmap);
{$ENDIF}
{$IF Defined(UNIX)}
       MyPrinter.Canvas.Draw(0, 0, MyBitmap.Bitmap);
{$ENDIF}


        // Increment Printed page count
        MyPrintedPageCount := MyPrintedPageCount + 1;

        if Assigned(FPrintCallback) then begin
          FPrintCallback(InPrintJobName, MyPrintedPageCount, MyPageCount);
        end;

      finally
        if MyFzPage <> nil then begin
          fz_drop_page(FCTX, MyFzPage);
          MyFzPage := nil;
        end;
        if MyFzDevice <> nil then begin
          fz_drop_device(FCTX, MyFzDevice);
          MyFzDevice := nil;
        end;
        if MyFzPixmap <> nil then begin
          fz_drop_pixmap(FCTX, MyFzPixmap);
          MyFzPixmap := nil;
        end;
        if MyBitmap <> nil then begin
          FreeAndNil(MyBitmap);
        end;
      end;
    end;
  end;

  if MyPrintedPageCount > 0 then begin
    MyPrinter.EndDoc;
  end
  else begin
    MyPrinter.Abort;
    MyPrintedPageCount := 0;
  end;

  // This flag will tell me sould I chane printer mode
  // Return printer to current state
  if MyShouldIChangeDuplexMode then begin
    SetPrinterDuplex(InPrinterName, Not(InPrintDuplex));
  end;

  Result := MyPrintedPageCount;
end;

function TPDFDoc.GetPageCount: Integer;
var MyResult: Integer;
begin
  MyResult := 0;
  if FDoc <> nil then begin
    MyResult := fz_count_pages(FCTX, FDoc);
  end
  else begin
    raise Exception.Create('PDF document is not initialized')
  end;
  Result := MyResult;
end;

function TPDFDoc.ShouldIPrintPage(InPageNumber: Integer; InPrintPages: String
  ): Boolean;
var MyTempString: String;
    MyTempString1: String;
    MyResult: Boolean;
    MyStrings1, MyStrings2: TStrings;
    MyVal1, MyVal2: Integer;
    F: Integer;
begin
  MyResult := True;
  MyStrings1 := nil;
  MyStrings2 := nil;

  // result is true - empty string means all pages are printed
  if InPrintPages <> '' then begin
    MyResult := False;
    // string contain pages separated by , or ;
    // string can contain range of pages
    // example 1,2,3,4-10,11-
    // first resplace all ; with ,
    try
      MyStrings1 := TStringList.Create;
      MyStrings2 := TStringList.Create;
      MyStrings1.Delimiter := ',';
      MyStrings2.Delimiter := '-';

      MyTempString := InPrintPages;
      MyTempString := StringReplace(InPrintPages, ';', ',', [rfReplaceAll]);
      MyStrings1.DelimitedText := MyTempString;
      for F := 0 to MyStrings1.Count-1 do begin
        MyTempString1 := MyStrings1.Strings[F];
        MyStrings2.DelimitedText := MyTempString1;
        MyVal1 := StrToIntDef(MyStrings2.Strings[0], 0);
        MyVal2 := MyVal1;
        if MyStrings2.Count > 1 then begin
          MyVal2 := StrToIntDef(MyStrings2.Strings[1], MaxInt);
        end;
        if (InPageNumber >= MyVal1) and (InPageNumber <= MyVal2) then begin
          MyResult := True;
          Break;
        end
      end;
    finally
      if MyStrings1 <> nil then FreeAndNil(MyStrings1);
      if MyStrings2 <> nil then FreeAndNil(MyStrings2);
    end;
  end;
  Result := MyResult;
end;

procedure TPDFDoc.RedrawBitmap(InBitmap: TBGRABitmap);
var MyPageCount: Integer;

      MyBoxRect: fz_irect;

      MyMatrix: fz_matrix;
      MyPixmap:pfz_pixmap;
      MyDevicedev: pfz_device;

      MyOldCursor: TCursor;

      MyFZRectPage: fz_rect;
begin
  if (FCTX <> nil) and (FDoc <> nil) and (FPage <> nil) then begin
    MyPageCount := GetPageCount;
    if (FPageNum >= 0) and (FPageNum < MyPageCount) then begin
      try
        MyOldCursor:=Screen.Cursor;
        Screen.Cursor:=crHourGlass;

        with MyBoxRect do
        begin
          x0:=0; y0:=0; x1:=InBitmap.Width; y1:=InBitmap.Height;
        end;

        // detect multiplicator
        fz_bound_page(FCTX, FPage, MyFZRectPage);

        with MyMatrix do
        begin
          if ((MyFZRectPage.x1 - MyFZRectPage.x0) > 0) and ((MyFZRectPage.y1 - MyFZRectPage.y0) > 0) then begin
            a:=((InBitmap.Width / (MyFZRectPage.x1 - MyFZRectPage.x0)));
            b:=0;
            c:=0;
            d:=((InBitmap.Height / (MyFZRectPage.y1 - MyFZRectPage.y0)));
            e:=0;
            f:=0;
          end
          else begin
            a:=(96/72);
            b:=0;
            c:=0;
            d:=(96/72);
            e:=0;
            f:=0;
          end;
        end;


        MyPixmap:=fz_new_pixmap_with_bbox_and_data(FCTX, fz_lookup_device_colorspace(FCTX,'DeviceBGR'), MyBoxRect, InBitmap.Data);
        MyDevicedev:=fz_new_draw_device(FCTX, MyPixmap);
        fz_run_page(FCTX, FPage, MyDevicedev, MyMatrix, nil);

        {$IF Defined(MSWINDOWS)}
        InBitmap.VerticalFlip;
        {$ENDIF}

      finally
        fz_drop_device(FCTX, MyDevicedev);
        fz_drop_pixmap(FCTX, MyPixmap);
        Screen.Cursor:=MyOldCursor;
      end;
    end;
  end;
end;

procedure TPDFDoc.ResizeBitmap(InVirtualScreen: TBGRAVirtualScreen;
  InZoom: Double; InHScrollBar, InVScrollBar: TScrollBar);
var  MyFzRect1, MyFzRect2: fz_rect;
     MyFzMatrix: fz_matrix;
     MyWidth, MyHeight: Integer;
begin
  if (FPageNum > -1) and (FDoc <> nil) and (FPage <> nil) then begin
    // Page Exists
    fz_bound_page(FCTX, FPage, MyFzRect1);
    // MyFzRect1.x = 0; MyFzRect1.y = 0; MyFzRect1.x1 := 595.276001; MyFzRect1.y1 = 841.890015;
    with MyFzMatrix do
    begin
      a:=96/72; b:=0; c:=0; d:=96/72; e:=0; f:=0;
    end;

    fz_transform_rect(MyFzRect1, MyFzMatrix);
    MyWidth:=round(((MyFzRect1.x1-MyFzRect1.x0)*96/72) * InZoom);
    MyHeight:=round(((MyFzRect1.y1-MyFzRect1.y0)*96/72) * InZoom);

    // InVirtualScreen.SetPageSize(MyWidth,MyHeight);
    InVirtualScreen.Width := MyWidth;
    InVirtualScreen.Height := MyHeight;
    InVirtualScreen.RedrawBitmap;

    // InVirtualScreen.Caption := IntToSTr(FPageNum);


    InHScrollBar.PageSize:=1;
    InHScrollBar.Min:=0;
    if (MyWidth - FContainerWidth) > 0 then begin;
      InHScrollBar.Max := Round(MyWidth - FContainerWidth);
    end
    else begin
      InHScrollBar.Max := 0
    end;
    InHScrollBar.SmallChange:=1;
    InHScrollBar.LargeChange:=MyWidth div 4;
    InHScrollBar.Position:=0;


    InVScrollBar.PageSize:=1;
    InVScrollBar.Min:=0;
    if (MyHeight - FContainerHeight) > 0 then begin;
      InVScrollBar.Max := Round(MyHeight - FContainerHeight);
    end
    else begin
      InVScrollBar.Max := 0;
    end;
    InVScrollBar.SmallChange:=1;
    InVScrollBar.LargeChange:=MyHeight div 4;
    InVScrollBar.Position:=0;
  end;
end;

procedure TPDFDoc.CenterPreview(InVirtualScreen: TBGRAVirtualScreen;
  InParentHolder: TPanel);
begin
  if InParentHolder <> nil then begin
    if InParentHolder.ClientWidth > InVirtualScreen.Width then begin
      InVirtualScreen.Left := (InParentHolder.ClientWidth - InVirtualScreen.Width) div 2;
    end
    else begin
      InVirtualScreen.Left := 0;
    end;

    if InParentHolder.ClientHeight > InVirtualScreen.Height then begin
      InVirtualScreen.Top := (InParentHolder.ClientHeight - InVirtualScreen.Height) div 2;
    end
    else begin
      InVirtualScreen.Top := 0;
    end;
  end;
end;

procedure TPDFDoc.WritePreviewStatus(InLabel: TLabel);
var MyString: String;
begin
  if InLabel <> nil then begin
    MyString := '';
    if FFileName <> '' then begin
      if MyString <> '' then MyString := MyString + #13#10;
      MyString := MyString + FFileName;
    end;
    if FPageNum <> -1 then begin
      if MyString <> '' then MyString := MyString + #13#10;
      MyString := MyString + Format('%d/%d', [FPageNum+1, GetPageCount])
    end;
    InLabel.Caption := MyString;
  end;
end;

function TPDFDoc.LoadPage(InPageNum: Integer): Integer;
begin
  if FDoc <> nil then begin
    FPageNum := -1;
    if FPage <> nil then begin
      // free old pge
      try
        fz_drop_page(FCTX, FPage);
      except
      end;
    end;
    if (InPageNum >= 0) and (InPageNum < GetPageCount) then begin
      try
        FPage := fz_load_page(FCTX, FDoc, InPageNum);
      except
        raise
      end;
      FPageNum := InPageNum;
    end;
  end
  else begin
    FPageNum := -1;
  end;
  Result := FPageNum;
end;

procedure TPDFDoc.SetContainerWidthAndHeight(InWidth, InHeight: Integer);
begin
  FContainerWidth := InWidth;
  FContainerHeight := InHeight;
end;

function TPDFDoc.IsPrinterSetInDuplexMode(InPrinterName: String): Boolean;
begin
  Result := False;
  {$IF Defined(MSWINDOWS)}
  Result := WinPrinterUtils.GetPrinterDuplexParam(InPrinterName);
//  printersettings.SetPrinterDuplexObsolete(Application.MainForm.Handle, InPrinterName, InDuplex);
  {$ENDIF}
end;

procedure TPDFDoc.SetPrinterDuplex(InPrinterName: String; InDuplex: Boolean);
begin
  {$IF Defined(MSWINDOWS)}
  WinPrinterUtils.SetPrinterDuplexGlobally(InPrinterName, InDuplex, False);
//  printersettings.SetPrinterDuplexObsolete(Application.MainForm.Handle, InPrinterName, InDuplex);
  {$ENDIF}
end;

function TPDFDoc.CalculateZoom(InWidth, InHeight, InPageNo: Integer): Double;
var MyFzPage: pfz_page;
    MyFZRectangle1, MyFZRectangle2: fz_rect;
    MyFZMatrix: fz_matrix;
    MyFzColorSpace: pfz_colorspace;
    MyFzPixmap: pfz_pixmap;
    MyFzDevice: pfz_device;
    MyFzBox   : fz_irect;
    MyFzCtmMatrix: fz_matrix;


//    MyPrintDensity: Integer;
//    MyRect: TRect;

    MyPrinter: TPrinter;

    transform: fz_matrix;
    my100: cfloat;

    MyFzRect: fz_rect;
    MyInt1, MyInt2: cint;

    MyWorkRect, MyPhysicalRect: TRect;

    MyRotation: Integer;

    MyDensityWidth, MyDensityHeight: Double;

begin
  MyFzPage := nil;
  MyDensityHeight := 100;
  MyDensityWidth := 100;
  if (FCTX <> nil) and (FDoc <> nil) and (InPageNo >= 0) and (InPageNo < GetPageCount) then begin
    // Load page
    try
      MyFzPage := fz_load_page(FCTX, FDoc, InPageNo);
      transform := Init_fz_matrix;

	    fz_rotate(transform, MyRotation);
      my100 := 100/100;
	    fz_pre_scale(transform, 1, 1);

      // Get dimesions of page - resolution is 72 dots per inch
      MyFZRectangle1 := Init_fz_rectangle;
      fz_bound_page(FCTX, MyFzPage, MyFZRectangle1);
      fz_transform_rect(MyFZRectangle1, transform);

      if ((MyFZRectangle1.x1 - MyFZRectangle1.x0) > 0) then begin
        MyDensityWidth := 72 * (InWidth / (MyFZRectangle1.x1 - MyFZRectangle1.x0));
      end
      else begin
        MyDensityHeight := 72;
      end;
      if ((MyFZRectangle1.y1 - MyFZRectangle1.y0) > 0) then begin
        MyDensityHeight := 72 * (InHeight / (MyFZRectangle1.y1 - MyFZRectangle1.y0))
      end
      else begin
        MyDensityHeight := 72;
      end;

    finally
      if MyFzPage <> nil then begin
        fz_drop_page(FCTX, MyFzPage);
      end;
    end;
  end;

  if MyDensityHeight < MyDensityWidth then Result := MyDensityHeight / 100
  else Result := MyDensityWidth / 100;

  if Result < 0 then Result := 1;
end;


end.

