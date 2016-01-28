unit FrmMainPDFPreview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, BGRAVirtualScreen, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Buttons, ComCtrls, types, BGRABitmap, BCTypes,
  Printers, PDFDoc, LCLType, LMessages, LCLIntf;

const
  LM_USER_OPEN_AT_START = LM_USER + 1001;


type

  { TfMainPdfPreview }

  TfMainPdfPreview = class(TForm)
    cbChoosePrinter: TComboBox;
    cbZoom: TComboBox;
    edPrintPages: TEdit;
    HScrollBar: TScrollBar;
    lblPreviewStatus: TLabel;
    lblPageCount: TLabel;
    lblPreviewStatus2: TLabel;
    lblPreviewStatus3: TLabel;
    lblPreviewStatus4: TLabel;
    odChoosePDF: TOpenDialog;
    Panel1: TPanel;
    pnlMenu: TPanel;
    pnlPreviewStatus: TPanel;
    cbDuplexPrinting: TCheckBox;
    sbFirst: TSpeedButton;
    sbLast: TSpeedButton;
    sbNext: TSpeedButton;
    sbOpenPDF: TSpeedButton;
    sbPrint: TSpeedButton;
    sbPrior: TSpeedButton;
    vsPDF: TBGRAVirtualScreen;
    pnlHolder: TPanel;
    VScrollBar: TScrollBar;
    procedure cbChoosePrinterEnter(Sender: TObject);
    procedure cbDuplexPrintingClick(Sender: TObject);
    procedure cbZoomChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure pnlHolderMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure sbFirstClick(Sender: TObject);
    procedure sbLastClick(Sender: TObject);
    procedure sbNextClick(Sender: TObject);
    procedure sbOpenPDFClick(Sender: TObject);
    procedure sbPrintClick(Sender: TObject);
    procedure sbPriorClick(Sender: TObject);
    procedure vsPDFMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure vsPDFRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure HScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure ToolButton4Click(Sender: TObject);
    procedure VScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
  public
    { public declarations }
    _PDFDoc: TPDFDoc;
    _Zoom: Double;
    procedure GotoPage(InPageNumber: Integer);
    procedure PrintCallback(InJobName: String; InCurrentPage, InPageCount: Integer);

    procedure OpenAtStart(var Msg: TLMessage); message LM_USER_OPEN_AT_START;

  end;

var
  fMainPdfPreview: TfMainPdfPreview;

implementation

{$R *.lfm}


{ TfMainPdfPreview }

procedure TfMainPdfPreview.vsPDFRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  if _PDFDoc <> nil then begin
    _PDFDoc.SetContainerWidthAndHeight(pnlHolder.Width, pnlHolder.Height);
    _PDFDoc.RedrawBitmap(Bitmap);
  end;
end;

procedure TfMainPdfPreview.HScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
 vsPDF.SetBounds(-ScrollPos, vsPDF.Top, vsPDF.Width, vsPDF.Height);
end;

procedure TfMainPdfPreview.GotoPage(InPageNumber: Integer);
begin
  if _PDFDoc <> nil then begin
    _PDFDoc.SetContainerWidthAndHeight(pnlHolder.Width, pnlHolder.Height);
    if _PDFDoc.LoadPage(InPageNumber) > -1 then begin
      _PDFDoc.ResizeBitmap(vsPDF, _Zoom, HScrollBar, VScrollBar);
      _PDFDoc.CenterPreview(vsPDF, pnlHolder);
      _PDFDoc.WritePreviewStatus(lblPreviewStatus);
      lblPageCount.Caption := Format('%d/%d', [_PDFDoc.FPageNum+1, _PDFDoc.GetPageCount]);
    end;
  end;
end;

procedure TfMainPdfPreview.PrintCallback(InJobName: String; InCurrentPage,
  InPageCount: Integer);
begin
  lblPreviewStatus.Caption := Format('%s%s%d/%d', [InJobName, #13#10, InCurrentPage, InPageCount]);
end;

procedure TfMainPdfPreview.OpenAtStart(var Msg: TLMessage);
begin
  if FileExists('PDFPreview.pdf') then begin
    _PDFDoc := TPDFDoc.Create;
    _PDFDoc.FPrintCallback := @PrintCallback;
    _PDFDoc.OpenDocument('PDFPreview.pdf');
    _Zoom := _PDFDoc.CalculateZoom(pnlHolder.ClientWidth-100, pnlHolder.ClientHeight-100, 0);
    GotoPage(0);
  end;
end;


procedure TfMainPdfPreview.ToolButton4Click(Sender: TObject);
begin
end;

procedure TfMainPdfPreview.VScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
// VRuler.ZeroOffset:=-ScrollPos;
  vsPDF.SetBounds(vsPDF.Left, -ScrollPos, vsPDF.Width, vsPDF.Height);
end;


procedure TfMainPdfPreview.FormCreate(Sender: TObject);
begin
  {$IF Defined(MSWINDOWS)}
  // set the size of controls
  cbChoosePrinter.Font.Height := 34;
  cbZoom.Font.Height := 34;
  edPrintPages.Font.Height := 34;
  {$ENDIF}
  {$IF Defined(UNIX)}
  // set the size of controls
  cbChoosePrinter.Font.Height := 14;
  cbZoom.Font.Height := 14;
  edPrintPages.Font.Height := 14;
  {$ENDIF}

 AssignListOfPrinters(cbChoosePrinter, cbChoosePrinter.Text);
 PostMessage(Self.Handle, LM_USER_OPEN_AT_START, 0,0);
end;

procedure TfMainPdfPreview.cbZoomChange(Sender: TObject);
begin
  _Zoom := StrToIntDef(cbZoom.Text, -1);
  if _Zoom <> -1 then begin
    _Zoom := _Zoom /100;
  end
  else begin
    // Calculate Zoom
    if _PDFDoc <> nil then begin
      _Zoom := _PDFDoc.CalculateZoom(pnlHolder.ClientWidth-100, pnlHolder.ClientHeight-100, _PDFDoc.FPageNum);
    end;
  end;

  if _PDFDoc <> nil then begin
    GotoPage(_PDFDoc.FPageNum);
//    vsPDF.SetBounds(-VScrollBar.Position, -HScrollBar.Position, vsPDF.Width, vsPDF.Height);
//    vsPDF.SetBounds(0, 0, vsPDF.Width, vsPDF.Height);
  end;
end;

procedure TfMainPdfPreview.cbChoosePrinterEnter(Sender: TObject);
begin
  AssignListOfPrinters(cbChoosePrinter, cbChoosePrinter.Text);
end;

procedure TfMainPdfPreview.cbDuplexPrintingClick(Sender: TObject);
begin
  {$IF Defined(UNIX)}
    ShowMessage('Duplex printing on linux is not solved. Define one Duplex Printer and One Simplex Printer and change the printer in combobox');
  {$IFEND}
end;

procedure TfMainPdfPreview.FormDestroy(Sender: TObject);
begin
  if _PDFDoc <> nil then FreeAndNil(_PDFDoc);
end;

procedure TfMainPdfPreview.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Shift = [ssAlt] then begin
    if Key = VK_PRIOR then begin
      Key := 0;
      sbPrior.Click;
    end
    else if Key = VK_NEXT then begin
      Key := 0;
      sbNext.Click;
    end
    else if Key = VK_END then begin
      Key := 0;
      sbLast.Click;
    end
    else if Key = VK_HOME then begin
      Key := 0;
      sbFirst.Click;
    end
    else if Key = VK_O then begin
      Key := 0;
      sbOpenPDF.Click;
    end
    else if Key = VK_P then begin
      Key := 0;
      sbPrint.Click;
    end
  end;
end;


procedure TfMainPdfPreview.FormResize(Sender: TObject);
begin
  if _PDFDoc <> nil then begin
    if cbZoom.ItemIndex = 0 then begin
      cbZoomChange(Self);
    end;
    _PDFDoc.CenterPreview(vsPDF, pnlHolder);
  end;
end;

procedure TfMainPdfPreview.pnlHolderMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if _PDFDoc <> nil then begin
    VScrollBar.Position := VScrollBar.Position - WheelDelta;
    vsPDF.SetBounds(vsPDF.Left, -VScrollBar.Position, vsPDF.Width, vsPDF.Height);
  end;
end;

procedure TfMainPdfPreview.sbFirstClick(Sender: TObject);
begin
  if _PDFDoc <> nil then begin
    if ((_PDFDoc.GetPageCount) > 0) then begin
      GotoPage(0);
    end;
  end;
end;

procedure TfMainPdfPreview.sbLastClick(Sender: TObject);
begin
  if _PDFDoc <> nil then begin
    if (_PDFDoc.GetPageCount > 0) then begin
      GotoPage(_PDFDoc.GetPageCount-1);
    end;
  end;
end;

procedure TfMainPdfPreview.sbNextClick(Sender: TObject);
begin
  if _PDFDoc <> nil then begin
    if ((_PDFDoc.FPageNum+1) < _PDFDoc.GetPageCount) then begin
      GotoPage(_PDFDoc.FPageNum+1);
    end;
  end;
end;

procedure TfMainPdfPreview.sbOpenPDFClick(Sender: TObject);
begin
  if odChoosePDF.Execute then begin
    if _PDFDoc <> nil then begin
      FreeAndNil(_PDFDoc)
    end;
    if FileExists(odChoosePDF.FileName) then begin
      _PDFDoc := TPDFDoc.Create;
      _PDFDoc.FPrintCallback := @PrintCallback;
      _PDFDoc.OpenDocument(odChoosePDF.FileName);
      GotoPage(0);
    end;
  end;
end;

procedure TfMainPdfPreview.sbPrintClick(Sender: TObject);
var MyNumberOfPrinterPages: Integer;
begin
  if _PDFDoc <> nil then begin
    if MessageDlg('Confirm Printing', 'Do you really want to print document?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
      MyNumberOfPrinterPages := _PDFDoc.PrintPages('TestJob', cbChoosePrinter.Text, edPrintPages.Text, cbDuplexPrinting.Checked);
      ShowMessage('Printer pages count: ' + IntToStr(MyNumberOfPrinterPages));
    end;
  end;
end;

procedure TfMainPdfPreview.sbPriorClick(Sender: TObject);
begin
  if _PDFDoc <> nil then begin
    if ((_PDFDoc.FPageNum-1) >= 0) then begin
      GotoPage(_PDFDoc.FPageNum-1);
    end;
  end;
end;

procedure TfMainPdfPreview.vsPDFMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if _PDFDoc <> nil then begin
    VScrollBar.Position := VScrollBar.Position - WheelDelta;
    vsPDF.SetBounds(vsPDF.Left, -VScrollBar.Position, vsPDF.Width, vsPDF.Height);
  end;
end;

end.

