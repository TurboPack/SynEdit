unit FormMain_ctCode;
interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  SynCompletionProposal, StdCtrls, SynEdit, ComCtrls, System.ImageList,
  Vcl.ImgList, Vcl.VirtualImageList, Vcl.BaseImageCollection,
  Vcl.ImageCollection;
const
  cCaseSensitive = 1;
  cAnsiStrings   = 2;
  cPrettyText    = 3;
  cInsertList    = 4;
  cMatchedText   = 5;
type
  TForm1 = class(TForm)
    scpDemo: TSynCompletionProposal;
    PageControl1: TPageControl;
    CodeCompletion: TTabSheet;
    TabSheet2: TTabSheet;
    mmoInsert: TMemo;
    mmoItem: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    Button2: TButton;
    SynTest: TSynEdit;
    edBiggestWord: TEdit;
    Label3: TLabel;
    cbCase: TCheckBox;
    cbPrettyText: TCheckBox;
    cbUseInsertList: TCheckBox;
    cbLimitToMatchedText: TCheckBox;
    SynEdit1: TSynEdit;
    edTitle: TEdit;
    Label4: TLabel;
    Button3: TButton;
    Button4: TButton;
    FontDialog1: TFontDialog;
    cbShowGripper: TCheckBox;
    cbFormShadow: TCheckBox;
    ImageCollection1: TImageCollection;
    VirtualImageList1: TVirtualImageList;
    procedure FormCreate(Sender: TObject);
    procedure CheckBoxClick(Sender: TObject);
    procedure edBiggestWordChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure edTitleChange(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure cbFormShadowClick(Sender: TObject);
    procedure cbShowGripperClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;
var
  Form1: TForm1;
implementation
{$R *.DFM}
procedure TForm1.FormCreate(Sender: TObject);
begin
  with mmoInsert.Lines do
  begin
    Clear;
    Add('Create');
    Add('Destroy');
    Add('Add');
    Add('ClearLine');
    Add('Delete');
    Add('First');
    Add('GetMarksForLine');
    Add('Insert');
    Add('Last');
    Add('Place');
    Add('Remove');
    Add('WMCaptureChanged');
    Add('WMCopy');
    Add('WMCut');
    Add('WMDropFiles');
    Add('WMEraseBkgnd');
    Add('WMGetDlgCode');
    Add('WMHScroll');
    Add('WMPaste');
  end;
  with mmoItem.Lines do
  begin
    Clear;
    Add('\image{5}\hspace{2}constructor \column{}\style{+B}Create\style{-B}(AOwner: TCustomSynEdit)');
    Add('\image{5}\hspace{2}destructor \column{}\style{+B}Destroy\style{-B}');
    Add('\image{3}\hspace{2}function \column{}\style{+B}Add\style{-B}(Item: TSynEditMark): Integer');
    Add('\image{4}\hspace{2}procedure \column{}\style{+B}ClearLine\style{-B}(line: integer)');
    Add('\image{4}\hspace{2}procedure \column{}\style{+B}Delete\style{-B}(Index: Integer)');
    Add('\image{3}\hspace{2}function \column{}\style{+B}First\style{-B}: TSynEditMark');
    Add('\image{4}\hspace{2}procedure \column{}\style{+B}GetMarksForLine\style{-B}(line: integer; var Marks: TSynEditMarks)');
    Add('\image{4}\hspace{2}procedure \column{}\style{+B}Insert\style{-B}(Index: Integer; Item: TSynEditMark)');
    Add('\image{3}\hspace{2}function \column{}\style{+B}Last\style{-B}: TSynEditMark');
    Add('\image{4}\hspace{2}procedure \column{}\style{+B}Place\style{-B}(mark: TSynEditMark)');
    Add('\image{3}\hspace{2}function \column{}\style{+B}Remove\style{-B}(Item: TSynEditMark): Integer');
    Add('\image{4}\hspace{2}procedure \column{}\style{+B}WMCaptureChanged\style{-B}(var Msg: TMessage); message WM_CAPTURECHANGED');
    Add('\image{4}\hspace{2}procedure \column{}\style{+B}WMCopy\style{-B}(var Message: TMessage); message WM_COPY');
    Add('\image{4}\hspace{2}procedure \column{}\style{+B}WMCut\style{-B}(var Message: TMessage); message WM_CUT');
    Add('\image{4}\hspace{2}procedure \column{}\style{+B}WMDropFiles\style{-B}(var Msg: TMessage); message WM_DROPFILES');
    Add('\image{4}\hspace{2}procedure \column{}\style{+B}WMEraseBkgnd\style{-B}(var Msg: TMessage); message WM_ERASEBKGND');
    Add('\image{4}\hspace{2}procedure \column{}\style{+B}WMGetDlgCode\style{-B}(var Msg: TWMGetDlgCode); message WM_GETDLGCODE');
    Add('\image{4}\hspace{2}procedure \column{}\style{+B}WMHScroll\style{-B}(var Msg: TWMScroll); message WM_HSCROLL');
    Add('\image{4}\hspace{2}procedure \column{}\style{+B}WMPaste\style{-B}(var Message: TMessage); message WM_PASTE');
  end;
  scpDemo.InsertList.AddStrings(mmoInsert.Lines);
  scpDemo.ItemList.AddStrings(mmoItem.Lines);
end;
procedure TForm1.CheckBoxClick(Sender: TObject);
begin
  if Sender is TCheckBox then
  begin
    if TCheckBox(Sender).Checked then
    begin
      Case TCheckBox(Sender).Tag of
        cCaseSensitive : scpDemo.Options := scpDemo.Options + [scoCaseSensitive];
        cPrettyText    : scpDemo.Options := scpDemo.Options + [scoUsePrettyText];
        cInsertList    : scpDemo.Options := scpDemo.Options + [scoUseInsertList];
        cMatchedText   : scpDemo.Options := scpDemo.Options + [scoLimitToMatchedText];
      end;
    end else begin
      Case TCheckBox(Sender).Tag of
        cCaseSensitive : scpDemo.Options := scpDemo.Options - [scoCaseSensitive];
        cPrettyText    : scpDemo.Options := scpDemo.Options - [scoUsePrettyText];
        cInsertList    : scpDemo.Options := scpDemo.Options - [scoUseInsertList];
        cMatchedText   : scpDemo.Options := scpDemo.Options - [scoLimitToMatchedText];
      end;
    end;
  end;
end;
procedure TForm1.edBiggestWordChange(Sender: TObject);
begin
//TODO: set column width based on word length
//  scpDemo.Columns[0].BiggestWord := edBiggestWord.Text;
end;
procedure TForm1.Button1Click(Sender: TObject);
begin
  scpDemo.InsertList.Clear;
  scpDemo.InsertList.AddStrings(mmoInsert.Lines);
end;
procedure TForm1.Button2Click(Sender: TObject);
begin
  scpDemo.ItemList.Clear;
  scpDemo.ItemList.AddStrings(mmoItem.Lines);
  scpDemo.ResetAssignedList;
end;
procedure TForm1.edTitleChange(Sender: TObject);
begin
  scpDemo.Title := edTitle.Text;
end;
procedure TForm1.Button3Click(Sender: TObject);
begin
  FontDialog1.Font.Assign(scpDemo.Font);
  if FontDialog1.Execute then
    scpDemo.Font.Assign(FontDialog1.Font);
end;
procedure TForm1.Button4Click(Sender: TObject);
begin
  FontDialog1.Font.Assign(scpDemo.TitleFont);
  if FontDialog1.Execute then
    scpDemo.TitleFont.Assign(FontDialog1.Font);
end;
procedure TForm1.cbFormShadowClick(Sender: TObject);
begin
  scpDemo.PaintFormShadow := cbFormShadow.Checked;
end;

procedure TForm1.cbShowGripperClick(Sender: TObject);
begin
  scpDemo.ShowGripper := cbShowGripper.Checked;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  SynEdit1.SetFocus;
end;

initialization
  ReportMemoryLeaksOnShutdown := True;
end.
