unit frmMain;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  SynEdit,
  SynMarkdownViewer, Vcl.StdCtrls, Vcl.ComCtrls;

type
  // Interposer class
  TSynEdit = class(SynMarkdownViewer.TSynMarkdownViewer);

  TForm1 = class(TForm)
    SynEdit: TSynEdit;
    btnRender: TButton;
    reMarkdown: TRichEdit;
    procedure btnRenderClick(Sender: TObject);
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
  System.IOUtils,
  System.RegularExpressions;

procedure TForm1.btnRenderClick(Sender: TObject);
begin
  SynEdit.Markdown := reMarkdown.Lines.Text;
end;

initialization
  ReportMemoryLeaksOnShutdown := True;
end.
