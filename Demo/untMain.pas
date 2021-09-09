unit untMain;

interface

{.$DEFINE USE_TMS_ADVWEBBROWSER}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Generics.Collections, SynEditHighlighter, SynHighlighterHtml, SynEdit, Vcl.OleCtrls, SHDocVw, Vcl.ComCtrls,
  Vcl.ExtCtrls, System.ImageList, Vcl.ImgList,  Vcl.StdCtrls,
  Vcl.Imaging.pngimage, HtmlEmail,
  {$IFDEF USE_TMS_ADVWEBBROWSER}
  , AdvCustomControl, AdvWebBrowser
  {$ENDIF}
  ;

type
  TfrmMain = class(TForm)
    SynHTMLSyn1: TSynHTMLSyn;
    Panel2: TPanel;
    Panel1: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    SynEdit1: TSynEdit;
    PageControl2: TPageControl;
    TabSheet3: TTabSheet;
    lbElements: TListBox;
    Bevel6: TBevel;
    Bevel7: TBevel;
    Bevel8: TBevel;
    Bevel9: TBevel;
    Bevel10: TBevel;
    Bevel11: TBevel;
    Bevel12: TBevel;
    Bevel13: TBevel;
    Panel3: TPanel;
    Image1: TImage;
    Button2: TButton;
    Button1: TButton;
    Button3: TButton;
    Image2: TImage;
    TabControl1: TTabControl;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Panel5: TPanel;
    Bevel14: TBevel;
    Memo1: TMemo;
    Bevel5: TBevel;
    procedure SynEdit1Change(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure lbElementsClick(Sender: TObject);
    procedure lbElementsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure AdvWebBrowser1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Memo1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    FHtml: IHtmlDocument;
    {$IFDEF USE_TMS_ADVWEBBROWSER}
    FBrowser: TAdvWebBrowser;
    {$ELSE}
    FBrowser: TWebBrowser;
    {$ENDIF}
    procedure AddToListBox(AElement: IHtmlElement);
    { Private declarations }
  protected
    procedure DoShow; override;
    procedure UpdateBrowser;
    procedure UpdateElementList;
  public
    constructor Create(AOwner: TComponent); override;
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

const
  C_WELCOME_TO_HTML_EMAIL = 'Welcome to HtmlEmailBuilder! :-)';
  C_POWERED_BY = 'Powered by HtmlEmailBuilder for Delphi';

{$R *.dfm}

procedure TfrmMain.AdvWebBrowser1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FHtml.SelectElement(nil);
end;

procedure TfrmMain.Button1Click(Sender: TObject);
var
  p: IElementParagraph;
begin
  FHtml.Clear;
  FHtml.Title := 'Html Email Builder';

  FHtml.Content.AddSpacer(10);
  FHtml.Content.AddImage(Image1.Picture.Graphic, alCenter).Style.Width := '60px';
  FHtml.Content.AddSpacer(10);
  FHtml.Content.AddHR;
  FHtml.Content.AddSpacer(20);
  FHtml.Content.AddParagraph('Example showing bootstrap style alert elements...');
  FHtml.Content.AddAlert(C_WELCOME_TO_HTML_EMAIL, asSuccess);
  FHtml.Content.AddSpacer(20);
  FHtml.Content.AddAlert(C_WELCOME_TO_HTML_EMAIL, asDanger);
  FHtml.Content.AddSpacer(20);
  FHtml.Content.AddAlert(C_WELCOME_TO_HTML_EMAIL, asWarning);
  FHtml.Content.AddSpacer(20);
  FHtml.Content.AddHr;

  p := FHtml.Content.AddParagraph(C_POWERED_BY);
  p.Style.FontSize := '11px';
  p.Style.Color := 'grey';
  FHtml.Content.AddSpacer(50);

  SynEdit1.Lines.Text := FHtml.HTML;

  UpdateBrowser;
  UpdateElementList;
end;

procedure TfrmMain.Button2Click(Sender: TObject);
var
  p: IElementParagraph;
begin
  FHtml.Clear;
  FHtml.Title := 'Html Email Builder';

  FHtml.Content.AddSpacer(10);
  FHtml.Content.AddImage(Image1.Picture.Graphic, alCenter).Style.Width := '60px';
  FHtml.Content.AddSpacer(10);
  FHtml.Content.AddHR;
  FHtml.Content.AddSpacer(20);
  FHtml.Content.AddParagraph('Example showing H1 to H6 elements...');
  FHtml.Content.AddH1(C_WELCOME_TO_HTML_EMAIL);
  FHtml.Content.AddH2(C_WELCOME_TO_HTML_EMAIL);
  FHtml.Content.AddH3(C_WELCOME_TO_HTML_EMAIL);
  FHtml.Content.AddH4(C_WELCOME_TO_HTML_EMAIL);
  FHtml.Content.AddH5(C_WELCOME_TO_HTML_EMAIL);
  FHtml.Content.AddH6(C_WELCOME_TO_HTML_EMAIL);
  FHtml.Content.AddSpacer(20);
  FHtml.Content.AddHr;

  p := FHtml.Content.AddParagraph(C_POWERED_BY);
  p.Style.FontSize := '11px';
  p.Style.Color := 'grey';
  FHtml.Content.AddSpacer(50);

  SynEdit1.Lines.Text := FHtml.HTML;

  UpdateBrowser;
  UpdateElementList;
end;

procedure TfrmMain.Button3Click(Sender: TObject);
var
  p: IElementParagraph;
begin
  FHtml.Clear;
  FHtml.Title := 'Html Email Builder';


  FHtml.Content.AddSpacer(10);
  FHtml.Content.AddImage(Image1.Picture.Graphic, alCenter).Style.Width := '60px';
  FHtml.Content.AddSpacer(10);
  FHtml.Content.AddHR;
  FHtml.Content.AddSpacer(20);
  FHtml.Content.AddParagraph('Example showing bootstrap style buttons...');

  FHtml.Content.AddButton('Green Button', '#', bcGreen).Style.Width := '110px';
  FHtml.Content.AddButton('Blue Button', '#', bcBlue).Style.Width := '110px';;
  FHtml.Content.AddButton('Red Button', '#', bcRed).Style.Width := '110px';;
  FHtml.Content.AddButton('Yellow Button', '#', bcYellow).Style.Width := '110px';;
  FHtml.Content.AddButton('Grey Button', '#', bcGrey).Style.Width := '110px';;

  FHtml.Content.AddHr;

  p := FHtml.Content.AddParagraph(C_POWERED_BY);
  p.Style.FontSize := '11px';
  p.Style.Color := 'grey';

  FHtml.Content.AddImage(Application.Icon, alCenter).Style.Width := '32px';
  FHtml.Content.AddSpacer(50);

  SynEdit1.Lines.Text := FHtml.HTML;

  UpdateBrowser;
  UpdateElementList;
end;

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  FHtml := CreateHtmlEmail;

  {$IFDEF USE_TMS_ADVWEBBROWSER}
  FBrowser := TAdvWebBrowser.Create(Self);
  {$ELSE}
  FBrowser := TWebBrowser.Create(Self);
  {$ENDIF}
  TWinControl(FBrowser).Parent := TabSheet1;
  FBrowser.Align := alClient;
end;

procedure TfrmMain.DoShow;
begin
  inherited;
  SynEdit1.OnChange(Self);
end;

procedure TfrmMain.lbElementsClick(Sender: TObject);
var
  AElement: IHtmlElement;
begin
  if Supports(lbElements.Items.Objects[lbElements.ItemIndex], IHtmlElement, AElement) then
  begin
    FHtml.SelectElement(AElement);
    SynEdit1.Text := FHtml.HTML;
    UpdateBrowser;
    Memo1.Text := StringReplace(AElement.Content, '<br>', #13#10, [rfReplaceAll, rfIgnoreCase]);
  end;
end;

procedure TfrmMain.lbElementsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  r: TRect;
  t: string;
  AElement: IHtmlElement;
begin
  lbElements.Canvas.FillRect(Rect);
  if Supports(lbElements.Items.Objects[Index], IHtmlElement, AElement) then
  begin
    r := Rect;
    t := AElement.DisplayName;
    lbElements.Canvas.FillRect(Rect);
    r.Left := r.Left + 50;
    lbElements.Canvas.Draw(Rect.Left+4, Rect.Top+1, Image2.Picture.Graphic);
    lbElements.Canvas.TextRect(r, t, [TTextFormats.tfVerticalCenter, TTextFormats.tfSingleLine]);
  end;
end;

procedure TfrmMain.Memo1Change(Sender: TObject);
var
  AElement: IHtmlElement;
begin
  if lbElements.ItemIndex = -1 then
    Exit;
  if Supports(lbElements.Items.Objects[lbElements.ItemIndex], IHtmlElement, AElement) then
  begin
    AElement.Content := StringReplace(Memo1.Text, #10, '<br>',[rfReplaceAll]);
    SynEdit1.Text := FHtml.HTML;
    UpdateBrowser;

  end;
end;

procedure TfrmMain.SynEdit1Change(Sender: TObject);
begin
  UpdateBrowser;
end;

procedure TfrmMain.UpdateBrowser;
{$IFNDEF USE_TMS_ADVWEBBROWSER}
var
  Doc: Variant;
{$ENDIF}
begin
  {$IFDEF USE_TMS_ADVWEBBROWSER}
  FBrowser.LoadHTML(SynEdit1.Lines.Text);
  {$ELSE}

  if not Assigned(FBrowser.Document) then
    FBrowser.Navigate('about:blank');

  Doc := FBrowser.Document;
  Doc.Clear;
  Doc.Write(SynEdit1.Lines.Text);
  Doc.Close;
  {$ENDIF}
end;

procedure TfrmMain.AddToListBox(AElement: IHtmlElement);
var
  ICount: integer;
  AChild: IHtmlElement;
begin
  lbElements.Items.AddObject(AElement.DisplayName, TObject(AElement));

  for ICount := 0 to AElement.Elements.Count-1 do
  begin
    AChild := Aelement.Elements[ICount];
    AddToListBox(AChild);
  end;
end;

procedure TfrmMain.UpdateElementList;
var
  AElement: IHtmlElement;
  ICount: integer;
begin
  lbElements.Clear;
  for ICount := 0 to FHtml.Content.Count-1 do
  begin
    AElement := FHtml.Content[ICount];
    AddToListBox(AElement);
  end;
end;

end.
