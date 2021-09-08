{*******************************************************************************
*                                                                              *
*  HTML Email Builder                                                          *
*                                                                              *
*  https://github.com/gmurt/ksAws                                              *
*                                                                              *
*  Copyright 2021 Graham Murt                                                  *
*                                                                              *
*  email: graham@kernow-software.co.uk                                         *
*                                                                              *
*  Licensed under the Apache License, Version 2.0 (the "License");             *
*  you may not use this file except in compliance with the License.            *
*  You may obtain a copy of the License at                                     *
*                                                                              *
*    http://www.apache.org/licenses/LICENSE-2.0                                *
*                                                                              *
*  Unless required by applicable law or agreed to in writing, software         *
*  distributed under the License is distributed on an "AS IS" BASIS,           *
*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    *
*  See the License for the specific language governing permissions and         *
*  limitations under the License.                                              *
*                                                                              *
*******************************************************************************}

unit HtmlEmail.Elements;

interface

uses HtmlEmail, Classes, System.Generics.Collections, Graphics;

function CreateElementsList(ADocument: IHtmlDocument): IElementList;

implementation

uses SysUtils, Net.HttpClient, System.NetEncoding;

type
  TBaseElement = class(TInterfacedObject, IHtmlElement)
  private
    FUuid: string;
    FDocument: IHtmlDocument;
    FAttributes: TStrings;
    FContent: string;
    FChildren: IElementList;
    FClass: string;
    FSelected: Boolean;
    FCss: TCssClass;
    function GetStyle: TCssClass;
    function GetAttribute(AName: string): string;
    function GetDisplayName: string; virtual;
    procedure SetAttribute(AName: string; const Value: string);
    function GetCss: TCssClass;
    function GetSelected: Boolean;
    procedure SetSelected(const Value: Boolean);
    function GetElements: IElementList;
    function GetCssClass: string;
    procedure SetCssClass(const Value: string);
    function GetContent: string;
    procedure SetContent(const Value: string);
    function GetUuid: string;
  protected
    function IgnoreClosingTag: Boolean; virtual;
    function GetTag: string; virtual; abstract;
    function GetAttributesSingleLine: string;
    procedure GetHtml(ASource: TStrings); virtual;
  protected
    function GetTagDisplay: string; virtual;
    property Tag: string read GetTag;
    property TagDisplay: string read GetTagDisplay;
    property DisplayName: string read GetDisplayName;
    property Content: string read GetContent write SetContent;
    property Elements: IElementList read GetElements;
    property Attribute[AName: string]: string read GetAttribute write SetAttribute;
    property CssClass: string read GetCssClass write SetCssClass;
    property Selected: Boolean read GetSelected write SetSelected;
    property Css: TCssClass read GetCss;
  public
    constructor Create(ADocument: IHtmlDocument); virtual;
    destructor Destroy; override;
    property Uuid: string read GetUuid;
  end;

  TElement = class(TBaseElement)
  public
    property Tag;
    property TagDisplay;
    property Content;
    property Elements;
    property Attribute;
    property CssClass;
    property Selected;
    property DisplayName;
    property Css;
  end;

  TElementDiv = class(TElement, IElementDiv)
  protected
    function GetDisplayName: string; override;
    function GetTag: string; override;
    function GetTagDisplay: string; override;
  end;

  
  TElementLink = class(TElement, IElementLink)
  private
    function GetHref: string;
    procedure SetHref(const Value: string);
  protected
    function GetDisplayName: string; override;
    function GetTag: string; override;
  public
    property Href: string read GetHref write SetHref;
  end;

  TElementSpacer = class(TElementDiv, IElementSpacer)
  protected
    function GetDisplayName: string; override;
  end;

  TElementDivWrapped = class(TElement, IContainerDiv)
  private
    FContainerDiv: IElementDiv;
    function GetContainerDiv: IElementDiv;
  protected
    procedure GetHtml(ASource: TStrings); override;
  public
    constructor Create(AHtmlDocument: IHtmlDocument); override;
    property ContainerDiv: IElementDiv read GetContainerDiv;
  end;

  TElementH1 = class(TElement, IElementHeading)
  protected
    function GetDisplayName: string; override;
    function GetTag: string; override;
  end;

  TElementH2 = class(TElement, IElementHeading)
  protected
    function GetDisplayName: string; override;
    function GetTag: string; override;
  end;

  TElementH3 = class(TElement, IElementHeading)
  protected
    function GetDisplayName: string; override;
    function GetTag: string; override;
  end;

  TElementH4 = class(TElement, IElementHeading)
  protected
    function GetDisplayName: string; override;
    function GetTag: string; override;
  end;

  TElementH5 = class(TElement, IElementHeading)
  protected
    function GetDisplayName: string; override;
    function GetTag: string; override;
  end;

  TElementH6 = class(TElement, IElementHeading)
  protected
    function GetDisplayName: string; override;
    function GetTag: string; override;
  end;

  TElementHR = class(TElement, IElementHr)
  protected
    function GetDisplayName: string; override;
    function GetTag: string; override;
  end;

  TElementBR = class(TElement, IElementBR)
  protected
    function GetDisplayName: string; override;
    function GetTag: string; override;
  end;

  TElementParagraph = class(TElement, IElementParagraph)
  protected
    function GetDisplayName: string; override;
    function GetTag: string; override;
  end;

  TElementImage = class(TElementDivWrapped, IElementImage)
  protected
    function GetSrc: string;
    procedure SetSrc(AValue: string);
    function GetDisplayName: string; override;
    function GetTag: string; override;
  end;

  TElementButton = class(TElement, IElementButton)
  protected
    function GetDisplayName: string; override;
    function GetTag: string; override;
  end;

  THtmlElementAlert = class(TElementDiv, IHtmlElementAlert)
  protected
    function GetDisplayName: string; override;
    function GetAlertStyle: TAlertStyle;
    procedure SetAlertStyle(const Value: TAlertStyle);
  end;

  TElementList = class(TInterfacedObject, IElementList)
  private
    FItems: TList<IHtmlElement>;
    FDocument: IHtmlDocument;
    function GetItem(AIndex: integer): IHtmlElement;
    function GetCount: integer;
  public
    constructor Create(AHtmlDocument: IHtmlDocument); virtual;
    destructor Destroy; override;
    procedure Add(AObj: IHtmlElement);


    function AddAlert(AText: string; AAlertStyle: TAlertStyle): IHtmlElementAlert;
    function AddDiv(const AClass: string = ''): IElementDiv;
    function AddSpacer(AHeight: integer): IElementSpacer;
    function AddBr: IElementBR;
    function AddLink(AUrl: string): IElementLink;
    function AddH1(AText: string): IElementHeading;
    function AddH2(AText: string): IElementHeading;
    function AddH3(AText: string): IElementHeading;
    function AddH4(AText: string): IElementHeading;
    function AddH5(AText: string): IElementHeading;
    function AddH6(AText: string): IElementHeading;
    function AddHr: IElementHR;

    function AddImage(AUrl: string;
                      const AAlignment: THtmlAlign = alLeft;
                      const APadding: integer = 0): IElementImage; overload;

    function AddImage(AGraphic: TGraphic;
                      const AAlignment: THtmlAlign = alLeft;
                      const APadding: integer = 0): IElementImage; overload;

    function AddButton(AText, AUrl: string; AColor: TButtonColor; const ATarget: THtmlLinkTarget = ltNone): IElementButton;
    function AddParagraph(AText: string): IElementParagraph;
    procedure GetHtml(ASource: TStrings);

  end;

function CreateElementsList(ADocument: IHtmlDocument): IElementList;
begin
  Result := TElementList.Create(ADocument);
end;

{ TElementH1 }

function TElementH1.GetDisplayName: string;
begin
  Result := 'Heading 1';
end;

function TElementH1.GetTag: string;
begin
  Result := 'h1';
end;

{ TBaseElement }

constructor TBaseElement.Create(ADocument: IHtmlDocument);
begin
  FUuid := TGuid.NewGuid.ToString;
  FDocument := ADocument;
  FChildren := TElementList.Create(FDocument);
  FAttributes := TStringList.Create;
  FCss := TCssClass.Create;
  FSelected := False;
end;

destructor TBaseElement.Destroy;
begin
  FAttributes.Free;
  FCss.Free;
  inherited;
end;

function TBaseElement.GetAttribute(AName: string): string;
begin
  Result := FAttributes.Values[AName.ToLower];
end;

function TBaseElement.GetAttributesSingleLine: string;
var
  ICount: integer;
  ACss: string;
  AName: string;
begin
  Result := '';
  if FClass <> '' then ACss := FDocument.Styles.ClassAsSingleLine[FClass];

  if ACss = '' then
    ACss := FDocument.Styles.ClassAsSingleLine[Tag.ToLower];

  ACss := ACss + ' ' + FCss.AsString;

  if FSelected then
    ACss := ACss + 'outline: red dashed 1px; outline-offset: 2px;';
  if Self is TElementSpacer then ACss := ACss +'width: 100%; ';


  for ICount := 0 to FAttributes.Count-1 do
  begin
    AName := FAttributes.Names[ICount].ToLower;
    Result := Result + ' '+AName +'="'+FAttributes.ValueFromIndex[ICount]+'"';
  end;

  if Trim(ACss) <> '' then

    Result := Result + ' style="'+ACss+'" ';
end;

function TBaseElement.GetContent: string;
begin
  Result := FContent;
end;

function TBaseElement.GetCss: TCssClass;
begin
  Result := FCss;
end;

function TBaseElement.GetCssClass: string;
begin
  Result := FClass;
end;


function TBaseElement.GetDisplayName: string;
begin
  Result := TagDisplay;
end;

function TBaseElement.GetElements: IElementList;
begin
  Result := FChildren;
end;

procedure TBaseElement.GetHtml(ASource: TStrings);
var
  AOpenTag: string;
  ACloseTag: string;
  ICount: integer;
begin
  AOpenTag := '<'+Tag;
  AOpenTag := AOpenTag+ ' '+GetAttributesSingleLine;
  if Tag = 'table' then
    AOpenTag := Trim(AOpenTag)+'><tr><td>'
  else
    AOpenTag := Trim(AOpenTag) + '>';


  ASource.Add(AOpenTag+FContent);

  for ICount := 0 to FChildren.Count-1 do
    FChildren[ICount].GetHtml(ASource);


  if (Tag <> 'br') and (Tag <> 'img') and (Tag <> 'hr') then
  begin
    ACloseTag := '</'+Tag+'>';
    if Tag = 'table' then
      ACloseTag := '</td></tr>'+ACloseTag;
    if FChildren.Count = 0 then
      ASource.Text := Trim(ASource.Text) + ACloseTag
    else
      ASource.Add(ACloseTag);
  end;
end;

function TBaseElement.GetSelected: Boolean;
begin
  Result := FSelected;
end;

function TBaseElement.GetStyle: TCssClass;
begin
  Result := FCss;
end;

function TBaseElement.GetTagDisplay: string;
begin
  Result := Tag;
end;

function TBaseElement.GetUuid: string;
begin
  Result := FUuid;
end;

function TBaseElement.IgnoreClosingTag: Boolean;
begin
  Result := False;
end;

procedure TBaseElement.SetAttribute(AName: string; const Value: string);
begin
  FAttributes.Values[AName.ToLower] := Value;
end;

procedure TBaseElement.SetContent(const Value: string);
begin
  FContent := Value;
end;

procedure TBaseElement.SetCssClass(const Value: string);
begin
  FClass := Value;
end;

procedure TBaseElement.SetSelected(const Value: Boolean);
begin
  FSelected := Value;
end;

{ TElementParagraph }

function TElementParagraph.GetDisplayName: string;
begin
  Result := 'Paragraph';
end;

function TElementParagraph.GetTag: string;
begin
  Result := 'p';
end;

{ TElementImage }

function TElementImage.GetDisplayName: string;
begin
  Result := 'Image';
end;

function TElementImage.GetSrc: string;
begin
  Result := Attribute['src'];
end;

function TElementImage.GetTag: string;
begin
  Result := 'img';
end;


procedure TElementImage.SetSrc(AValue: string);
begin
  Attribute['src'] := AValue;
end;

{ TElementList }

procedure TElementList.Add(AObj: IHtmlElement);
begin
  FItems.Add(AObj);
end;

function TElementList.AddAlert(AText: string; AAlertStyle: TAlertStyle): IHtmlElementAlert;
begin
  Result := THtmlElementAlert.Create(FDocument);
  Result.AlertStyle := AAlertStyle;
  Result.Style.Width := '100%';
  Result.Content := AText;
  Add(Result);
end;

function TElementList.AddBr: IElementBR;
begin
  Result := TElementBR.Create(FDocument);
  Add(Result);
end;

function TElementList.AddButton(AText, AUrl: string; AColor: TButtonColor; const ATarget: THtmlLinkTarget = ltNone): IElementButton;
begin
  Result := TElementButton.Create(FDocument);
  Result.Content := AText;
  Result.Attribute['href'] := AUrl;
  case AColor of
    bcGreen: Result.CssClass := 'button.green';
    bcBlue: Result.CssClass := 'button.blue';
    bcRed: Result.CssClass := 'button.red';
    bcYellow: Result.CssClass := 'button.yellow';
    bcGrey: Result.CssClass := 'button.grey';
  end;

  case ATarget of
    ltBlank: Result.Attribute['target'] := '_blank';
  end;
  Add(Result);
end;

function TElementList.AddDiv(const AClass: string = ''): IElementDiv;
begin
  Result := TElementDiv.Create(FDocument);
  Result.CssClass := AClass;
  Add(Result);
end;

function TElementList.AddH1(AText: string): IElementHeading;
begin
  Result := TElementH1.Create(FDocument);
  Result.Content := AText;
  Add(Result);
end;

function TElementList.AddH2(AText: string): IElementHeading;
begin
  Result := TElementH2.Create(FDocument);
  Result.Content := AText;
  Add(Result);
end;

function TElementList.AddH3(AText: string): IElementHeading;
begin
  Result := TElementH3.Create(FDocument);
  Result.Content := AText;
  Add(Result);
end;

function TElementList.AddH4(AText: string): IElementHeading;
begin
  Result := TElementH4.Create(FDocument);
  Result.Content := AText;
  Add(Result);
end;

function TElementList.AddH5(AText: string): IElementHeading;
begin
  Result := TElementH5.Create(FDocument);
  Result.Content := AText;
  Add(Result);
end;

function TElementList.AddH6(AText: string): IElementHeading;
begin
  Result := TElementH6.Create(FDocument);
  Result.Content := AText;
  Add(Result);
end;

function TElementList.AddHr: IElementHR;
begin
  Result := TElementHR.Create(FDocument);
  Add(Result);
end;

function TElementList.AddImage(AUrl: string;
                               const AAlignment: THtmlAlign = alLeft;
                               const APadding: integer = 0): IElementImage;
var
  AHttp: THttpClient;
  AData: TMemoryStream;
  ABase64: TStringStream;
begin
  ABase64 := TStringStream.Create;
  try
    AHttp := THTTPClient.Create;
    AData := TMemoryStream.Create;
    try
      AHttp.Get(AUrl, AData);
      AData.Position := 0;
      TNetEncoding.Base64.Encode(AData, ABase64);
      ABase64.Position := 0;
    finally
      AHttp.Free;
      AData.Free;
    end;
    Result := TElementImage.Create(FDocument);
    Result.Attribute['src'] := AUrl;
    Result.Attribute['alt'] := 'image';
    case AAlignment of
      alCenter: Result.ContainerDiv.Attribute['align'] := 'center';
      alRight: Result.ContainerDiv.Attribute['align'] := 'right';
    end;

    if APadding > 0 then Result.Style.Padding := APadding.ToString+'px';
    Result.Style.Margin := '0px';

    Add(Result);
  finally
    ABase64.Free;
  end;
end;

function TElementList.AddImage(AGraphic: TGraphic;
                               const AAlignment: THtmlAlign = alLeft;
                               const APadding: integer = 0): IElementImage;
var
  AData: TMemoryStream;
  ABase64: TStringStream;
begin
  ABase64 := TStringStream.Create;
  AData := TMemoryStream.Create;
  try
    AGraphic.SaveToStream(AData);
    AData.Position := 0;
    TNetEncoding.Base64.Encode(AData, ABase64);
    ABase64.Position := 0;

    Result := TElementImage.Create(FDocument);
    Result.Attribute['src'] := 'data:image/jpeg;base64,'+ABase64.DataString;

    Result.Attribute['alt'] := 'image';
    case AAlignment of
      alCenter: Result.ContainerDiv.Attribute['align'] := 'center';
      alRight: Result.ContainerDiv.Attribute['align'] := 'right';
    end;


    if APadding > 0 then Result.Style.Padding := APadding.ToString+'px';
    Add(Result);
  finally
    ABase64.Free;
    AData.Free;
  end;
end;



function TElementList.AddLink(AUrl: string): IElementLink;
begin
  Result := TElementLink.Create(FDocument);
  Result.Href := AURL;
  Add(Result);
end;

function TElementList.AddParagraph(AText: string): IElementParagraph;
begin
  Result := TElementParagraph.Create(FDocument);
  Result.Content := AText;
  Add(Result);
end;

function TElementList.AddSpacer(AHeight: integer): IElementSpacer;
begin
  Result := TElementSpacer.Create(FDocument);
  Result.Style.Height := AHeight.ToString+'px';
  Result.Style.Width := '100%';
  Add(Result);
end;

constructor TElementList.Create(AHtmlDocument: IHtmlDocument);
begin
  inherited Create;
  FItems := TList<IHtmlElement>.Create;
  FDocument := AHtmlDocument;
end;

destructor TElementList.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TElementList.GetCount: integer;
begin
  Result := FItems.Count;
end;

procedure TElementList.GetHtml(ASource: TStrings);
var
  AElement: IHtmlElement;
begin
  for AElement in FItems do
    AElement.GetHtml(ASource);
end;

function TElementList.GetItem(AIndex: integer): IHtmlElement;
begin
  Result := FItems[AIndex];
end;

{ TElementHR }

function TElementHR.GetDisplayName: string;
begin
  Result := 'Horizontal Rule';
end;

function TElementHR.GetTag: string;
begin
  Result := 'hr';
end;

{ TElementDiv }


function TElementDiv.GetDisplayName: string;
begin
  Result := 'Spacer';
end;

function TElementDiv.GetTag: string;
begin
  Result := 'table';
end;

function TElementDiv.GetTagDisplay: string;
begin
  Result := 'div';
end;

{ TElementBR }

function TElementBR.GetDisplayName: string;
begin
  Result := 'Break';
end;

function TElementBR.GetTag: string;
begin
  Result := 'br';
end;

{ TElementDivWrapped }

constructor TElementDivWrapped.Create;
begin
  inherited;
  FContainerDiv := TElementDiv.Create(FDocument);
end;

function TElementDivWrapped.GetContainerDiv: IElementDiv;
begin
  Result := FContainerDiv;
end;

procedure TElementDivWrapped.GetHtml(ASource: TStrings);
var
  AHtml: TStrings;
begin
  AHtml := TStringList.Create;
  try
    inherited GetHtml(AHtml);
    AHtml.Text := '<table '+FContainerDiv.GetAttributesSingleLine+' cellpadding="0" cellspacing="0" border="0" ><tr><td>'+AHtml.Text;
    AHtml.Text := AHtml.Text + '</td></tr></table>';
    ASource.AddStrings(AHtml);
  finally
    AHtml.Free;
  end;
end;

{ TElementButton }

function TElementButton.GetDisplayName: string;
begin
  Result := 'Button';
end;

function TElementButton.GetTag: string;
begin
  Result := 'a';
end;

{ TElementLink }

function TElementLink.GetDisplayName: string;
begin
  Result := 'Hyperlink';
end;

function TElementLink.GetHref: string;
begin
  Result := Attribute['href']
end;

function TElementLink.GetTag: string;
begin
  Result := 'a';
end;

procedure TElementLink.SetHref(const Value: string);
begin
  Attribute['href'] := Value;
end;

{ TElementSpacer }

function TElementSpacer.GetDisplayName: string;
begin
  Result := 'Spacer';
end;

{ THtmlElementAlert }

function THtmlElementAlert.GetAlertStyle: TAlertStyle;
begin
  Result := asUnknown;
  if CssClass = 'alert.success' then Result := asSuccess;
  if CssClass = 'alert.danger' then Result := asDanger;
  if CssClass = 'alert.warning' then Result := asWarning;
end;

function THtmlElementAlert.GetDisplayName: string;
begin
  Result := 'alert';
end;

procedure THtmlElementAlert.SetAlertStyle(const Value: TAlertStyle);
begin
  case Value of
    asSuccess: CssClass := 'alert.success';
    asDanger: CssClass := 'alert.danger';
    asWarning: CssClass := 'alert.warning';
  end;
end;

{ TElementH6 }

function TElementH6.GetDisplayName: string;
begin
  Result := 'Heading 6';
end;

function TElementH6.GetTag: string;
begin
  Result := 'h6';
end;

{ TElementH2 }

function TElementH2.GetDisplayName: string;
begin
  Result := 'Heading 2';
end;

function TElementH2.GetTag: string;
begin
  Result := 'h2';
end;

{ TElementH3 }

function TElementH3.GetDisplayName: string;
begin
  Result := 'Heading 3';
end;

function TElementH3.GetTag: string;
begin
  Result := 'h3';
end;

{ TElementH4 }

function TElementH4.GetDisplayName: string;
begin
  Result := 'Heading 4';
end;

function TElementH4.GetTag: string;
begin
  Result := 'h4';
end;

{ TElementH5 }

function TElementH5.GetDisplayName: string;
begin
  Result := 'Heading 5';
end;

function TElementH5.GetTag: string;
begin
  Result := 'h5';
end;

end.
