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

unit HtmlEmail;

interface

uses Classes, System.Generics.Collections, Graphics;

type

  THtmlAlign = (alLeft, alCenter, alRight);
  THtmlLinkTarget = (ltNone, ltBlank);

  TAlertStyle = (asSuccess, asDanger, asWarning, asUnknown);
  TButtonColor = (bcGreen, bcBlue, bcRed, bcYellow, bcGrey);

  TCssProperty = (Background,
                  BackgroundColor,
                  Border,
                  Color,
                  Cursor,
                  Display,
                  Font,
                  FontFamily,
                  FontSize,
                  FontVariant,
                  FontWeight,
                  Height,
                  LetterSpacing,
                  LineHeight,
                  Margin,
                  MaxWidth,
                  Padding,
                  PaddingLeft,
                  PaddingRight,
                  TextAlign,
                  TextDecoration,
                  Width,
                  Unknown);


  TCssClass = class;
  TCssClassList = class;
  TDocumentStyles = class;

  IElementDiv = interface;
  IHtmlElement = interface;
  IElementImage = interface;
  IElementParagraph = interface;
  IElementSpacer = interface;
  IElementButton = interface;

  IElementBR = interface;
  IElementLink = interface;
  IElementHeading = interface;
  IElementHr = interface;
  IHtmlElementAlert = interface;

  IElementList = interface
    ['{916D6812-A5BF-4BFD-8DB6-6459A8254BFB}']
    function GetCount: integer;
    function GetItem(AIndex: integer): IHtmlElement;

    function AddAlert(AText: string; AAlertStyle: TAlertStyle): IHtmlElementAlert;
    function AddBr: IElementBR;
    function AddLink(AUrl: string): IElementLink;
    function AddH1(AText: string): IElementHeading;
    function AddH2(AText: string): IElementHeading;
    function AddH3(AText: string): IElementHeading;
    function AddH4(AText: string): IElementHeading;
    function AddH5(AText: string): IElementHeading;
    function AddH6(AText: string): IElementHeading;
    function AddHr: IElementHR;

    function AddButton(AText, AUrl: string; AColor: TButtonColor; const ATarget: THtmlLinkTarget = ltNone): IElementButton;


    function AddDiv(const AClass: string = ''): IElementDiv;
    function AddImage(AUrl: string;
                      const AAlignment: THtmlAlign = alLeft;
                      const APadding: integer = 0): IElementImage; overload;
    function AddImage(AGraphic: TGraphic;
                      const AAlignment: THtmlAlign = alLeft;
                      const APadding: integer = 0): IElementImage; overload;

    function AddParagraph(AText: string): IElementParagraph;
    function AddSpacer(AHeight: integer): IElementSpacer;

    property Item[AIndex: integer]: IHtmlElement read GetItem; default;
    property Count: integer read GetCount;
  end;




  IHtmlElement = interface
    ['{08B1724A-4C2B-487C-9854-5DBD77902B33}']

    function GetStyle2: TCssClass;

    function GetContent: string;
    function GetAttribute(AName: string): string;
    function GetAttributesSingleLine: string;
    function GetStyle(AProperty: TCssProperty): string;
    procedure SetStyle(AProperty: TCssProperty; AValue: string);
    procedure SetAttribute(AName: string; const Value: string);
    procedure SetContent(const Value: string);
    function GetUuid: string;
    function GetCss: TCssClass;
    function GetElements: IElementList;
    function GetDisplayName: string;
    function GetSelected: Boolean;
    function GetCssClass: string;
    procedure SetCssClass(const Value: string);

    procedure SetSelected(const Value: Boolean);
    procedure GetHtml(ASource: TStrings);
    property DisplayName: string read GetDisplayName;
    property Elements: IElementList read GetElements;
    property Selected: Boolean read GetSelected write SetSelected;
    property Style: TCssClass read GetStyle2;
    property CssClass: string read GetCssClass write SetCssClass;
    property Content: string read GetContent write SetContent;
    property Attribute[AName: string]: string read GetAttribute write SetAttribute;
    property Uuid: string read GetUuid;
  end;

  IElementBR = interface(IHtmlElement)
    ['{BEB42585-81C1-4182-AA05-D94CA2A706F8}']
  end;

  IElementLink = interface(IHtmlElement)
    ['{E60C9004-8FBF-4AD9-B7DF-E2EADFC9FF51}']
    function GetHref: string;
    procedure SetHref(const Value: string);
    property Href: string read GetHref write SetHref;
  end;

  IElementHeading = interface(IHtmlElement)
    ['{93B61A23-69A9-4D9C-A0A3-418849C29662}']
  end;

  IElementHr = interface(IHtmlElement)
    ['{0AA4F1BD-B448-4937-8118-926865092FF0}']
  end;

  IElementButton = interface(IHtmlElement)
    ['{27632734-90EB-404F-A5CD-785DF16E1DD6}']
  end;

  IHtmlElementAlert = interface(IHtmlElement)
    ['{0EA3441A-B3AC-4957-AB49-4F16BE8A4C83}']
    function GetAlertStyle: TAlertStyle;
    procedure SetAlertStyle(const Value: TAlertStyle);
    property AlertStyle: TAlertStyle read GetAlertStyle write SetAlertStyle;
  end;


  IElementDiv = interface(IHtmlElement)
    ['{CB81149E-4480-44C9-BDB6-4AA48E94F1F6}']
  end;

  IContainerDiv = interface(IHtmlElement)
    ['{DCE1669D-4E5B-42F8-9C44-2FC40D8763AC}']
    function GetContainerDiv: IElementDiv;
    property ContainerDiv: IElementDiv read GetContainerDiv;
  end;

  IElementImage = interface(IContainerDiv)
    ['{F1D4FD5E-A774-415C-8D51-6523222D0017}']
    function GetSrc: string;
    procedure SetSrc(AValue: string);
    property Src: string read GetSrc write SetSrc;
  end;

  IElementParagraph = interface(IHtmlElement)
    ['{50B02A2F-0E15-4A18-B1FB-BF08F6449884}']
  end;

  IElementSpacer = interface(IHtmlElement)
    ['{0A9EFFB9-06E3-40FE-83BC-9056D63D6E1B}']
  end;



  IHtmlDocument = interface
    ['{EDD4DF8A-78F5-4625-8F12-1E6C0891E00C}']
    function GetDocumentStyles: TDocumentStyles;
    function GetCss: TCssClassList;
    function GetTitle: string;
    function GetHtmlText: string;
    function GetContent: IElementList;
    procedure Clear;
    procedure SelectElement(AElement: IHtmlElement);
    procedure SetTitle(AValue: string);
    property Styles: TDocumentStyles read GetDocumentStyles;
    property Content: IElementList read GetContent;
    property HTML: string read GetHtmlText;
    property Title: string read GetTitle write SetTitle;
  end;



  TCssItemName = record
    Item: TCssProperty;
    Name: string;
  end;

  TCssValue = class
  private
    FProp: TCssProperty;
    FValue: string;
  public
    property Prop: TCssProperty read FProp;
    property Value: string read FValue write FValue;
  end;

  TCssClass = class
  private
    FName: string;
    FCssValues: TObjectList<TCssValue>;
    function GetItem(AProperty: TCssProperty): TCssValue;
    function GetAsSingleLine: string;
    function GetCssValue(const Index: Integer): string;
    procedure SetCssValue(const Index: Integer; const Value: string);
    procedure SetAsString(const Value: string);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Name: string read FName write FName;
    property Item[AProperty: TCssProperty]: TCssValue read GetItem; default;
    property Items: TObjectList<TCssValue> read FCssValues;
    property AsString: string read GetAsSingleLine write SetAsString;

    property Background: string index Background read GetCssValue write SetCssValue;
    property Border: string index Border read GetCssValue write SetCssValue;
    property Color: string index Color read GetCssValue write SetCssValue;
    property Font: string index Font read GetCssValue write SetCssValue;
    property FontSize: string index FontSize read GetCssValue write SetCssValue;
    property Height: string index Height read GetCssValue write SetCssValue;
    property LetterSpacing: string index LetterSpacing read GetCssValue write SetCssValue;
    property Margin: string index Margin read GetCssValue write SetCssValue;
    property MaxWidth: string index MaxWidth read GetCssValue write SetCssValue;
    property Padding: string index Padding read GetCssValue write SetCssValue;
    property PaddingLeft: string index PaddingLeft read GetCssValue write SetCssValue;
    property PaddingRight: string index PaddingRight read GetCssValue write SetCssValue;
    property TextAlign: string index TextAlign read GetCssValue write SetCssValue;
    property Width: string index Width read GetCssValue write SetCssValue;
  end;



  TCssClassList = class(TObjectList<TCssClass>)
  private
    function GetClassByName(AName: string): TCssClass;
    function GetClassAsSingleLine(AClassName: string): string;
  public
    function AddClass(AName: string; const AValue: string = ''): TCssClass;
    property ClassByName[AName: string]: TCssClass read GetClassByName; default;
    property ClassAsSingleLine[AClass: string]: string read GetClassAsSingleLine;
  end;

  TDocumentStyles = class(TCssClassList)
  private
    function GetBody: TCssClass;
    function GetP: TCssClass;
    function GetH1: TCssClass;
    function GetH2: TCssClass;
    function GetH3: TCssClass;
    function GetH4: TCssClass;
    function GetH5: TCssClass;
    function GetH6: TCssClass;
    function GetOrCreateStyle(AName: string): TCssClass;
    function GetAlertStyle(AAlertStyle: TAlertStyle): TCssClass;
    function GetButton(AColor: TButtonColor): TCssClass;
    function GetHR: TCssClass;
  public
    property Body: TCssClass read GetBody;
    property P: TCssClass read GetP;
    property H1: TCssClass read GetH1;
    property H2: TCssClass read GetH2;
    property H3: TCssClass read GetH3;
    property H4: TCssClass read GetH4;
    property H5: TCssClass read GetH5;
    property H6: TCssClass read GetH6;
    property HR: TCssClass read GetHR;
    property Alert[AAlertStyle: TAlertStyle]: TCssClass read GetAlertStyle;
    property Button[AColor: TButtonColor]: TCssClass read GetButton;
  end;

  function CreateHtmlEmail: IHtmlDocument;

  function PropertyToString(AProp: TCssProperty): string;

implementation

uses SysUtils, HtmlEmail.Elements;

const
  TCssItemNameLookup: array[1..23] of TCssItemName =
    (
      (Item: Background;      Name: 'background' ),
      (Item: BackgroundColor; Name: 'background-color' ),
      (Item: Border;          Name: 'border' ),
      (Item: Color;           Name: 'color' ),
      (Item: Cursor;          Name: 'cursor' ),
      (Item: Display;         Name: 'display' ),
      (Item: Font;            Name: 'font' ),
      (Item: FontFamily;      Name: 'font-family' ),
      (Item: FontSize;        Name: 'font-size' ),
      (Item: FontVariant;     Name: 'font-variant' ),
      (Item: FontWeight;      Name: 'font-weight' ),
      (Item: Height;          Name: 'height' ),
      (Item: LetterSpacing;   Name: 'letter-spacing' ),
      (Item: LineHeight;      Name: 'line-height' ),
      (Item: Margin;          Name: 'margin' ),
      (Item: MaxWidth;        Name: 'max-width' ),
      (Item: Padding;         Name: 'padding' ),
      (Item: PaddingLeft;     Name: 'padding-left' ),
      (Item: PaddingRight;    Name: 'padding-right' ),
      (Item: TextAlign;       Name: 'text-align' ),
      (Item: TextDecoration;  Name: 'text-decoration' ),
      (Item: Width;           Name: 'width' ),
      (Item: Unknown;         Name: '' )
    );
type

  THtmlDocument = class(TInterfacedObject, IHtmlDocument)
  private
    FMeta: TStrings;
    FTitle: string;
    FCssClasses: TDocumentStyles;
    FElements: IElementList;
    FContent: IElementDiv;
    function GetHtmlText: string;
    function GetContent: IElementList;
    function GetCss: TCssClassList;
    function GetTitle: string;
    function GetDocumentStyles: TDocumentStyles;
    procedure SetTitle(AValue: string);
    procedure BuildDefaultStyles;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    property Content: IElementList read GetContent;
    procedure Clear;
    procedure SelectElement(AElement: IHtmlElement);
    property Styles: TDocumentStyles read GetDocumentStyles;
    property HTML: string read GetHtmlText;
    property Title: string read GetTitle write SetTitle;
  end;

function CreateHtmlEmail: IHtmlDocument;
begin
  Result := THtmlDocument.Create;
end;


function StringToProperty(AStr: string): TCssProperty;
var
  ICount: integer;
begin
  Result := Unknown;
  for ICount := Low(TCssItemNameLookup) to High(TCssItemNameLookup) do
  begin
    if TCssItemNameLookup[ICount].Name.ToLower = Trim(AStr.ToLower) then
    begin
      Result := TCssItemNameLookup[ICount].Item;
      Exit;
    end;
  end;
end;

function PropertyToString(AProp: TCssProperty): string;
var
  ICount: integer;
begin
  Result := '';
  for ICount := Low(TCssItemNameLookup) to High(TCssItemNameLookup) do
  begin
    if TCssItemNameLookup[ICount].Item = AProp then
    begin
      Result := TCssItemNameLookup[ICount].Name;
      Exit;
    end;
  end;
end;



{ THtmlDocument }

procedure THtmlDocument.BuildDefaultStyles;
begin
  Styles.Clear;
  Styles.Body.AsString  := 'background: #efefef; line-height: 40px; font-family: Arial, Helvetica, sans-serif; font-size: 15px; color: #666; line-height: 1.6; ';
  Styles.P.AsString     := 'line-height: 1.6; font-family: Arial, Helvetica, sans-serif; color: #666;  font-size: 15px;';
  Styles.H1.AsString    := 'font-family: Tahoma, Geneva, sans-serif; color: #005CB7; font-weight: 400;';
  Styles.H2.AsString    := 'font-family: Tahoma, Geneva, sans-serif; color: #005CB7; font-weight: 400;';
  Styles.H3.AsString    := 'font-family: Tahoma, Geneva, sans-serif; color: #005CB7; font-weight: 400;';
  Styles.H4.AsString    := 'font-family: Tahoma, Geneva, sans-serif; color: #005CB7; font-weight: 400;';
  Styles.H5.AsString    := 'font-family: Tahoma, Geneva, sans-serif; color: #005CB7; font-weight: 400;';
  Styles.H6.AsString    := 'font-family: Tahoma, Geneva, sans-serif; color: #005CB7; font-weight: 400;';
  Styles.HR.AsString := 'border: none; height: 1px; color: white; background-color: #ccc;';
  Styles.Button[bcBlue].AsString := 'background-color: #0d6efd; color: white; border: none; padding: 15px 32px; text-align: center; text-decoration: none; display: inline-block; font-size: 16px; margin: 4px 2px; cursor: pointer;';
  Styles.Button[bcGrey].AsString := 'background-color: grey; color: white; border: none; padding: 15px 32px; text-align: center; text-decoration: none; display: inline-block; font-size: 16px; margin: 4px 2px; cursor: pointer;';
  Styles.Button[bcGreen].AsString := 'background-color: #198754; color: white; border: none; padding: 15px 32px; text-align: center; text-decoration: none; display: inline-block; font-size: 16px; margin: 4px 2px; cursor: pointer;';
  Styles.Button[bcRed].AsString := 'background-color: #dc3545; color: white; border: none; padding: 15px 32px; text-align: center; text-decoration: none; display: inline-block; font-size: 16px; margin: 4px 2px; cursor: pointer;';
  Styles.Button[bcYellow].AsString := 'background-color: #ffc107; color: black; border: none; padding: 15px 32px; text-align: center; text-decoration: none; display: inline-block; font-size: 16px; margin: 4px 2px; cursor: pointer;';
  Styles.Button[bcGrey].AsString := 'background-color: #adb5bd; color: black; border: none; padding: 15px 32px; text-align: center; text-decoration: none; display: inline-block; font-size: 16px; margin: 4px 2px; cursor: pointer;';

  Styles.Alert[asSuccess].AsString := 'color: #3c763d;background-color: #dff0d8; padding: 10px; margin-bottom: 20px; border: 1px solid #BADFAA; border-radius: 10px; text-align: center;';
  Styles.Alert[asDanger].AsString := 'color: #B20000; background-color: #FFB0B0; padding: 10px; margin-bottom: 20px; border: 1px solid #FF9999; border-radius: 10px; text-align: center;';
  Styles.Alert[asWarning].AsString := 'color: #B28500; background-color: #FFFFBF; padding: 10px; margin-bottom: 20px; border: 1px solid #FFDC73; border-radius: 10px; text-align: center;';
end;

procedure THtmlDocument.Clear;
var
  AClass: TCssClass;
begin
  FMeta.Clear;
  FCssClasses.Clear;
  FElements := CreateElementsList(Self);
  BuildDefaultStyles;

  FContent := FElements.AddDiv('content');
  AClass := FCssClasses.AddClass('content');
  AClass.Width := '100%';
  AClass.MaxWidth := '600px';
  AClass.Margin := 'auto';
  AClass.Background := 'white';
  AClass.Border := '1px silver solid';
  AClass.TextAlign := 'center';
  AClass.PaddingLeft := '16px';
  AClass.PaddingRight := '16px';
  AClass.LetterSpacing := '0.5px';
end;

constructor THtmlDocument.Create;
begin
  inherited Create;
  FMeta := TStringList.Create;
  FCssClasses := TDocumentStyles.Create;
  Clear;
end;

destructor THtmlDocument.Destroy;
begin
  FMeta.Free;
  FCssClasses.Free;
  inherited;
end;

function THtmlDocument.GetContent: IElementList;
begin
  Result := FContent.Elements;
end;

function THtmlDocument.GetCss: TCssClassList;
begin
  Result := FCssClasses;
end;

function THtmlDocument.GetDocumentStyles: TDocumentStyles;
begin
  Result := FCssClasses;
end;

procedure THtmlDocument.SelectElement(AElement: IHtmlElement);
var
  e: IHtmlElement;
  ICount: integer;

begin
  for ICount := 0 to FContent.Elements.Count-1 do
  begin
    e := FContent.Elements[ICount];
    e.Selected := e.Uuid = AElement.Uuid;
  end;
end;

procedure THtmlDocument.SetTitle(AValue: string);
begin
  FTitle := AValue;
end;

function THtmlDocument.GetHtmlText: string;
var
  AStrings: TStringList;
  AMeta: string;
begin
  AStrings := TStringList.Create;
  try
    AStrings.Add('<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">');

    AStrings.Add('<html>');
    AStrings.Add('<head>');
    AStrings.Add('<title>'+FTitle+'</title>');
    AStrings.Add('<meta charset="utf-8" http-equiv="X-UA-Compatible" content="IE=edge"/>');
    AStrings.Add('<meta http-equiv="Content-Type" content="text/html charset=UTF-8" />');
    for AMeta in FMeta do
    begin
      if Trim(AMeta) <> '' then AStrings.Add('<meta '+AMeta+'/>');
    end;
    AStrings.Add('</head>');
    AStrings.Add('<body style="'+FCssClasses.GetClassAsSingleLine('body')+'">');
    FContent.GetHtml(AStrings);
    AStrings.Add('</body>');
    AStrings.Add('</html>');
    Result := AStrings.Text;
  finally
    AStrings.Free;
  end;
end;

function THtmlDocument.GetTitle: string;
begin
  Result := FTitle;
end;

{ TCssClass }

constructor TCssClass.Create;
begin
  FCssValues := TObjectList<TCssValue>.Create;
end;

destructor TCssClass.Destroy;
begin
  FCssValues.Free;
  inherited;
end;

function TCssClass.GetAsSingleLine: string;

  function PropertyToStr(AProp: TCssProperty): string;
  var
    AItem: TCssItemName;
  begin
    Result := '';
    for AItem in TCssItemNameLookup do
    begin
      if AItem.Item = AProp then
      begin
        Result := AItem.Name;
        Exit;
      end;
    end;
  end;

var
  AValue: TCssValue;
begin
  Result := '';
  for AValue in FCssValues do
  begin
    if Trim(AValue.Value) <> '' then
      Result := Result + PropertyToStr(AValue.Prop)+': '+Trim(AValue.Value)+'; ';

  end;
end;

function TCssClass.GetCssValue(const Index: Integer): string;
begin
  Result := Item[TCssProperty(Index)].Value;
end;

function TCssClass.GetItem(AProperty: TCssProperty): TCssValue;
var
  AItem: TCssValue;
begin

  for AItem in FCssValues do
  begin
    if AItem.Prop = AProperty then
    begin
      Result := AItem;
      Exit;
    end;
  end;
  Result := TCssValue.Create;
  Result.FProp := AProperty;
  FCssValues.Add(Result);
end;


procedure TCssClass.SetAsString(const Value: string);
var
  AProperties: TStrings;
  ICount: integer;
  AProp: TCssProperty;
begin
  if Value <> '' then
  begin
    AProperties := TStringList.Create;
    try
      AProperties.Text := StringReplace(Value, ';', #13, [rfReplaceAll]);
      for ICount := 0 to AProperties.Count-1 do
      begin
        AProperties[ICount] := StringReplace(AProperties[ICount], ':', '=', []);
        AProp := StringToProperty(AProperties.Names[ICount]);
        if AProp <> Unknown then
        begin
          Item[AProp].Value := Trim(AProperties.ValueFromIndex[ICount]);
        end;
      end;
    finally
      AProperties.Free;
    end;
  end;
end;

procedure TCssClass.SetCssValue(const Index: Integer; const Value: string);
begin
  Item[TCssProperty(Index)].Value := Value;
end;

{ TCssClassList }

function TCssClassList.AddClass(AName: string; const AValue: string = ''): TCssClass;
var
  AProperties: TStrings;
  ICount: integer;
  AProp: TCssProperty;
begin
  Result := ClassByName[AName];
  if Result = nil then
  begin
    Result := TCssClass.Create;
    Result.FName := AName;
    Add(Result);
  end;

  if AValue <> '' then
  begin
    AProperties := TStringList.Create;
    try
      AProperties.Text := StringReplace(AValue, ';', #13, [rfReplaceAll]);
      for ICount := 0 to AProperties.Count-1 do
      begin
        AProperties[ICount] := StringReplace(AProperties[ICount], ':', '=', []);
        AProp := StringToProperty(AProperties.Names[ICount]);
        if AProp <> Unknown then
        begin
          Result.Item[AProp].Value := Trim(AProperties.ValueFromIndex[ICount]);
        end;

      end;
    finally
      AProperties.Free;
    end;
  end;
end;

function TCssClassList.GetClassAsSingleLine(AClassName: string): string;
var
  AClass: TCssClass;
begin
  Result := '';
  AClass := ClassByName[AClassName];
  if AClass <> nil then
    Result := AClass.AsString;
end;

function TCssClassList.GetClassByName(AName: string): TCssClass;
var
  AClass: TCssClass;
begin
  Result := nil;
  for AClass in Self do
  begin
    if AClass.Name.ToLower = AName.ToLower then
    begin
      Result := AClass;
      Exit;
    end;
  end;
end;



{ TDocumentStyles }

function TDocumentStyles.GetAlertStyle(AAlertStyle: TAlertStyle): TCssClass;
begin
  Result := nil;
  case AAlertStyle of
    asSuccess:  Result := GetOrCreateStyle('alert.success');
    asDanger:   Result := GetOrCreateStyle('alert.danger');
    asWarning:  Result := GetOrCreateStyle('alert.warning');
  end;
end;

function TDocumentStyles.GetBody: TCssClass;
begin
  Result := GetOrCreateStyle('body');
end;

function TDocumentStyles.GetButton(AColor: TButtonColor): TCssClass;
begin
  Result := nil;
  case AColor of
    bcGreen: Result := GetOrCreateStyle('button.green');
    bcBlue: Result := GetOrCreateStyle('button.blue');
    bcRed: Result := GetOrCreateStyle('button.red');
    bcYellow: Result := GetOrCreateStyle('button.yellow');
    bcGrey: Result := GetOrCreateStyle('button.grey');
  end;
end;

function TDocumentStyles.GetH1: TCssClass;
begin
  Result := GetOrCreateStyle('h1');
end;

function TDocumentStyles.GetH2: TCssClass;
begin
  Result := GetOrCreateStyle('h2');
end;

function TDocumentStyles.GetH3: TCssClass;
begin
  Result := GetOrCreateStyle('h3');
end;

function TDocumentStyles.GetH4: TCssClass;
begin
  Result := GetOrCreateStyle('h4');
end;

function TDocumentStyles.GetH5: TCssClass;
begin
  Result := GetOrCreateStyle('h5');
end;

function TDocumentStyles.GetH6: TCssClass;
begin
  Result := GetOrCreateStyle('h6');
end;

function TDocumentStyles.GetHR: TCssClass;
begin
  Result := GetOrCreateStyle('hr');
end;

function TDocumentStyles.GetOrCreateStyle(AName: string): TCssClass;
begin
  Result := ClassByName[AName];
  if Result = nil then
    Result := AddClass(AName, '');
end;

function TDocumentStyles.GetP: TCssClass;
begin
  Result := GetOrCreateStyle('p');
end;

end.
