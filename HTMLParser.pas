unit HTMLParser;

(*
 Author:            Len-A-R
 Date of creation:  06.02.2023
 Update Date:       06.02.2023

 Description:
  -A class for parsing text with HTML tags and rendering on canvas
  -����� ��� �������� ������ � HTML ������ � ��������� �� �����.

 Supported HTML tag:
 <font size=10 name="Calibri" color=$00FF0000 style=[b,i,u,s]>....</font> - Font properties
 <b>...</b> - Bold
 <i>...</i> - Italic
 <u>...</u> - Underline
 <s>...</s> - Strikeout


 Using as object:
 var html := '����� ��� <font size=14>���� <b>������ <i>�����������</i> ����� <u>�� �����</u> ���</b>. <font color=$00FF0000>� ������</font> ��� ������</font>';
 HTMLDrawer:= THtmlDrawer.Create;
 HTMLDrawer.Source := html;
 Edit1.Caption := HTMLDrawer.SimpleText; //get text without html-tags
 ....
 procedure TForm1.FormPaint(Sender: TObject);
 begin
   HTMLDrawer.DrawHTML(Canvas, Rect(10,100, 300, 100));
 end;

 Using as function
 For drawing html on canvas:
 DrawHTMLText(html, Canvas, Rect(10,100, 300, 100));

 For convert Html text to simple text
 ShowMessage(HtmlToSimpleText(html));

 *)


interface

uses  System.SysUtils, System.Classes, VCL.Graphics, Masks, System.Generics.Collections,
  Winapi.Windows, Messages;

type
  TElement = class;
  PElement = ^TElement;
  TElement = class(TObjectList<TElement>)
  private
    FFontSize: Integer;
    FFontStyle: TFontStyles;
    FFontName: TFontName;
    FText: String;
    FFontColor: TColor;
    FParent: PElement;
    FTag: string;
    FTagClosed: Boolean;

  protected
    property TagClosed: Boolean     read FTagClosed write FTagClosed;
  public
    constructor Create(AParent: PElement);
    destructor Destroy;
    procedure Assign(Obj: PElement);
    property Parent: PElement       read FParent;
    property FontColor: TColor      read FFontColor write FFontColor;
    property FontStyle: TFontStyles read FFontStyle write FFontStyle;
    property FontSize: Integer      read FFontSize  write FFontSize;
    property FontName: TFontName    read FFontName  write FFontName;
    property Text: String           read FText      write FText;
    property Tag: string            read FTag       write FTag;
  end;

  THtmlDrawer = class
  private
    FSource: string;
    FTree: TElement;
    procedure ParseHTMLEx(ASource: string; var AParent: TElement; const APos: Integer = 0);
    procedure SetSource(const Value: string);
    procedure DoDrawHTML(ACanvas: TCanvas; ATree: TElement; var  ARect: TRect; AMaxRect: TRect);
    procedure DoGetSimpleText(ATree: TElement; var s: string);
    function GetStringRect (const AString: string; const X,Y: Integer; AFont: TFont): TRect;
    function GetSimpleText: string;
  public
    constructor Create;
    destructor Destroy;
    property Source: string read FSource write SetSource;
    property SimpleText: string read GetSimpleText;
    property Tree: TElement read FTree;
    procedure DrawHTML(ACanvas: TCanvas; ARect: TRect);
  end;

procedure DrawHTMLText(html: string; ACanvas: TCanvas; ARect: TRect);
function HtmlToSimpleText(html: string): string;

implementation

function StrToFontStyle(s: string): TFontStyles;
begin
  Result := [];
  if s.Contains('b') then Result := Result + [fsBold];
  if s.Contains('i') then Result := Result + [fsItalic];
  if s.Contains('u') then Result := Result + [fsUnderline];
  if s.Contains('s') then Result := Result + [fsStrikeOut];
end;


procedure DrawHTMLText(html: string; ACanvas: TCanvas; ARect: TRect);
begin
  var obj := THtmlDrawer.Create;
  try
    obj.Source := html;
    obj.DrawHTML(ACanvas, ARect);
  finally
    FreeAndNil(obj);
  end;
end;

function HtmlToSimpleText(html: string): string;
begin
  var obj := THtmlDrawer.Create;
  try
    obj.Source := html;
    Result := obj.SimpleText;
  finally
    FreeAndNil(obj);
  end;
end;

constructor THtmlDrawer.Create;
begin
  FTree:= TElement.Create(nil);
end;

destructor THtmlDrawer.Destroy;
begin
  FreeAndNil(FTree);
end;

procedure THtmlDrawer.DoDrawHTML(ACanvas: TCanvas; ATree: TElement;
  var ARect: TRect; AMaxRect: TRect);
var oldFont: TFont;
    rc: TRect;
begin
  oldFont := TFont.Create;
  oldFont.Assign(ACanvas.Font);
  try
    for var el in ATree do begin
      var sText := el.Text;
      ACanvas.Font.Color := el.FontColor;
      ACanvas.Font.Size  := el.FontSize;
      ACanvas.Font.Name  := el.FontName;
      ACanvas.Font.Style := el.FontStyle;

      rc := GetStringRect(el.Text, ARect.Left, ARect.Top, ACanvas.Font);
      if rc.Left < AMaxRect.Right then begin
        if rc.Right > AMaxRect.Right then
          ACanvas.TextRect(rc, sText, [tfWordBreak, tfEndEllipsis, tfModifyString])
        else
          ACanvas.TextRect(rc, rc.Left, rc.Top, sText);
      end;

      ARect.Left := rc.Right;

      if el.Count > 0 then
        DoDrawHTML(ACanvas, el, ARect, AMaxRect);
    end;
  finally
    ACanvas.Font.Assign(oldFont);
    FreeAndNil(oldFont);
  end;
end;

procedure THtmlDrawer.DoGetSimpleText(ATree: TElement; var s: string);
begin
  for var el in ATree do begin
    s := s + el.Text;
    if el.Count > 0 then
     DoGetSimpleText(el, s);
  end;
end;

procedure THtmlDrawer.DrawHTML(ACanvas: TCanvas; ARect: TRect);
begin
  var rc: TRect := ARect;
  DoDrawHTML(ACanvas, FTree, rc, ARect);
end;

function THtmlDrawer.GetSimpleText: string;
var s: string;
begin
  DoGetSimpleText(FTree, s);
  Result := s;
end;

function THtmlDrawer.GetStringRect(const AString: string; const X, Y: Integer;
  AFont: TFont): TRect;
var
 DC: HDC;
 RGN: HRGN;
begin
  DC := CreateCompatibleDC(0);
  Win32Check(DC<>0);
  try
    SelectObject(DC, AFont.Handle);
    Win32Check(BeginPath(DC));
    try
      TextOut(DC,X,Y,PChar(AString),Length(AString));
    finally
      EndPath(DC);
    end;

    RGN := PathToRegion(DC);
    Win32Check(RGN<>0);
    try
      GetRgnBox(RGN,Result);
    finally
      DeleteObject(RGN);
    end;

  finally
   DeleteDC(DC);
  end;
end;

procedure THtmlDrawer.ParseHTMLEx(ASource: string; var AParent: TElement; const APos: Integer = 0);
var TagOwner: TElement;
    IsClosingTag: Boolean;
    IsTag: Boolean;
    child: TElement;
    PosAfterTag: Integer;
begin
  IsClosingTag := False;
  IsTag := False;
  TagOwner := nil;
  child := nil;
  PosAfterTag := APos;
  if APos >= ASource.Length then exit;
 // if not Assigned(AParent) then AParent := TElement.Create(nil, nil);
  //������ ������ ����
  var Tag := ASource.Substring(APos, ASource.IndexOf('>', APos)-APos);
  IsTag := Tag.StartsWith('<');
  Tag := Tag.Trim(['<', '>']);
  if Tag.Contains('<') then Tag :=Tag.Substring(0, Tag.IndexOf('<'));
  var TagInfo := Trim(Tag).Split([' ']);
  var TagName := TagInfo[0];
  if IsTag and (TagName.StartsWith('/')) then begin
    IsClosingTag := True;
    TagName := TagName.Substring(1);
  end;
  if IsTag then begin
    if IsClosingTag then begin
      {���� � ������ ���� �� ����������� � ������  � ��� �����, ��� ������}
      var el := AParent;
      while el <> nil do begin
        if el.Parent = nil then exit;
        if (el.Tag = TagName) and not el.TagClosed then begin
          TagOwner := el;
          TagOwner.TagClosed := True;
          PosAfterTag := APos + String('</' + TagName + '>').Length;
          ParseHTMLEx(ASource, TagOwner.Parent^, PosAfterTag);
          break;
        end;
        el := el.Parent^;
      end;
      {��������� � ������}
    end
    else if ASource.Substring(APos, 2).StartsWith('<') then begin
      //������� ����� �������� �������
      child := TElement.Create(@AParent);
      child.Tag := tagName;
      //������ ��������
      if length(TagInfo) > 1 then begin
        for var i := 1 to High(TagInfo) do begin
          var attrName := TagInfo[i].Split(['='])[0];
          var attrValue := TagInfo[i].Split(['='])[1];
          if TagName.Equals('font') then begin
            if attrName.Equals('size')  then child.FontSize  := attrValue.ToInteger;
            if attrName.Equals('name')  then child.FontName  := attrValue.DeQuotedString;
            if attrName.Equals('color') then child.FontColor := attrValue.ToInteger;
            if attrName.Equals('style') then child.FontStyle := StrToFontStyle(attrValue);
          end;
        end;
      end;
      if TagName.Equals('b') then child.FontStyle := child.FontStyle + [fsBold] else
      if TagName.Equals('i') then child.FontStyle := child.FontStyle + [fsItalic] else
      if TagName.Equals('u') then child.FontStyle := child.FontStyle + [fsUnderline] else
      if TagName.Equals('s') then child.FontStyle := child.FontStyle + [fsStrikeOut];
      //���������� �������� ����� ����
      PosAfterTag := APos + Tag.Length +2;
      //������ ����� ����� ������������ ����, �� ������� ���� �� �������� "<"
      var text := ASource.Substring(PosAfterTag, ASource.IndexOf('<', PosAfterTag)-PosAfterTag);
      child.Text := text;
      ParseHTMLEx(ASource, child, PosAfterTag + text.Length);
    end
  end
  else begin
    //����� � ����������� �������� ����
    child := TElement.Create(@AParent);
    //���������� �������� ����� ����
    PosAfterTag := APos + Tag.Length;
    child.Text := Tag;
    child.Tag := '-';
    ParseHTMLEx(ASource, child, PosAfterTag);
  end;
end;

{ TElement }

procedure TElement.Assign(Obj: PElement);
begin
  FFontSize := Obj.FontSize;
  FFontStyle:= Obj.FontStyle;
  FFontName := Obj.FontName;
  FFontColor:= Obj.FontColor;
end;

constructor TElement.Create(AParent: PElement);
begin
  inherited Create;
  //�������� �� ���������
  FParent := AParent;
  if FParent <> nil then FParent.Add(self);
  FFontSize:=8;
  FFontStyle:= [];
  FFontName:= 'Tahoma';
  FText:='';
  FFontColor:= clBlack;
  TagClosed := False;
  //���� ���� ��������, �� �������� ��� ��������
  if AParent <> nil then
    Assign(FParent);
end;

destructor TElement.Destroy;
begin
  clear;
  Inherited Destroy;
end;

procedure THtmlDrawer.SetSource(const Value: string);
begin
  FSource := Value;
  FTree.Clear;
  ParseHTMLEx('<BODY>' + FSource + '</BODY>', FTree);
end;

end.

