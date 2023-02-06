unit uTest;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, HTMLParser, Vcl.ComCtrls, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    TreeView1: TTreeView;
    Edit1: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    { Private declarations }
    HTMLDrawer: THtmlDrawer;
    procedure BuildTree(Elem: TElement; Node: TTreeNode);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

const html = '—ьешь ещЄ <font size=14>этих <b>м€гких <i>французских</i> булок ' +
             '<u>да выпей</u> чаю</b>. <font color=$00FF0000> ' +
             '» сделай</font> что нибудь</font>';


procedure TForm1.BuildTree(Elem: TElement; Node: TTreeNode);
begin
  for var el in elem do begin
    var itm := TreeView1.Items.AddChild(node, '[' + el.Tag + ']' + el.Text);
    if el.Count > 0 then BuildTree(el, itm);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  HTMLDrawer:= THtmlDrawer.Create;
  HTMLDrawer.Source := html;
  var tree := HTMLDrawer.Tree;
  BuildTree(tree, nil);
  edit1.Text := HTMLDrawer.SimpleText;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
//  DrawHTMLText(html, Canvas, Rect(10,100, 300, ClientHeight));
  HTMLDrawer.DrawHTML(Canvas, Rect(10,100, ClientWidth, ClientHeight));
end;

end.
