
(********************************************************)
(*                                                      *)
(*  Private Distribution of Codebot Class Library       *)
(*  Version: 2.00.00                                    *)
(*  Date: March 2009                                    *)
(*                                                      *)
(********************************************************)

unit XMLParser;

interface

{$I CODEBOT.INC}

uses
  ActiveX, XMLObjects, Classes, SysUtils, Variants, Windows, BaseTypes
  {$IFDEF FASTSTRING}, FastStrings{$ENDIF};

type
  IDocumentInterface = XMLObjects.IDocumentInterface;
  ITextInterface = XMLObjects.ITextInterface;
  IDocument = XMLObjects.IDocument;
  INodes = XMLObjects.INodes;
  IElement = XMLObjects.IElement;
  IFiler = XMLObjects.IFiler;
  INode = XMLObjects.INode;
  IAttributes = XMLObjects.IAttributes;
  IAttribute = XMLObjects.IAttribute;

{ IObjectStorage }

type
  IObjectStorage = interface(IUnknown)
    ['{D08F4BE4-DEF3-4395-A58B-D427A86AC140}']
    function GetName: string;
    procedure WriteData(Node: INode);
    procedure ReadData(Node: INode);
    property Name: string read GetName;
  end;

procedure SaveObject(Instance: TObject; Document: IDocumentInterface);
procedure LoadObject(Instance: TObject; Document: IDocumentInterface);

function BeautifyXML(const XML: string): string; overload;
procedure BeautifyXML(const XML: string; const FileName: string); overload;

function CreateDocument: IDocument;
function CreateDocumentTest: IUnknown;

implementation

uses
  MSXMLParser;

const
  FieldTerminator = '/';
  DocVersion = 'version="1.0" encoding="ISO-8859-1"';

{ TDocument }

type
  TDocument = class(TInterfacedObject, IDocumentInterface, ITextInterface, IDocument)
  private
    FDocument: IXMLDOMDocument;
    { IDocumentInterface }
    function GetController: IUnknown;
    { ITextInterface }
    function GetText: string;
    procedure SetText(Value: string);
    { IDocument }
    function GetNodes: INodes;
    function GetRoot: INode;
    procedure SetRoot(Node: INode);
    function GetStyleSheet: string;
    procedure SetStyleSheet(Value: string);
    function CreateNode(const Name: string): INode;
    function ForceRoot(const Name: string): INode;
    procedure Instruct(const Target, Data: string);
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    function Transform(StyleSheet: IDocument): string;
  public
    constructor Create(Document: IXMLDOMDocument);
  end;

{ TNodes }

  TNodes = class(TInterfacedObject, IDocumentInterface, INodes)
  private
    FParent: IXMLDOMNode;
    FNodes: IXMLDOMNodeList;
    { IDocumentInterface }
    function GetController: IUnknown;
    { INodes }
    function GetCount: Integer;
    procedure SetNode(Index: Integer; Node: INode);
    function GetNode(Index: Integer): INode;
    function Add(const Name: string): INode;
    procedure Append(Node: INode);
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Insert(Index: Integer; Node: INode);
    procedure Move(CurIndex, NewIndex: Integer);
    procedure Remove(Node: INode);
    procedure Replace(OldNode, NewNode: INode);
  public
    constructor Create(Parent: IXMLDOMNode; Nodes: IXMLDOMNodeList);
  end;

  TElement = class(TInterfacedObject)
  private
    FNode: IXMLDOMNode;
    { IDocumentInterface }
    function GetController: IUnknown;
    { ITextInterface }
    function GetText: string;
    procedure SetText(Value: string);
    { IElement }
    function GetDocument: IDocument;
    function GetParent: INode;
    function GetName: string;
    function GetValue: string;
    procedure SetValue(const Value: string);
  public
    constructor Create(Node: IXMLDOMNode);
  end;

{ TNode }

  TNode = class(TElement, IDocumentInterface, ITextInterface, IElement, INode, IFiler)
  private
    FAttributes: IAttributes;
    FNodes: INodes;
    { INode }
    function GetAttributes: IAttributes;
    function GetFiler: IFiler;
    function GetNodes: INodes;
    function Clone(Deep: Boolean = True): INode;
    function FindAttribute(const Name: string): IAttribute;
    function FindNode(const Name: string): INode;
    function FindNodes(const Name: string): INodes;
    function ForceNode(const Name: string): INode;
    { IFiler }
    procedure ReadBinary(const Name: string; Stream: IStream);
    procedure WriteBinary(const Name: string; Stream: IStream);
    function ReadBool(const Name: string; Default: Boolean = False; Stored: Boolean = True): Boolean;
    procedure WriteBool(const Name: string; Value: Boolean);
    function ReadDate(const Name: string; const Default: TDateTime = 0; Stored: Boolean = True): TDateTime;
    procedure WriteDate(const Name: string; const Value: TDateTime);
    function ReadDateTime(const Name: string; const Default: TDateTime = 0; Stored: Boolean = True): TDateTime;
    procedure WriteDateTime(const Name: string; const Value: TDateTime);
    function ReadTime(const Name: string; const Default: TDateTime = 0; Stored: Boolean = True): TDateTime;
    procedure WriteTime(const Name: string; const Value: TDateTime);
    function ReadFloat(const Name: string; const Default: Double = 0; Stored: Boolean = True): Double;
    procedure WriteFloat(const Name: string; const Value: Double);
    function ReadInteger(const Name: string; Default: Integer = 0; Stored: Boolean = True): Integer;
    procedure WriteInteger(const Name: string; Value: Integer);
    function ReadString(const Name: string; const Default: string = ''; Stored: Boolean = True): string;
    procedure WriteString(const Name: string; const Value: string);
    function Read(const Name: string): Variant;
    procedure Write(const Name: string; const Value: Variant);
  end;

{ TAttributes }

  TAttributes = class(TInterfacedObject, IDocumentInterface, IAttributes, IFiler)
  private
    FParent: IXMLDOMNode;
    FAttributes: IXMLDOMNamedNodeMap;
    { IDocumentInterface }
    function GetController: IUnknown;
    { IAttributes }
    function GetCount: Integer;
    function GetFiler: IFiler;
    function GetNamedAttribute(const Name: string): IAttribute;
    function GetIndexedAttribute(Index: Integer): IAttribute;
    function Add(const Name: string): IAttribute;
    procedure Clear;
    procedure Remove(Attribute: IAttribute);
    { cannot move attributes? }
    { procedure Move(CurIndex, NewIndex: Integer); }
    { IFiler }
    procedure ReadBinary(const Name: string; Stream: IStream);
    procedure WriteBinary(const Name: string; Stream: IStream);
    function ReadBool(const Name: string; Default: Boolean = False; Stored: Boolean = True): Boolean;
    procedure WriteBool(const Name: string; Value: Boolean);
    function ReadDate(const Name: string; const Default: TDateTime = 0; Stored: Boolean = True): TDateTime;
    procedure WriteDate(const Name: string; const Value: TDateTime);
    function ReadDateTime(const Name: string; const Default: TDateTime = 0; Stored: Boolean = True): TDateTime;
    procedure WriteDateTime(const Name: string; const Value: TDateTime);
    function ReadTime(const Name: string; const Default: TDateTime = 0; Stored: Boolean = True): TDateTime;
    procedure WriteTime(const Name: string; const Value: TDateTime);
    function ReadFloat(const Name: string; const Default: Double = 0; Stored: Boolean = True): Double;
    procedure WriteFloat(const Name: string; const Value: Double);
    function ReadInteger(const Name: string; Default: Integer = 0; Stored: Boolean = True): Integer;
    procedure WriteInteger(const Name: string; Value: Integer);
    function ReadString(const Name: string; const Default: string = ''; Stored: Boolean = True): string;
    procedure WriteString(const Name: string; const Value: string);
    function Read(const Name: string): Variant;
    procedure Write(const Name: string; const Value: Variant);
  public
    constructor Create(Parent: IXMLDOMNode;
      Attributes: IXMLDOMNamedNodeMap);
  end;

  TAttribute = class(TElement, IDocumentInterface, ITextInterface, IElement,
    IAttribute)
  end;

{ TDocument }

constructor TDocument.Create(Document: IXMLDOMDocument);
begin
  inherited Create;
  FDocument := Document;
end;

{ TDocument.IDocumentController }

function TDocument.GetController: IUnknown;
begin
  Result := FDocument;
end;

{ TDocument.ITextInterface }

function TDocument.GetText: string;
begin
  Result := FDocument.xml;
end;

procedure TDocument.SetText(Value: string);
begin
  FDocument.loadXML(Value);
end;

function TDocument.GetNodes: INodes;
begin
  Result := TNodes.Create(FDocument, FDocument.childNodes);
end;

function TDocument.GetRoot: INode;
var
  R: IXMLDOMNode;
begin
  Result := nil;
  R := FDocument.lastChild;
  if (R <> nil) and
    (R.nodeType = NODE_ELEMENT) then
    Result := TNode.Create(R);
end;

procedure TDocument.SetRoot(Node: INode);
var
  R: IXMLDOMNode;
begin
  R := FDocument.lastChild;
  if (R <> nil) and
    (R.nodeType = NODE_ELEMENT) then
    FDocument.removeChild(R);
  if Node <> nil then
    FDocument.appendChild(Node.Controller as IXMLDOMNode);
end;

function TDocument.GetStyleSheet: string;
var
  Node: IXMLDOMNode;
  S: string;
  I, J: Integer;
begin
  Result := '';
  for I := 0 to FDocument.childNodes.length - 1 do
  begin
    Node := FDocument.childNodes[I];
    if (Node.nodeType = NODE_PROCESSING_INSTRUCTION) and (Node.nodeName = 'xml-stylesheet') then
    begin
      S := Node.nodeValue;
      J := Pos(' href="', S);
      if J = 0 then Break;
      J := J + Length(' href="');
      if Length(S) < J then Break;
      S := Copy(S, J, Length(S) - J + 1);
      J := Pos('"', S);
      if J = 0 then Break;
      SetLength(S, J - 1);
      Result := S;
      Break;
    end;
  end;
end;

procedure TDocument.SetStyleSheet(Value: string);
var
  Node: IXMLDOMNode;
  I: Integer;
begin
  for I := 0 to FDocument.childNodes.length - 1 do
  begin
    Node := FDocument.childNodes[I];
    if (Node.nodeType = NODE_PROCESSING_INSTRUCTION) and (Node.nodeName = 'xml-stylesheet') then
    begin
      Node.nodeValue := 'type="text/xsl" href="' + Value + '"';
      Exit;
    end;
  end;
  Node := FDocument.createProcessingInstruction('xml-stylesheet',
    'type="text/xsl" href="' + Value + '"');
  FDocument.insertBefore(Node, FDocument.lastChild);
end;

function TDocument.CreateNode(const Name: string): INode;
begin
  Result := TNode.Create(FDocument.createNode(NODE_ELEMENT, Name, ''));
end;

function TDocument.ForceRoot(const Name: string): INode;
begin
  Result := GetRoot;
  if (Result = nil) or (Result.Name <> Name) then
  begin
    Result := CreateNode(Name);
    SetRoot(Result);
  end;
end;

procedure TDocument.Instruct(const Target, Data: string);
var
  Node: IXMLDOMNode;
  I: Integer;
begin
  Node := FDocument.createProcessingInstruction('xml', DocVersion);
  if FDocument.firstChild = nil then
    FDocument.appendChild(Node)
  else if (FDocument.firstChild.nodeType <> NODE_PROCESSING_INSTRUCTION) or
    (FDocument.firstChild.nodeName <> 'xml') then
    FDocument.insertBefore(Node, FDocument.firstChild);
  for I := 0 to FDocument.childNodes.length - 1 do
  begin
    Node := FDocument.childNodes[I];
    if (Node.nodeType = NODE_PROCESSING_INSTRUCTION) and (Node.nodeName = Target) then
    begin
      Node.nodeValue := Data;
      Exit;
    end;
  end;
  Node := FDocument.createProcessingInstruction(Target, Data);
  if FDocument.lastChild.nodeType = NODE_ELEMENT then
    FDocument.insertBefore(Node, FDocument.lastChild)
  else
    FDocument.appendChild(Node);
end;

procedure TDocument.LoadFromFile(const FileName: string);
begin
  FDocument.load(FileName);
end;

procedure TDocument.SaveToFile(const FileName: string);
begin
  FDocument.save(FileName);
end;

function TDocument.Transform(StyleSheet: IDocument): string;
begin
  Result := '';
  if StyleSheet = nil then Exit;
  Result := FDocument.transformNode(StyleSheet.Controller as IXMLDOMNode);
  {Result := FastReplace(FDocument.transformNode(StyleSheet.Controller as IXMLDOMNode),
    ' xmlns=""', '', True);}
end;

{ TNodes }

constructor TNodes.Create(Parent: IXMLDOMNode; Nodes: IXMLDOMNodeList);
begin
  inherited Create;
  FParent := Parent;
  FNodes := Nodes;
end;

{ TNodes.IDocumentController }

function TNodes.GetController: IUnknown;
begin
  Result := FNodes;
end;

function TNodes.GetCount: Integer;
begin
  Result := FNodes.length;
end;

function TNodes.GetNode(Index: Integer): INode;
begin
  Result := TNode.Create(FNodes.item[Index]);
end;

procedure TNodes.SetNode(Index: Integer; Node: INode);
begin
  FParent.replaceChild(Node.Controller as IXMLDOMNode,
    GetNode(Index).Controller as IXMLDOMNode);
end;

function TNodes.Add(const Name: string): INode;
var
  Document: IXMLDomDocument;
  Parent, Child: IXMLDOMNode;
  Count, I: Integer;
begin
  if FParent.QueryInterface(IXMLDomDocument, Document) <> S_OK then
    Document := FParent.ownerDocument;
  Count := FieldCount(Name, FieldTerminator);
  Parent := FParent;
  Child := nil;
  for I := 0 to Count - 1 do
  begin
    Child := Document.createNode(NODE_ELEMENT, FieldValue(Name, FieldTerminator, I), '');
    Parent := Parent.appendChild(Child);
  end;
  Result := TNode.Create(Parent);
end;

procedure TNodes.Append(Node: INode);
begin
  FParent.appendChild(Node.Controller as IXMLDOMNode)
end;

procedure TNodes.Clear;
var
  I: Integer;
begin
  for I := FNodes.length - 1 downto 0 do
    FParent.removeChild(FNodes.item[I]);
end;

procedure TNodes.Delete(Index: Integer);
begin
  FParent.removeChild(FNodes.item[Index]);
end;

procedure TNodes.Insert(Index: Integer; Node: INode);
begin
  if Index < FNodes.length then
    FParent.insertBefore(Node.Controller as IXMLDOMNode, FNodes.item[Index])
  else
    FParent.appendChild(Node.Controller  as IXMLDOMNode);
end;

procedure TNodes.Move(CurIndex, NewIndex: Integer);
var
  A, B: IXMLDOMNode;
begin
  A := FNodes.item[CurIndex];
  B := FNodes.item[NewIndex];
  FParent.insertBefore(A, B);
end;

procedure TNodes.Remove(Node: INode);
begin
  FParent.removeChild(Node.Controller as IXMLDOMNode);
end;

procedure TNodes.Replace(OldNode, NewNode: INode);
begin
  FParent.replaceChild(NewNode.Controller as IXMLDOMNode, OldNode.Controller as IXMLDOMNode);
end;

{ TElement }

constructor TElement.Create(Node: IXMLDOMNode);
begin
  inherited Create;
  FNode := Node;
end;

{ TElement.IDocumentInterface }

function TElement.GetController: IUnknown;
begin
  Result := FNode;
end;

{ TElement.ITextInterface }

function TElement.GetText: string;
begin
  Result := FNode.xml;
end;

procedure TElement.SetText(Value: string);
begin
end;

{ TElement.IElement }

function TElement.GetDocument: IDocument;
begin
  Result := TDocument.Create(FNode.ownerDocument);
end;

function TElement.GetParent: INode;
var
  N: IXMLDOMNode;
begin
  Result := nil;
  N := FNode.parentNode;
  if  N <> nil then
  begin
    if N.nodeType = NODE_DOCUMENT then
      Exit;
    Result := TNode.Create(N);
  end;
end;

function TElement.GetName: string;
begin
  Result := FNode.nodeName;
end;

function TElement.GetValue: string;
begin
  Result := FNode.text;
end;

procedure TElement.SetValue(const Value: string);
begin
  FNode.text := Value;
end;

{ TNode.INode }

function TNode.GetAttributes: IAttributes;
begin
  if FAttributes = nil then
    if FNode.attributes <> nil then
      FAttributes := TAttributes.Create(FNode, FNode.attributes)
    else
      FAttributes := nil;
  Result := FAttributes;      
end;

function TNode.GetFiler: IFiler;
begin
  Result := Self;
end;

function TNode.GetNodes: INodes;
begin
  if FNodes = nil then
    FNodes := TNodes.Create(FNode, FNode.childNodes);
  Result := FNodes;
end;

function TNode.Clone(Deep: Boolean = True): INode;
begin
  Result := TNode.Create(FNode.cloneNode(Deep));
end;

function TNode.FindAttribute(const Name: string): IAttribute;
var
  Node: IXMLDOMNode;
begin
  Node := FNode.attributes.getNamedItem(Name);
  if Node <> nil then
    Result := TAttribute.Create(Node)
  else
    Result := nil;
end;

function TNode.FindNode(const Name: string): INode;
var
  Node: IXMLDOMNode;
begin
  Node := FNode.selectSingleNode(Name);
  if Node <> nil then
    Result := TNode.Create(Node)
  else
    Result := nil;
end;

function TNode.FindNodes(const Name: string): INodes;
var
  NodeList: IXMLDOMNodeList;
begin
  NodeList := FNode.selectNodes(Name);
  Result := TNodes.Create(FNode, NodeList)
end;

function TNode.ForceNode(const Name: string): INode;
var
  Document: IXMLDomDocument;
  Parent, Child: IXMLDOMNode;
  Count, I: Integer;
begin
  Document := FNode.ownerDocument;
  Count := FieldCount(Name, FieldTerminator);
  Parent := FNode;
  Child := nil;
  for I := 0 to Count - 1 do
  begin
    Child := Parent.selectSingleNode(FieldValue(Name, FieldTerminator, I));
    if Child <> nil then
    begin
      Parent := Child;
      Continue;
    end;
    Child := Document.createNode(NODE_ELEMENT, FieldValue(Name, FieldTerminator, I), '');
    Parent := Parent.appendChild(Child);
  end;
  Result := TNode.Create(Parent);
end;

{ TNode.IFiler }

procedure TextToBinary(const Text: string; Buffer: Pointer; Size: Integer);
const
  HexCodes: array['0'..'f'] of SmallInt =
    ( 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,-1,-1,-1,-1,-1,-1,
     -1,10,11,12,13,14,15,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,10,11,12,13,14,15);
var
  B: PByte absolute Buffer;
  Index: Integer;
  I: Integer;
begin
  if Length(Text) <> Size * 2 then Exit;
  Index := 1;
  for I := 1 to Size do
  begin
    if not CharInSet(Text[Index], ['0'..'9', 'A'..'F', 'a'..'f']) or
      not CharInSet(Text[Index], ['0'..'9', 'A'..'F', 'a'..'f']) then Break;
    B^ := Byte((HexCodes[Text[Index]] shl 4) + HexCodes[Text[Index + 1]]);
    Inc(Index, 2);
    Inc(B);
  end;
end;

procedure BinaryToText(Buffer: Pointer; Size: Integer; var Text: string);
const
  HexChars = '0123456789ABCDEF';
var
  B: PByte;
  Index: Integer;
  I: Integer;
begin
  Text := '';
  if Size < 1 then Exit;
  SetLength(Text, (Size) * 2);
  B := Buffer;
  Index := 1;
  for I := 1 to Size do
  begin
    Text[Index] := HexChars[(B^ shr $4) + 1];
    Inc(Index);
    Text[Index] := HexChars[(B^ and $F) + 1];
    Inc(Index);
    Inc(B);
  end;
end;

procedure TNode.ReadBinary(const Name: string; Stream: IStream);
var
  S: string;
  B: Pointer;
  I, W: Integer;
begin
  S := ForceNode(Name).Value;
  I := Length(S) shr 1;
  if I = 0 then Exit;
  GetMem(B, I);
  try
    TextToBinary(S, B, I);
    Stream.Write(B, I, @W);
  finally
    FreeMem(B);
  end;
end;

procedure TNode.WriteBinary(const Name: string; Stream: IStream);
var
  S, T: string;
  B: array[$0..$FF] of Byte;
  I: Integer;
begin
  S := '';
  T := '';
  repeat
    Stream.Write(@B, SizeOf(B), @I);
    BinaryToText(@B, I, T);
    S := S + T;
  until I < SizeOf(B);
  ForceNode(Name).Value := S;
end;

function TNode.ReadBool(const Name: string; Default: Boolean = False; Stored: Boolean = True): Boolean;
var
  Node: INode;
  S: string;
begin
  if Stored then
    Node := ForceNode(Name)
  else
    Node := FindNode(Name);
  if Node = nil then
  begin
    Result := Default;
    Exit;
  end;
  S := UpperCase(Trim(Node.Value));
  if (S = '1') or (S = 'Y') or (S = 'YES') or (S = 'TRUE') then
    Result := True
  else if (S = 'N') or (S = 'NO') or (S = 'FALSE') then
    Result := False
  else
    Result := Default;
  if Stored then
    if Result then Node.Value := 'true' else Node.Value := 'false';
end;

procedure TNode.WriteBool(const Name: string; Value: Boolean);
var
  Node: INode;
begin
  Node := ForceNode(Name);
  if Value then Node.Value := 'true' else Node.Value := 'false';
end;

function TNode.ReadDate(const Name: string; const Default: TDateTime = 0; Stored: Boolean = True): TDateTime;
var
  Node: INode;
begin
  if Stored then
    Node := ForceNode(Name)
  else
    Node := FindNode(Name);
  if Node = nil then
  begin
    Result := Default;
    Exit;
  end;
  Result := StrToDateDef(Node.Value, Default);
  if Stored then
    Node.Value := DateToStr(Result);
end;

procedure TNode.WriteDate(const Name: string; const Value: TDateTime);
begin
  ForceNode(Name).Value := DateToStr(Value);
end;

function TNode.ReadDateTime(const Name: string; const Default: TDateTime = 0; Stored: Boolean = True): TDateTime;
var
  Node: INode;
begin
  if Stored then
    Node := ForceNode(Name)
  else
    Node := FindNode(Name);
  if Node = nil then
  begin
    Result := Default;
    Exit;
  end;
  Result := StrToDateTimeDef(Node.Value, Default);
  if Stored then
    Node.Value := DateTimeToStr(Result);
end;

procedure TNode.WriteDateTime(const Name: string; const Value: TDateTime);
begin
  ForceNode(Name).Value := DateTimeToStr(Value);
end;

function TNode.ReadTime(const Name: string; const Default: TDateTime = 0; Stored: Boolean = True): TDateTime;
var
  Node: INode;
begin
  if Stored then
    Node := ForceNode(Name)
  else
    Node := FindNode(Name);
  if Node = nil then
  begin
    Result := Default;
    Exit;
  end;
  Result := StrToTimeDef(Node.Value, Default);
  if Stored then
    Node.Value := TimeToStr(Result);
end;

procedure TNode.WriteTime(const Name: string; const Value: TDateTime);
begin
  ForceNode(Name).Value := TimeToStr(Value);
end;

function TNode.ReadFloat(const Name: string; const Default: Double = 0; Stored: Boolean = True): Double;
var
  Node: INode;
begin
  if Stored then
    Node := ForceNode(Name)
  else
    Node := FindNode(Name);
  if Node = nil then
  begin
    Result := Default;
    Exit;
  end;
  Result := StrToFloatDef(Node.Value, Default);
  if Stored then
    Node.Value := FloatToStr(Result);
end;

procedure TNode.WriteFloat(const Name: string; const Value: Double);
begin
  ForceNode(Name).Value := FloatToStr(Value);
end;

function TNode.ReadInteger(const Name: string; Default: Integer = 0; Stored: Boolean = True): Integer;
var
  Node: INode;
begin
  if Stored then
    Node := ForceNode(Name)
  else
    Node := FindNode(Name);
  if Node = nil then
  begin
    Result := Default;
    Exit;
  end;
  Result := StrToIntDef(Node.Value, Default);
  if Stored then
    Node.Value := IntToStr(Result);
end;

procedure TNode.WriteInteger(const Name: string; Value: Integer);
begin
  ForceNode(Name).Value := IntToStr(Value);
end;

function TNode.ReadString(const Name: string; const Default: string = ''; Stored: Boolean = True): string;
var
  Node: INode;
begin
  if Stored then
    Node := ForceNode(Name)
  else
    Node := FindNode(Name);
  if Node = nil then
  begin
    Result := Default;
    Exit;
  end;
  Result := Trim(Node.Value);
  if Result = '' then
  begin
    Result := Default;
    if Stored then
      Node.Value := Result;
  end;
end;

procedure TNode.WriteString(const Name: string; const Value: string);
begin
  ForceNode(Name).Value := Value;
end;

function TNode.Read(const Name: string): Variant;
begin
  Result := ForceNode(Name).Value;
end;

procedure TNode.Write(const Name: string; const Value: Variant);
begin
  if Value = NULL then
    ForceNode(Name).Value := ''
  else
    ForceNode(Name).Value := Value;
end;

{ TAttributes }

constructor TAttributes.Create(Parent: IXMLDOMNode;
   Attributes: IXMLDOMNamedNodeMap);
begin
  inherited Create;
  FParent := Parent;
  FAttributes := Attributes;
end;

{ TAttributes.IDocumentInterface }

function TAttributes.GetController: IUnknown;
begin
  Result := FAttributes;
end;

{ TAttributes.IAttributes }

function TAttributes.GetCount: Integer;
begin
  Result := FAttributes.length;
end;

function TAttributes.GetFiler: IFiler;
begin
  Result := Self;
end;

function TAttributes.GetNamedAttribute(const Name: string): IAttribute;
var
  Node: IXMLDOMNode;
  Attribute: IXMLDOMAttribute;
begin
  Node := FAttributes.getNamedItem(Name);
  if Node = nil then
  begin
    Attribute := FParent.ownerDocument.createAttribute(Name);
    FAttributes.setNamedItem(Attribute);
  end
  else
    Attribute := Node as IXMLDOMAttribute;
  Result := TAttribute.Create(Attribute);
end;

function TAttributes.GetIndexedAttribute(Index: Integer): IAttribute;
begin
  Result := TAttribute.Create(FAttributes.item[Index]);
end;

function TAttributes.Add(const Name: string): IAttribute;
var
  Attribute: IXMLDOMAttribute;
begin
  Attribute := FParent.ownerDocument.createAttribute(Name);
  Result := TAttribute.Create(FAttributes.setNamedItem(Attribute));
end;

procedure TAttributes.Clear;
var
  I: Integer;
begin
  for I := FAttributes.length - 1 downto 0 do
    FAttributes.removeNamedItem(FAttributes.item[I].nodeName);
end;

procedure TAttributes.Remove(Attribute: IAttribute);
begin
  FAttributes.removeNamedItem(Attribute.Name);
end;

{procedure TAttributes.Move(CurIndex, NewIndex: Integer);
var
  A, B: IXMLDOMNode;
begin
  A := FAttributes.item[CurIndex];
  B := FAttributes.item[NewIndex];
  FParent.insertBefore(A, B);
end;}

{ TAttributes.IFiler }

procedure TAttributes.ReadBinary(const Name: string; Stream: IStream);
begin
end;

procedure TAttributes.WriteBinary(const Name: string; Stream: IStream);
begin
end;

function TAttributes.ReadBool(const Name: string; Default: Boolean = False; Stored: Boolean = True): Boolean;
var
  Attribute: IAttribute;
  S: string;
begin
  if Stored or (FAttributes.getNamedItem(Name) <> nil) then
    Attribute := GetNamedAttribute(Name)
  else
  begin
    Result := Default;
    Exit;
  end;
  S := UpperCase(Trim(Attribute.Value));
  if (S = '1') or (S = 'Y') or (S = 'YES') or (S = 'TRUE') then
    Result := True
  else if (S = 'N') or (S = 'NO') or (S = 'FALSE') then
    Result := False
  else
    Result := Default;
  if Stored then
    if Result then Attribute.Value := 'true' else Attribute.Value := 'false';
end;

procedure TAttributes.WriteBool(const Name: string; Value: Boolean);
var
  Attribute: IAttribute;
begin
  Attribute := GetNamedAttribute(Name);
  if Value then Attribute.Value := 'true' else Attribute.Value := 'false';
end;

function TAttributes.ReadDate(const Name: string; const Default: TDateTime = 0; Stored: Boolean = True): TDateTime;
var
  Attribute: IAttribute;
begin
  if Stored or (FAttributes.getNamedItem(Name) <> nil) then
    Attribute := GetNamedAttribute(Name)
  else
  begin
    Result := Default;
    Exit;
  end;
  Result := StrToDateDef(Attribute.Value, Default);
  if Stored then
    Attribute.Value := DateToStr(Result);
end;

procedure TAttributes.WriteDate(const Name: string; const Value: TDateTime);
begin
  GetNamedAttribute(Name).Value := DateToStr(Value);
end;

function TAttributes.ReadDateTime(const Name: string; const Default: TDateTime = 0; Stored: Boolean = True): TDateTime;
var
  Attribute: IAttribute;
begin
  if Stored or (FAttributes.getNamedItem(Name) <> nil) then
    Attribute := GetNamedAttribute(Name)
  else
  begin
    Result := Default;
    Exit;
  end;
  Result := StrToDateTimeDef(Attribute.Value, Default);
  if Stored then
    Attribute.Value := DateTimeToStr(Result);
end;

procedure TAttributes.WriteDateTime(const Name: string; const Value: TDateTime);
begin
  GetNamedAttribute(Name).Value := DateTimeToStr(Value);
end;

function TAttributes.ReadTime(const Name: string; const Default: TDateTime = 0; Stored: Boolean = True): TDateTime;
var
  Attribute: IAttribute;
begin
  if Stored or (FAttributes.getNamedItem(Name) <> nil) then
    Attribute := GetNamedAttribute(Name)
  else
  begin
    Result := Default;
    Exit;
  end;
  Result := StrToTimeDef(Attribute.Value, Default);
  if Stored then
    Attribute.Value := TimeToStr(Result);
end;

procedure TAttributes.WriteTime(const Name: string; const Value: TDateTime);
begin
  GetNamedAttribute(Name).Value := TimeToStr(Value);
end;

function TAttributes.ReadFloat(const Name: string; const Default: Double = 0; Stored: Boolean = True): Double;
var
  Attribute: IAttribute;
begin
  if Stored or (FAttributes.getNamedItem(Name) <> nil) then
    Attribute := GetNamedAttribute(Name)
  else
  begin
    Result := Default;
    Exit;
  end;
  Result := StrToFloatDef(Attribute.Value, Default);
  if Stored then
    Attribute.Value := FloatToStr(Result);
end;

procedure TAttributes.WriteFloat(const Name: string; const Value: Double);
begin
  GetNamedAttribute(Name).Value := FloatToStr(Value);
end;

function TAttributes.ReadInteger(const Name: string; Default: Integer = 0; Stored: Boolean = True): Integer;
var
  Attribute: IAttribute;
begin
  if Stored or (FAttributes.getNamedItem(Name) <> nil) then
    Attribute := GetNamedAttribute(Name)
  else
  begin
    Result := Default;
    Exit;
  end;
  Result := StrToIntDef(Attribute.Value, Default);
  if Stored then
    Attribute.Value := IntToStr(Result);
end;

procedure TAttributes.WriteInteger(const Name: string; Value: Integer);
begin
  GetNamedAttribute(Name).Value := FloatToStr(Value);
end;

function TAttributes.ReadString(const Name: string; const Default: string = ''; Stored: Boolean = True): string;
var
  Attribute: IAttribute;
begin
  if Stored or (FAttributes.getNamedItem(Name) <> nil) then
    Attribute := GetNamedAttribute(Name)
  else
  begin
    Result := Default;
    Exit;
  end;
  Result := Trim(Attribute.Value);
  if Result = '' then
  begin
    Result := Default;
    if Stored then
      Attribute.Value := Result;
  end;
end;

procedure TAttributes.WriteString(const Name: string; const Value: string);
begin
  GetNamedAttribute(Name).Value := Value;
end;

function TAttributes.Read(const Name: string): Variant;
var
  Attribute: IAttribute;
begin
  Attribute := GetNamedAttribute(Name);
  Result := Trim(Attribute.Value);
end;

procedure TAttributes.Write(const Name: string; const Value: Variant);
begin
  if Value = NULL then
    GetNamedAttribute(Name).Value := ''
  else
    GetNamedAttribute(Name).Value := Value;
end;

procedure SaveObject(Instance: TObject; Document: IDocumentInterface);
var
  O: IObjectStorage;
  D: IDocument;
  N: INode;
begin
  if Supports(Instance, IObjectStorage, O) then
  begin
    if Supports(Document, IDocument, D) then
    begin
      N := D.CreateNode(O.Name);
      O.WriteData(N);
    end
    else if Supports(Document, INode, N) then
    begin
      N := N.Nodes.Add(O.Name);
      O.WriteData(N);
    end;
  end;
end;

procedure LoadObject(Instance: TObject; Document: IDocumentInterface);
var
  O: IObjectStorage;
  D: IDocument;
  N: INode;
begin
  if Supports(Instance, IObjectStorage, O) then
  begin
    if Supports(Document, IDocument, D) and (D.Root <> nil) and (D.Root.Name = O.Name) then
      O.ReadData(D.Root)
    else if Supports(Document, INode, N) then
    begin
      N := N.FindNode(O.Name);
      if N <> nil then
        O.ReadData(N);
    end;
  end;
end;

function BeautifyXML(const XML: string): string;
var
  D: IDocument;
begin
  D := CreateDocument;
  {$IFDEF FASTSTRING}
  D.Text := FastReplace(XML,  '><', '>'#13#10'<');
  Result := FastReplace(D.Text, #9, '  ');
  {$ELSE}
  D.Text := StringReplace(XML,  '><', '>'#13#10'<', [rfReplaceAll]);
  Result := StringReplace(D.Text, #9, '  ', [rfReplaceAll]);
  {$ENDIF}
end;

procedure BeautifyXML(const XML: string; const FileName: string);
var
  D: IDocument;
  S: string;
begin
  D := CreateDocument;
  {$IFDEF FASTSTRING}
  S := FastReplace(XML,  '><', '>'#13#10'<');
  S := FastReplace(D.Text, #9, '  ');
  {$ELSE}
  S := StringReplace(XML,  '><', '>'#13#10'<', [rfReplaceAll]);
  S := StringReplace(D.Text, #9, '  ', [rfReplaceAll]);
  {$ENDIF}
  D.Text := S;
  D.SaveToFile(FileName);
end;

procedure OleCheck(Result: HResult);
begin
  if Result <> S_OK then raise Exception.Create('com call failed');
end;

function ProgIDToClassID(const ProgID: string): TGUID;
begin
  OleCheck(CLSIDFromProgID(PWideChar(WideString(ProgID)), Result));
end;

function CreateOleObject(const ClassName: string): IUnknown;
var
  ClassID: TCLSID;
begin
  ClassID := ProgIDToClassID(ClassName);
  OleCheck(CoCreateInstance(ClassID, nil, CLSCTX_INPROC_SERVER or
    CLSCTX_LOCAL_SERVER, IUnknown, Result));
end;

function CreateDocument: IDocument;
var
  Clsid: TCLSID;
begin
  {if CLSIDFromProgID('Msxml2.DOMDocument.4.0', Clsid) = S_OK then
    Result := TDocument.Create(CreateOleObject('Msxml2.DOMDocument.4.0') as IXMLDOMDocument)
  else}
    Result := TDocument.Create(CreateOleObject('Msxml.DOMDocument') as IXMLDOMDocument);
end;

function CreateDocumentTest: IUnknown;
begin
  Result := CreateOleObject('Msxml2.DOMDocument.4.0');
end;

end.
