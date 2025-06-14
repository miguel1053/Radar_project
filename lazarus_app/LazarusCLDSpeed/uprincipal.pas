unit uPrincipal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  lNet, lNetBase, lNetComponents, StrUtils, Types,  fphttpclient, opensslsockets,
  System.NetEncoding;
  //,
  //DCPrijndael, DCPsha256, DCPecdsa;

type

  { TFrmPrincipal }

  TFrmPrincipal = class(TForm)
    GpConex: TGroupBox;
    GroupBox1: TGroupBox;
    pbxA: TImage;
    Memo1: TMemo;
    pbxA1: TImage;
    pbxA2: TImage;
    pbxL1: TImage;
    pbxL2: TImage;
    Server: TLTCPComponent;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    lblVelocidade: TStaticText;
    lblTamanho: TStaticText;
    procedure FormActivate(Sender: TObject);
    procedure ServerAccept(aSocket: TLSocket);
    procedure ServerReceive(aSocket: TLSocket);
  private

  public
    procedure TTY(Msg: string);
    procedure processa(Msg: string);
    procedure captura(url: string);
    function GetImage(url: string): RawByteString;
    //function GetCameraImage(const URL: string): TMemoryStream;
    //function ValidateECDSASignature(const PublicKey, Signature, Data: string): Boolean;
    //function ValidateECDSA(const PublicKey, Signature, Data: string): Boolean;

  end;

var
  FrmPrincipal: TFrmPrincipal;
  bImgagem : Boolean;

implementation

{$R *.lfm}

{ TFrmPrincipal }

procedure TFrmPrincipal.TTY(Msg: string);
begin
try
  with Memo1.Lines do
    begin
      if Count > 100 then
        Delete(0);
      Add(Msg);
    end;
    except
      on E: Exception do
      Begin
       //TTY2('-----');
       //TTY2('Erro (TTY): ' + e.Message);
       //TTY2('-----');
      End;
    end;
end;

procedure TFrmPrincipal.FormActivate(Sender: TObject);
begin
  //Server.SocketNet := LA;
  Server.Listen(8001, LADDR_ANY);
  Memo1.Clear;
end;

procedure TFrmPrincipal.ServerAccept(aSocket: TLSocket);
begin
  TTY('Cliente Conectado...');
end;

{
function ValidateECDSA(const PublicKey, Signature, Data: string): Boolean;
var
  ECDSA: TDCP_ecdsa;
begin
  ECDSA := TDCP_ecdsa.Create(nil);
  try
    ECDSA.LoadPublicKey(PublicKey);
    Result := ECDSA.VerifyString(Data, Signature);
  finally
    ECDSA.Free;
  end;
end;



function ValidateECDSASignature(const PublicKey, Signature, Data: string): Boolean;
var
  ECDSA: TDCP_ecdsa;
begin
  ECDSA := TDCP_ecdsa.Create(nil);
  try
    ECDSA.LoadPublicKey(PublicKey);
    Result := ECDSA.VerifyString(Data, Signature);
  finally
    ECDSA.Free;
  end;
end;

 }

 function TFrmPrincipal.GetImage(url: string): RawByteString;
 var
   Buffer: RawByteString;//TBytes;
   WebClient: TFPHTTPClient;
   _username : string;
  _password : string ;
  sErro : string ;
 begin
    _username := 'cldradar';
   _password := 'cldradar10';
   //Buffer := nil;
   WebClient := TFPHTTPClient.Create(nil);
   try
     WebClient.AddHeader('User-Agent', 'Mozilla/5.0');
     WebClient.AddHeader('Accept', 'application/json');
     WebClient.AddHeader('Authorization', 'Basic ' + TNetEncoding.Base64.Encode(_username + ':' + _password));
     //WriteLn(WebClient.ResponseHeaders.Text);
     try
       Buffer := WebClient.Get(url);
     except
       on E: EHTTPClient do
         sErro := 'Erro: ' + E.Message;
       on E: Exception do
         sErro := 'Erro: ' + E.Message;
     end;
   finally
     WebClient.Free;
   end;
   Result := Buffer;
 end;


procedure  TFrmPrincipal.captura(url: string);
var
  HttpClient: TFPHttpClient;
  ImageStream: TMemoryStream;
  _username : string;// "admin";
  _password : string ;//"admin123";//"cldradar10";
Begin
HttpClient := TFPHttpClient.Create(nil);
 ImageStream := TMemoryStream.Create;
 try
   _username := 'cldradar';
   _password := 'cldradar10';
   {
   HttpClient.UserName := _username;
   HttpClient.Password := _password;
   HttpClient.AddHeader('User-Agent', 'Mozilla/5.0');
   HttpClient.AddHeader('Accept', 'application/json');
   HttpClient.AddHeader('Authorization', 'Basic ' + TNetEncoding.Base64.Encode(_username + ':' + _password));
   }
   HttpClient.Get(url, ImageStream);
   if HttpClient.ResponseStatusCode = 200 then
   begin
     ImageStream.Position := 0;
     pbxA.Picture.LoadFromStream(ImageStream);
   end
   else
     ShowMessage('Erro ao baixar a imagem: ' + IntToStr(HttpClient.ResponseStatusCode));
 finally
   HttpClient.Free;
   ImageStream.Free;
 end;
end;






procedure  TFrmPrincipal.processa(Msg: String);
var
  arr: TStringDynArray;
  i: Integer;
Begin
     arr := SplitString(Msg, '#');

     if ((arr[2] = 'E1')) then
          pbxL1.Visible := true;
     if ((arr[2] = 'S1')) then
          pbxL1.Visible := false;
     if ((arr[2] = 'E2')) then
        pbxL2.Visible := true;
     if ((arr[2] = 'S2'))  then
        pbxL2.Visible := false;

     //GetImage('http://admin:admin123@192.168.1.108/cgi-bin/snapshot.cgi?channel=1 ');

 //http://admin:admin123@192.168.1.108/cgi-bin/snapshot.cgi?channel=1
     if (arr[2] = 'S2') then
     Begin
       TTY('Captura Imagem...');
       if (bImgagem)   then
          captura('https://images-wixmp-ed30a86b8c4ca887773594c2.wixmp.com/f/ba48a25c-4624-484d-a0ee-5beffa93a1a0/de7m91a-62db1269-4290-4cb4-9fa7-67328d80b857.png/v1/fill/w_1032,h_774,q_70,strp/toyota_supra_by_94eliasdesing_de7m91a-pre.jpg?token=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ1cm46YXBwOjdlMGQxODg5ODIyNjQzNzNhNWYwZDQxNWVhMGQyNmUwIiwiaXNzIjoidXJuOmFwcDo3ZTBkMTg4OTgyMjY0MzczYTVmMGQ0MTVlYTBkMjZlMCIsIm9iaiI6W1t7ImhlaWdodCI6Ijw9MzAwMCIsInBhdGgiOiJcL2ZcL2JhNDhhMjVjLTQ2MjQtNDg0ZC1hMGVlLTViZWZmYTkzYTFhMFwvZGU3bTkxYS02MmRiMTI2OS00MjkwLTRjYjQtOWZhNy02NzMyOGQ4MGI4NTcucG5nIiwid2lkdGgiOiI8PTQwMDAifV1dLCJhdWQiOlsidXJuOnNlcnZpY2U6aW1hZ2Uub3BlcmF0aW9ucyJdfQ.Ii27mXhl2Rm1cl0e5YhCfGWgsXq_puzhkQkPf0a-6Hk')
       else
          captura('https://th.bing.com/th/id/R.2084d6e53f50514932ae4c468e2be2fb?rik=PS9WhKnywpRpgA&riu=http%3a%2f%2f2.bp.blogspot.com%2f-NXGtBkeIp54%2fVF-AKJCknXI%2fAAAAAAAAKyc%2fPRBF9mPyJkA%2fs1600%2fCarros%2b1.jpeg&ehk=D5m3IbRMrzyls5jXXo9Il3MD3WtmDQGfew7jRtjS3EU%3d&risl=&pid=ImgRaw&r=0');
          bImgagem := not bImgagem;
     end;
     if (arr[2] = 'MD') then
     Begin
        lblVelocidade.Caption := arr[3] + ' km/h' ;
        lblTamanho.Caption:= arr[4] + ' metros';
     end;
End;

procedure TFrmPrincipal.ServerReceive(aSocket: TLSocket);
var
  Data: string;
begin
     aSocket.GetMessage(Data);
     TTY(TimeToStr(Now) + ':'  +Data);
     processa(Data);
end;

end.

