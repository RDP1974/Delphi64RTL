object DM: TDM
  OnCreate = DataModuleCreate
  Height = 2430
  Width = 3240
  PixelsPerInch = 216
  object WS: TipwWSServer
    LocalPort = 8080
    SSLCertStore = 'MY'
    OnConnected = WSConnected
    OnConnectionRequest = WSConnectionRequest
    OnDataIn = WSDataIn
    Left = 248
    Top = 120
  end
end
