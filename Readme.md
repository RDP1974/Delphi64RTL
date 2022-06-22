Sea RTL subset for Delphi 64bit

Object Pascal wrappers from Intel Integrated Performance Primitives and Intel Threading Building Blocks royalty-free packages

Copyright 17 June 2019 Roberto Della Pasqua (updated 9 March 2020)

This folder contains:

- SeaMM.dll lock-free scalable allocator
- SeaRTL.dll simd enabled rtl subset routines
- SeaZIP.dll accelerated zlib deflate compression
- RDPMM64.pas wrapper for memory manager (put this unit as first unit clause in project source)
- RDPSimd64.pas wrapper for simd rtl
- RDPZlib64.pas wrapper for zlib deflate
- RDPWebBroker64.pas utils to enhance webbroker web apps
- SeaIISFilter ultra-fast realtime deflate filter for IIS web server (5x faster than default gzip)
- License.txt for legal terms

The final user deployment is donateware. Please contact me roberto.dellapasqua@live.com or www.dellapasqua.com

A test with Indy, the built-in TCP Delphi library, on I7 cpu, shows an enhancement from 6934.29 ops/sec to 23097.68 ops/sec

Another test with WebBroker http compression, on I7 cpu, shows an enhancement from 147 pages/sec to 722 pages/sec

Another test with DMVC web api, on I9 cpu and windows 2016, simulating with apachebench 10000 requests and 100 users, shows an enhancement from 111 reqs/sec to 6448 reqs/sec

Another test, a ISAPI, on I9 cpu and windows 2016, doing in sequence DB query -> dataset of 1500 lines x 10 rows -> serialize to json string -> shrink it with deflate, is populating 2000 http reqs/sec, correctly filling all the cpu cores

If you want enable accelerated zlib programmatically into your WebBroker app, just add one line of code in afterdispatch event:

- procedure TWebModule.WebModuleAfterDispatch(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean); 
- begin 
- Response.ZlibDeflate; 
- end;

btw. rem // RedirectCode(@System.Move, @Move2); in RDPSimd64 if you have single threaded app with smallest ram allocations

The library is well tested, but if you found any trouble please notify me.

Thank you and best regards

Roberto Della Pasqua
