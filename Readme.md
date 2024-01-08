Sea RTL subset for Delphi 64bit

Object Pascal wrappers from Intel Integrated Performance Primitives and Intel Threading Building Blocks royalty-free packages

17 June 2019 Roberto Della Pasqua www.dellapasqua.com<br>
24 August 2022 DLL built with the latest stable Intel oneAPI and TBB ver. 2021.6<br>
10 January 2023 updated zlib to 1.2.13 with latest Intel IPP ver. 2021.7<br>
20 February 2023 updated webbroker deflate helper for reliability<br>
02 November 2023 updated oneTBB allocator<br>(oneTBB src.v2021.12.0 msvc_19.29_cxx_64_md_release)<br>(https://github.com/oneapi-src/oneTBB)

This folder contains:

- SeaMM.dll (ver. 2023 106496 bytes)(MD5 4363d696fbd2395529dee46607c06205)
- SeaRTL.dll simd enabled rtl subset routines (ver. 2022 201728 bytes)(MD5 b7fe56a81870b13b466816fe4e268d78)
- SeaZIP.dll accelerated zlib compression (ver. 2023 978944 bytes)(MD5 c916569b4e8e974d226c898164675989) 
- RDPMM64.pas wrapper for memory manager (put this unit as first unit clause in project source)
- RDPSimd64.pas wrapper for simd rtl
- RDPZlib64.pas wrapper for zlib deflate (level -2 AC mode in deflate call should be used over UTF-8 streams for web optimization, if you need to compress binary contents use level from 1 to 9 to avoid exceptions)<br>
- RDPWebBroker64.pas utils to enhance webbroker web apps<br> 
- SeaIISFilter ultra-fast realtime deflate filter for IIS web server (5x faster than default gzip)(will update it with a small project built over this extension)
- License.txt for legal terms

A test with Indy, the built-in TCP Delphi library, on I7 cpu, show an enhancement from 6934.29 ops/sec to 23097.68 ops/sec

Another test with WebBroker http compression, on I7 cpu, show an enhancement from 147 pages/sec to 722 pages/sec

Another test with DMVC web api, on I9 cpu and windows 2016, simulating with apachebench 10000 requests and 100 users, show an enhancement from 111 reqs/sec to 6448 reqs/sec

Another test, a ISAPI, on I9 cpu and windows 2016, doing in sequence DB query -> dataset of 1500 lines x 10 rows -> serialize to json string -> shrink it with deflate, is populating 2000 http reqs/sec, correctly filling all the cpu cores

Another WebBroker http app (Delphi 11) jump from 542 reqs/s to 3364 reqs/s (i9 cpu hyper-v windows 2022 server)(libs ver. 2022)

A simple web api console test with Horse (Delphi 12) jump from 2200 reqs/s to 37000 reqs/s (i9 cpu hyper-v windows 2022 server)(libs ver. 2023)

If you want enable accelerated zlib programmatically into your WebBroker app, just add one line of code in afterdispatch event:

- procedure TWebModule.WebModuleAfterDispatch(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean); 
- begin 
- Response.ZlibDeflate;
- end;

The library is well tested, run on Intel and Amd x64 Windows, if you found any trouble please notify me;<br>
big thanks to the Delphi community and its great coders<br>

Contact me: roberto dot dellapasqua at live.com

Thank you and best regards

Roberto Della Pasqua
