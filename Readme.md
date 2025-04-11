Sea RTL subset for Delphi 64bit

Object Pascal wrappers from Intel Integrated Performance Primitives and Intel Threading Building Blocks royalty-free packages

17 June 2019 Roberto Della Pasqua www.dellapasqua.com<br>
24 August 2022 DLL built with the latest stable Intel oneAPI and TBB ver. 2021.6<br>
10 January 2023 updated zlib to 1.2.13 with latest Intel IPP ver. 2021.7<br>
20 February 2023 updated webbroker deflate helper for reliability<br>
02 November 2023 updated oneTBB allocator<br>
8 August 2024 updated to Intel IPP v2021.12, zlib v1.3.1, visual c++ v19.29.30154<br>
30 October 2024 updated to Intel IPP v2022.0, visual c++ v19.41.34123<br>
09 April 2025 rem fillchar and move in case of Delphi 12.x (comes with asm optimized x64 functions)<br>
11 April 2025 updated to intel one api v2022.1, visual c++ v19.43.34810<br>

This folder contains:

- SeaMM.dll memory manager (md5 df1e5b489d2f9ac325d194a9dc67c8bc size 107520)
- SeaRTL.dll simd enabled rtl subset routines (md5 6f35648fbf2b386e3129ec82bb12d30d size 200704)
- SeaZIP.dll accelerated zlib compression (md5 5c4409f5c93f490119134bb5477a89fb size 982016)
- RDPMM64.pas wrapper for memory manager (put this unit as first unit clause in project source)
- RDPSimd64.pas wrapper for simd rtl api
- RDPZlib64.pas wrapper for zlib deflate (level -2 AC mode in deflate call should be used over UTF-8 strings for web optimization)<br>
- RDPWebBroker64.pas utils to enhance webbroker web apps<br> 
- SeaIISFilter ultra-fast realtime deflate filter for IIS web server (5x faster than default gzip)
- License.txt for legal terms
<br>
<p align="center"><img src="RobiMM.gif" width="50%"></p>
A common Delphi web framework tested with apachebench and 100 concurrent users (vm windows 2022 intel 9900k)<br>
(This enhancement is achieved in highly threaded apps doing concurrent memory and search operations)<br>
<br>
About zlib IPP accelerated: you can use symbolic name Z_IPP_FAST_COMPRESSION which is equal to -2<br>    
Introduced new sub-ranges of compression levels from 11 to 29<br>
The standard levels 1..9 are compatible with original Zlib<br>
Levels 11..19 are similar to 1..9, but fit better when compressing large files (more than 1 MB)<br>
Levels 21..29 are for highly compressible files with compression ratio equal to 30x and higher
<br><br>
If you want enable accelerated zlib programmatically into your WebBroker app, just add one line of code in afterdispatch event:

- procedure TWebModule.WebModuleAfterDispatch(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean); 
- begin 
- Response.ZlibDeflate;
- end;

The library is well tested, run on Intel and Amd x64 Windows, if you found any trouble please notify me;<br>
big thanks to the Delphi community and its great coders, in particular for the support received from Bruno Fierens [TMS](https://www.tmssoftware.com/) and Daniele Teti [DMVC](https://github.com/danieleteti/delphimvcframework).<br>

Contact me: roberto dot dellapasqua at live.com

Thank you and best regards

Roberto Della Pasqua
