@set FSC="C:\Program Files\Microsoft SDKs\F#\3.0\Framework\v4.0\fsc"
@set FSCOPT=--platform:x86 --nologo --standalone --lib:C:\Windows\Microsoft.NET\Framework\v4.0.30319
%FSC% %FSCOPT% --target:winexe --out:V6FS.exe Main.fsx
