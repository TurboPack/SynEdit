@echo off
echo ========================================
echo  Building FMX SynEdit Tests
echo ========================================

call "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat"

echo.
echo Compiling Tests\FMX\FMXSynEditTests.dproj...
msbuild Tests\FMX\FMXSynEditTests.dproj /t:Build /p:Config=Debug /p:Platform=Win32 /v:minimal /nologo
if %ERRORLEVEL% NEQ 0 (
    echo.
    echo BUILD FAILED
    exit /b 1
)

echo.
echo ========================================
echo  Running Tests
echo ========================================
echo.
Tests\bin\Win32\Debug\FMXSynEditTests.exe --exit:continue
exit /b %ERRORLEVEL%
