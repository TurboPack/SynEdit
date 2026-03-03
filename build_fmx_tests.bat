@echo off
where msbuild >nul 2>&1 || (echo Error: msbuild not found. Run rsvars.bat first. & exit /b 1)

echo ========================================
echo  Building FMX SynEdit Tests
echo ========================================

echo.
echo Compiling Tests\FMX\FMXSynEditTests.dproj...
cd /d "%~dp0"
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
