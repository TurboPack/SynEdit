@echo off
call "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat"

echo.
echo ============================================
echo   Building FMX HighlighterDemo [Win32]
echo ============================================

cd /d "D:\Documents\SynEdit\Demos\FMX\HighlighterDemo"
msbuild FMXHighlighterDemo.dproj /t:Build /p:Config=Debug /p:Platform=Win32 /v:minimal /nologo
if %ERRORLEVEL% NEQ 0 goto :error

echo.
echo ============================================
echo   Building FMX EditApp [Win32]
echo ============================================

cd /d "D:\Documents\SynEdit\Demos\FMX\EditApp"
msbuild FMXEditApp.dproj /t:Build /p:Config=Debug /p:Platform=Win32 /v:minimal /nologo
if %ERRORLEVEL% NEQ 0 goto :error

echo.
echo ============================================
echo   Building FMX FeaturesDemo [Win32]
echo ============================================

cd /d "D:\Documents\SynEdit\Demos\FMX\FeaturesDemo"
msbuild FMXFeaturesDemo.dproj /t:Build /p:Config=Debug /p:Platform=Win32 /v:minimal /nologo
if %ERRORLEVEL% NEQ 0 goto :error

echo.
echo ============================================
echo   All FMX demos built successfully!
echo ============================================
goto :end

:error
echo.
echo BUILD FAILED (exit code %ERRORLEVEL%)

:end
