@echo off
where msbuild >nul 2>&1 || (echo Error: msbuild not found. Run rsvars.bat first. & exit /b 1)

echo.
echo ============================================
echo   Building FMX HighlighterDemo [Win32]
echo ============================================

cd /d "%~dp0Demos\FMX\HighlighterDemo"
msbuild FMXHighlighterDemo.dproj /t:Build /p:Config=Debug /p:Platform=Win32 /v:minimal /nologo
if %ERRORLEVEL% NEQ 0 goto :error

echo.
echo ============================================
echo   Building FMX EditApp [Win32]
echo ============================================

cd /d "%~dp0Demos\FMX\EditApp"
msbuild FMXEditApp.dproj /t:Build /p:Config=Debug /p:Platform=Win32 /v:minimal /nologo
if %ERRORLEVEL% NEQ 0 goto :error

echo.
echo ============================================
echo   Building FMX FeaturesDemo [Win32]
echo ============================================

cd /d "%~dp0Demos\FMX\FeaturesDemo"
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
