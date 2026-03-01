@echo off
where msbuild >nul 2>&1 || (echo Error: msbuild not found. Run rsvars.bat first. & exit /b 1)
set PLATFORM=

if "%1"=="" (
    set PKG=SynEditSharedDR
) else (
    set PKG=%1
)

if "%2"=="" (
    set PLAT=Win32
) else (
    set PLAT=%2
)

echo.
echo ============================================
echo   Building Delphi: %PKG% [%PLAT%]
echo ============================================

cd /d "%~dp0Packages\11AndAbove\Delphi"
msbuild %PKG%.dproj /t:Clean /p:Config=Release /p:Platform=%PLAT% /v:quiet /nologo
msbuild %PKG%.dproj /t:Build /p:Config=Release /p:Platform=%PLAT% /v:minimal /nologo

echo.
echo EXIT CODE: %ERRORLEVEL%
