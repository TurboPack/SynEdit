@echo off
setlocal enabledelayedexpansion
where msbuild >nul 2>&1 || (echo Error: msbuild not found. Run rsvars.bat first. & exit /b 1)
set PLATFORM=

echo.
echo ============================================
echo   Building ALL packages [Win32]
echo ============================================

cd /d "%~dp0Packages\11AndAbove\Delphi"

for %%P in (SynEditSharedDR SynEditDR SynEditDD SynEditFMXDR SynEditFMXDD) do (
    echo.
    echo === Delphi: %%P ===
    msbuild %%P.dproj /t:Clean /p:Config=Release /p:Platform=Win32 /v:quiet /nologo
    msbuild %%P.dproj /t:Build /p:Config=Release /p:Platform=Win32 /v:minimal /nologo
    if !ERRORLEVEL! NEQ 0 (
        echo FAILED: %%P
        exit /b 1
    )
    echo OK: %%P
)

cd /d "%~dp0Packages\11AndAbove\CBuilder"

for %%P in (SynEditSharedCR SynEditCR SynEditCD SynEditFMXCR SynEditFMXCD) do (
    echo.
    echo === C++: %%P ===
    msbuild %%P.cbproj /t:Clean /p:Config=Release /p:Platform=Win32 /v:quiet /nologo
    msbuild %%P.cbproj /t:Build /p:Config=Release /p:Platform=Win32 /v:minimal /nologo
    if !ERRORLEVEL! NEQ 0 (
        echo FAILED: %%P
        exit /b 1
    )
    echo OK: %%P
)

echo.
echo ============================================
echo   ALL WIN32 BUILDS SUCCEEDED
echo ============================================
