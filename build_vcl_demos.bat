@echo off
call "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat"
set PLATFORM=
set FAIL=0
set EXTRA_PATHS=..\..\..\Source;..\..\..\Source\VCL;..\..\..\Source\Highlighters

for %%D in (
    "Demos\VCL\CompletionProposalDemo\CompletionProposalDemo.dproj"
    "Demos\VCL\CompletionProposalDemo\ParamCompletionDemo.dproj"
    "Demos\VCL\EditAppDemos\EditAppMDI.dproj"
    "Demos\VCL\EditAppDemos\EditAppSDI.dproj"
    "Demos\VCL\EditAppDemos\EditAppWorkbook.dproj"
    "Demos\VCL\Folding\FoldingDemo.dproj"
    "Demos\VCL\HighlighterDemo\HighlighterDemo.dproj"
    "Demos\VCL\MarkdownViewer\MarkdownViewerDemo.dproj"
    "Demos\VCL\PrintDemo\TestPP.dproj"
    "Demos\VCL\SearchReplaceDemo\SearchReplaceDemo.dproj"
    "Demos\VCL\SimpleIDEDemo\SimpleIDEDemo.dproj"
    "Demos\VCL\SpellCheck\SpellCheck.dproj"
) do (
    echo.
    echo ============================================
    echo   Building: %%~nD
    echo ============================================
    cd /d "D:\Documents\SynEdit"
    msbuild %%D /t:Build /p:Config=Release /p:Platform=Win32 /p:DCC_UnitSearchPath="%EXTRA_PATHS%" /p:DCC_IncludePath="%EXTRA_PATHS%" /v:minimal /nologo
    if errorlevel 1 (
        echo   FAILED: %%~nD
        set FAIL=1
    ) else (
        echo   OK: %%~nD
    )
)

echo.
echo ============================================
if "%FAIL%"=="1" (
    echo   SOME BUILDS FAILED
) else (
    echo   ALL BUILDS SUCCEEDED
)
echo ============================================
