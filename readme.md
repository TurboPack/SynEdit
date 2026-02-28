# TurboPack SynEdit

A syntax-highlighting editor component for Delphi and C++ Builder, supporting both **VCL** (Windows) and **FMX** (cross-platform) frameworks.

Compatible with Delphi 12 Athens or later. You can also access the [11 Alexandria](https://github.com/TurboPack/SynEdit/tree/11Alexandria), [10.3 Rio](https://github.com/TurboPack/SynEdit/releases/tag/103RIO), [10.2 Tokyo](https://github.com/TurboPack/SynEdit/releases/tag/102Tokyo) and [10.1 Berlin](https://github.com/TurboPack/SynEdit/releases/tag/101Berlin) releases.

### Table of contents
1. [Introduction](#introduction)
2. [What's New](#whats-new)
3. [Architecture](#architecture)
4. [Package Names](#package-names)
5. [Installation](#installation)
6. [Demos](#demos)
7. [Testing](#testing)
8. [Building from Source](#building-from-source)

---

## Introduction

SynEdit is a syntax highlighting edit control, not based on the Windows common controls. It includes:

- **66 language highlighters** (Delphi, C++, Python, JavaScript, HTML, XML, SQL, and many more)
- **189 Omni highlighter configurations** for additional languages via INI-based definitions
- **VCL editor** (`TCustomSynEdit`) — full-featured Windows editor with DirectWrite rendering, code folding, completion proposals, printing, spell check, OLE drag-drop, and accessibility
- **FMX editor** (`TCustomFMXSynEdit`) — cross-platform editor with FMX Canvas rendering, syntax highlighting, keyboard input, selection, clipboard, undo/redo, file I/O, code folding, search/replace, completion proposals, plugin support, printing (abstract provider), and spell check (abstract provider)

All highlighters are shared between VCL and FMX — write once, highlight everywhere.

---

## What's New

See [What's new.md](What's%20new.md) for detailed information on recent additions including:

- **TSynDelphiSyn** — Modern Delphi highlighter with multiline string literals, code folding, and Delphi 13 keyword support
- **IDE Settings Importer** — Design-time tool to import your Delphi IDE color scheme and editor preferences into SynEdit components
- **FMX Editor** — Cross-platform FireMonkey editor with syntax highlighting, clipboard, undo/redo, file I/O, code folding, search/replace, completion proposals, printing, spell check, and plugin support

---

## Architecture

SynEdit uses a **three-layer architecture**:

```
┌────────────────────┐  ┌────────────────────┐
│  Source/VCL/        │  │  Source/FMX/        │
│  36 Vcl.* units     │  │  18 FMX.* units     │
└─────────┬──────────┘  └─────────┬──────────┘
          └──────┬───────────────-┘
                 │ uses
      ┌──────────▼──────────┐
      │  Source/ (shared)    │
      │  66 highlighters     │
      │  189 Omni configs    │
      │  Core types & buffer │
      └─────────────────────┘
```

- **Shared** (no prefix): Platform-independent units — highlighters, text buffer, types, key commands, spell-check providers
- **VCL** (`Vcl.*` prefix): Windows-specific — DirectWrite, OLE, printing, accessibility
- **FMX** (`FMX.*` prefix): Cross-platform — FMX Canvas rendering, FMX scrollbars, FMX clipboard

See [ARCHITECTURE.md](ARCHITECTURE.md) for the full technical reference including directory structure, package dependencies, unit scope resolution, build order, and contributor guidelines.

---

## Package Names

### Delphi

| Package | Type | Description |
|---------|------|-------------|
| SynEditSharedDR | Runtime | Shared core — types, text buffer, 66 highlighters |
| SynEditDR | Runtime | VCL editor and supporting units |
| SynEditDD | Designtime | VCL component registration and property editors |
| SynEditFMXDR | Runtime | FMX editor and supporting units |
| SynEditFMXDD | Designtime | FMX component registration |

### C++ Builder

| Package | Type | Description |
|---------|------|-------------|
| SynEditSharedCR | Runtime | Shared core |
| SynEditCR | Runtime | VCL editor |
| SynEditCD | Designtime | VCL component registration |
| SynEditFMXCR | Runtime | FMX editor |
| SynEditFMXCD | Designtime | FMX component registration |

Build order: **Shared** → **VCL/FMX Runtime** → **VCL/FMX Designtime**

---

## Installation

TurboPack SynEdit is available via the [GetIt Package Manager](http://docwiki.embarcadero.com/RADStudio/en/Installing_a_Package_Using_GetIt_Package_Manager) where you can quickly and easily install and uninstall it.

To manually install into your IDE:

1. Clone or unzip into a directory (e.g., `d:\SynEdit`).

2. Start RAD Studio.

3. Add these directories to the IDE's library path:
   - `Source` — shared units
   - `Source\Highlighters` — language highlighters
   - `Source\VCL` — VCL editor units
   - `Source\FMX` — FMX editor units (if using FMX)

   For C++ Builder, also add the hpp subdirectory (e.g., `Source\hpp\Win32\Release`) to the IDE's system include path.

4. Open and install the designtime packages from `Packages\11AndAbove\`:
   - **VCL**: Build `SynEditSharedDR.dpk`, then `SynEditDR.dpk`, then install `SynEditDD.dpk`
   - **FMX**: Build `SynEditFMXDR.dpk`, then install `SynEditFMXDD.dpk`

---

## Demos

### VCL Demos (`Demos/VCL/`)

| Demo | Description |
|------|-------------|
| HighlighterDemo | Browse all language highlighters with sample source |
| EditAppDemos | SDI, MDI, and Workbook editors with file I/O and search/replace |
| CompletionProposalDemo | Code completion popup |
| Folding | Code folding with Delphi highlighter |
| SearchReplaceDemo | Find and replace functionality |
| SimpleIDEDemo | Mini IDE with editor and output pane |
| PrintDemo | Printing support |
| MarkdownViewer | Markdown rendering |
| SpellCheck | Windows spell-check integration |

### FMX Demos (`Demos/FMX/`)

| Demo | Description |
|------|-------------|
| HighlighterDemo | Browse 13 language highlighters with syntax coloring |
| EditApp | Single-document editor with menus, file I/O, clipboard, undo/redo, and auto highlighter detection |
| FeaturesDemo | Comprehensive feature showcase with options panel, search/replace, completion proposals, code folding, and event log |

---

## Testing

Two DUnitX test suites run headless with `FailsOnNoAsserts` enabled and exact-value assertions throughout.

- **FMX** — **186 tests** across 16 fixtures covering buffer, caret, code folding, commands, content, highlighter, options, search, undo/redo, spell check, bug-fix regressions, selection, clipboard, editing edge cases, color conversion, and completion proposal logic.
- **VCL** — **47 tests** across 3 fixtures covering Hunspell provider, Windows spell-check COM provider, and spell check component integration.

```
build_fmx_tests.bat
```

Test projects: `Tests/FMX/FMXSynEditTests.dproj`, `Tests/VCL/VCLSynEditTests.dproj`

---

## Building from Source

### Prerequisites

- Delphi 12+ (RAD Studio 12+)
- Packages must be built in dependency order (see [ARCHITECTURE.md](ARCHITECTURE.md#building))

### Build Scripts

| Script | Description |
|--------|-------------|
| `build_delphi.bat` | Build all Delphi packages (shared + VCL + FMX) |
| `build_all_win32.bat` | Build all packages for Win32 (Delphi + C++ Builder) |
| `build_vcl_demos.bat` | Build all VCL demo applications |
| `build_fmx_demo.bat` | Build FMX demo applications |
| `build_fmx_tests.bat` | Build FMX DUnitX test suite |

### Quick Build (Delphi command line)

```
rsvars.bat
msbuild SynEditSharedDR.dproj /t:Build /p:Config=Release /p:Platform=Win32
msbuild SynEditDR.dproj /t:Build /p:Config=Release /p:Platform=Win32
msbuild SynEditFMXDR.dproj /t:Build /p:Config=Release /p:Platform=Win32
```

### Platform Support

| Platform | Delphi | C++ Builder |
|----------|--------|-------------|
| Win32 | All packages | All packages |
| Win64 | All packages | Known ilink64 generics limitation |

---

## License

See [LICENSE](LICENSE) for details. SynEdit is dual-licensed under the MPL 1.1 and LGPL 2.1+.
