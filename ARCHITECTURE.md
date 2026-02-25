# SynEdit Architecture

## Overview

SynEdit is a syntax-highlighting editor component for Delphi and C++ Builder.
It supports both **VCL** (Windows) and **FMX** (cross-platform) frameworks
through a three-layer architecture.

## Three-Layer Architecture

```
┌──────────────────────────┐  ┌──────────────────────────┐
│  Source/VCL/              │  │  Source/FMX/              │
│  Vcl.SynEdit.pas         │  │  FMX.SynEdit.pas          │
│  Vcl.SynDWrite.pas       │  │  FMX.SynEditRenderer.pas  │
│  Vcl.SynEditMiscClasses  │  │  FMX.SynEditMiscClasses   │
│  (36 units)              │  │  (11 units)               │
└────────────┬─────────────┘  └────────────┬──────────────┘
             │                              │
             └──────────┬──────────────────-┘
                        │  uses
             ┌──────────▼──────────┐
             │  Source/ (shared)    │
             │  SynEditTypes.pas   │
             │  SynEditHighlighter │
             │  SynEditTextBuffer  │
             │  66 highlighters    │
             │  189 Omni configs   │
             └─────────────────────┘
```

Each unit belongs to exactly one layer:

- **Shared** (no prefix): Platform-independent units consumed by both VCL and FMX.
  Found by exact match — no scope resolution needed.
- **VCL** (`Vcl.*` prefix): Windows-specific implementations using VCL controls,
  DirectWrite, OLE, Windows API.
- **FMX** (`FMX.*` prefix): Cross-platform implementations using FireMonkey
  controls and canvas.

## How Unit Scope Resolution Works

Delphi 12+ projects have **Unit Scope Names** configured in the project:
- VCL projects include `Vcl` in their scope names
- FMX projects include `FMX` in their scope names

When existing code says `uses SynEdit;`, the compiler searches:
1. Exact match: `SynEdit.pas` — not found (renamed to `Vcl.SynEdit.pas`)
2. Scope prefix: `Vcl.SynEdit.pas` — found in VCL projects
3. Scope prefix: `FMX.SynEdit.pas` — found in FMX projects

When code says `uses SynEditTypes;`, the compiler finds:
1. Exact match: `SynEditTypes.pas` — found (shared unit)

This means existing VCL code compiles unchanged. The same `uses SynEdit;`
statement resolves to the FMX version in FMX projects.

## Directory Structure

```
SynEdit/
  Source/
    SynEditTypes.pas              Shared types (TColor via System.UITypes)
    SynEditHighlighter.pas        Shared highlighter base class
    SynEditTextBuffer.pas         Shared text storage
    SynEditCodeFolding.pas        Shared code folding logic
    SynEditKeyCmds.pas            Shared key command constants
    SynEditMiscProcs.pas          Shared utility functions
    SynEditSearch.pas             Shared search engine
    SynEditRegexSearch.pas        Shared regex search
    SynEditWildcardSearch.pas     Shared wildcard search
    SynEditStrConst.pas           Shared string constants
    SynUnicode.pas                Shared encoding utilities
    SynEdit.inc                   Compiler directives
    Highlighters/
      SynHighlighter*.pas         66 language highlighters (shared)
      Omni Highlighters/          189 Omni highlighter configs (.ini)
    VCL/
      Vcl.SynEdit.pas             VCL editor (TCustomSynEdit : TCustomControl)
      Vcl.SynDWrite.pas           DirectWrite text rendering
      Vcl.SynEditTypes.pas        VCL-specific type extensions
      Vcl.SynEditMiscClasses.pas  VCL gutter, glyphs, bookmarks
      Vcl.SynEditScrollBars.pas   Native Windows scrollbars
      Vcl.SynEditKeyConst.pas     VCL key constant mapping
      Vcl.SynEditKbdHandler.pas   VCL keyboard/mouse event chains
      Vcl.SynEditUndo.pas         VCL undo/redo system
      Vcl.SynCompletionProposal   VCL code completion popup
      Vcl.SynEditDragDrop.pas     OLE drag-and-drop
      Vcl.SynEditDataObject.pas   OLE IDataObject
      Vcl.SynAccessibility.pas    Windows UI Automation
      Vcl.SynEditPrint*.pas       Windows printing support
      Vcl.SynDBEdit.pas           Database-aware editor
      Vcl.SynSpellCheck.pas       Windows spell-check COM
      Vcl.SynAutoCorrect*.pas     Auto-correction with VCL dialogs
      Vcl.SynMacroRecorder.pas    Macro recording
      Vcl.SynEditPlugins.pas      VCL plugin framework
      Vcl.SynURIOpener.pas        URI detection and opening
      Vcl.SynEditExport.pas       Base exporter
      Vcl.SynExportHTML.pas       HTML export
      Vcl.SynExportRTF.pas        RTF export
      Vcl.SynExportTeX.pas        TeX export
      Vcl.SynEditReg.pas          VCL component registration
      Vcl.SynEditPropertyReg.pas  VCL property editors
      DesignTimeEditors/          IDE importer tools
    FMX/
      FMX.SynEdit.pas             FMX editor (TCustomFMXSynEdit : TControl)
      FMX.SynEditRenderer.pas     FMX Canvas text rendering
      FMX.SynEditTypes.pas        FMX-specific type extensions
      FMX.SynEditMiscClasses.pas  FMX utility classes
      FMX.SynEditScrollBars.pas   FMX TScrollBar integration
      FMX.SynEditKeyConst.pas     FMX key constant mapping
      FMX.SynEditKbdHandler.pas   FMX keyboard/mouse event chains
      FMX.SynEditUndo.pas         FMX undo/redo system
      FMX.SynUnicode.pas          FMX clipboard (IFMXClipboardService)
      FMX.SynCompletionProposal   FMX code completion (TPopup-based)
      FMX.SynEditReg.pas          FMX component registration
  Packages/
    11AndAbove/
      Delphi/
        SynEditSharedDR.dpk       Shared runtime package
        SynEditDR.dpk             VCL runtime package
        SynEditDD.dpk             VCL designtime package
        SynEditFMXDR.dpk          FMX runtime package
        SynEditFMXDD.dpk          FMX designtime package
        SynEditDelphi.groupproj   All Delphi packages
      CBuilder/
        SynEditSharedCR.cbproj    Shared C++ runtime package
        SynEditCR.cbproj          VCL C++ runtime package
        SynEditCD.cbproj          VCL C++ designtime package
        SynEditFMXCR.cbproj       FMX C++ runtime package
        SynEditFMXCD.cbproj       FMX C++ designtime package
        SynEditCBuilder.groupproj All C++ packages
  Demos/
    VCL/                          VCL demo applications
    FMX/                          FMX demo applications
    uHighlighterProcs.pas         Shared demo helper
    SynEditDemosGroup.groupproj   All demos
```

## Package Dependencies

```
SynEditSharedDR (requires rtl)
  Contains: shared Source/*.pas + 66 highlighters
       ↑                       ↑
SynEditDR                SynEditFMXDR
(requires vcl, vcldb,    (requires fmx, rtl,
 vclx, rtl,               SynEditSharedDR)
 SynEditSharedDR)
       ↑                       ↑
SynEditDD                SynEditFMXDD
(requires designide,     (requires designide,
 SynEditDR)               SynEditFMXDR)
```

C++ Builder packages mirror the Delphi structure:
`SynEditSharedCR` → `SynEditCR`/`SynEditFMXCR` → `SynEditCD`/`SynEditFMXCD`

## Key Design Decisions

### TColor Portability

`TColor` is defined in `System.UITypes`, available on all platforms.
`Vcl.Graphics` merely re-exports it. All shared code (including highlighters)
uses `System.UITypes` for `TColor`. The FMX rendering layer converts
`TColor` to `TAlphaColor` at the paint boundary.

### Highlighter Sharing

All 66 language highlighters are pure tokenization logic. They reference
`TColor` (via `System.UITypes`) and `TSynHighlighterAttributes` (via
`SynEditHighlighter`). Windows-specific imports like `Registry` are
wrapped in `{$IFDEF MSWINDOWS}`. This allows the same highlighter files
to be used by both VCL and FMX editors.

### VCL vs FMX Editor Base Class

| Aspect | VCL | FMX |
|--------|-----|-----|
| Base class | `TCustomControl` | `TControl` |
| Rendering | DirectWrite (`Vcl.SynDWrite`) | FMX Canvas (`FMX.SynEditRenderer`) |
| Scrollbars | Native Windows (`WM_HSCROLL`/`WM_VSCROLL`) | FMX `TScrollBar` components |
| Input | Windows messages (`WM_KEYDOWN`, etc.) | FMX event overrides (`KeyDown`, etc.) |
| Clipboard | Windows API | `IFMXClipboardService` |
| Drag-drop | OLE `IDropTarget`/`IDropSource` | FMX `DragEnter`/`DragDrop` |
| Accessibility | Windows UI Automation | Not yet implemented |
| Printing | Windows GDI/printer API | Not applicable |

### FMX Message Handler Mapping

| VCL `WM_*` | FMX equivalent |
|-------------|----------------|
| `WM_PAINT` | `Paint` override |
| `WM_SIZE` | `Resize` override |
| `WM_SETFOCUS`/`WM_KILLFOCUS` | `DoEnter`/`DoExit` |
| `WM_KEYDOWN`/`WM_KEYUP` | `KeyDown`/`KeyUp` overrides |
| `WM_MOUSE*` | `MouseDown`/`MouseMove`/`MouseUp`/`MouseWheel` |
| `WM_HSCROLL`/`WM_VSCROLL` | FMX `TScrollBar.OnChange` |
| `WM_COPY`/`WM_CUT`/`WM_PASTE` | Editor commands via `KeyDown` |
| `WM_ERASEBKGND` | N/A (FMX handles it) |

## Building

### Prerequisites

- Delphi 12+ (RAD Studio 12+)
- Packages must be built in dependency order

### Build Order (Delphi)

```
1. SynEditSharedDR   (shared runtime)
2. SynEditDR         (VCL runtime)
3. SynEditDD         (VCL designtime)
4. SynEditFMXDR      (FMX runtime)
5. SynEditFMXDD      (FMX designtime)
```

### Build Order (C++ Builder)

```
1. SynEditSharedCR   (shared runtime)
2. SynEditCR         (VCL runtime)
3. SynEditCD         (VCL designtime)
4. SynEditFMXCR      (FMX runtime)
5. SynEditFMXCD      (FMX designtime)
```

### Platform Support

| Platform | Delphi | C++ Builder |
|----------|--------|-------------|
| Win32    | All OK | All OK |
| Win64    | All OK | Known ilink64 generics limitation |
| Win64x   | All OK | Toolchain init_record mismatch |

## For Contributors

### Adding a New Shared Unit

1. Place in `Source/` (no prefix)
2. Add to `SynEditSharedDR.dpk` contains clause
3. Add to `SynEditSharedCR.cbproj` source list
4. Use only `System.*` and `SynEdit*` (no-prefix) units

### Adding a New VCL Unit

1. Place in `Source/VCL/` with `Vcl.` prefix
2. Add to `SynEditDR.dpk` contains clause
3. Add to `SynEditCR.cbproj` source list
4. Can reference both shared units and other `Vcl.*` units

### Adding a New FMX Unit

1. Place in `Source/FMX/` with `FMX.` prefix
2. Add to `SynEditFMXDR.dpk` contains clause
3. Add to `SynEditFMXCR.cbproj` source list
4. Can reference both shared units and other `FMX.*` units

### Unit Naming Rule

A unit is **never** both shared AND scope-resolved. If a unit has a `Vcl.*`
or `FMX.*` counterpart, it must not exist as a bare name in `Source/`. If a
unit is shared, it must not have a prefixed counterpart.
