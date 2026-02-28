# What's New in SynEdit

---

## **FMX Cross-Platform Editor**

SynEdit now supports **FireMonkey (FMX)** alongside VCL through a three-layer architecture that shares highlighters and core logic between both frameworks.

### Three-Layer Architecture

The codebase has been restructured into three layers:

- **Shared** (`Source/`) — Platform-independent units: 66 highlighters, text buffer, types, key commands, spell-check infrastructure (Hunspell and Windows providers). These are consumed by both VCL and FMX.
- **VCL** (`Source/VCL/`, `Vcl.*` prefix) — Windows-specific: DirectWrite rendering, OLE drag-drop, printing, accessibility.
- **FMX** (`Source/FMX/`, `FMX.*` prefix) — Cross-platform: FMX Canvas rendering, FMX scrollbars, FMX clipboard.

See [ARCHITECTURE.md](ARCHITECTURE.md) for the full technical reference.

### FMX Editor (`TCustomFMXSynEdit`)

**Unit:** `Source\FMX\FMX.SynEdit.pas`

**Inheritance:** `TControl`

A cross-platform syntax-highlighting editor built on FireMonkey. Supports:

* **Syntax highlighting** — All 66 shared highlighters work out of the box.
* **Keyboard input** — Full text editing with insert/overwrite modes.
* **Selection** — Mouse and keyboard selection with correct split-token rendering at selection boundaries.
* **Clipboard** — Cut, Copy, Paste via `IFMXClipboardService`.
* **Undo/Redo** — Full undo/redo stack.
* **File I/O** — `LoadFromFile`, `SaveToFile`, `LoadFromStream`, `SaveToStream`.
* **Code folding** — Collapse and expand foldable regions.
* **Search/Replace** — `SearchReplace` method with options for case, whole word, regex, selection-only, and replace all.
* **Completion proposals** — `TFMXSynCompletionProposal` popup with keyboard navigation, filtering, and customizable display.
* **Plugin support** — `TSynFMXEditPlugin` base class for extending the editor via `OnCommand` hooks.
* **Printing** — `TFMXSynEditPrint` with abstract provider interface for platform-specific rendering.
* **Spell check** — `TFMXSynSpellCheck` with shared provider infrastructure (`ISynSpellCheckProvider`). Includes Hunspell and Windows spell-check providers usable by both VCL and FMX.
* **Range scanning** — Incremental re-scanning for multi-line highlighters (XML, HTML, Delphi, etc.).
* **Scrolling** — FMX `TScrollBar`-based scrolling with mouse wheel support.
* **Gutter** — Line numbers with configurable width.
* **Right edge** — Configurable right margin indicator.
* **Active line highlighting** — Configurable active line background color.
* **Editor options** — Auto indent, smart tabs, tabs-to-spaces, and more via `TSynEditorOptions`.

### New Packages

Five new packages support the three-layer architecture:

| Package | Description |
| :---- | :---- |
| SynEditSharedDR / SynEditSharedCR | Shared runtime — types, text buffer, 66 highlighters |
| SynEditFMXDR / SynEditFMXCR | FMX runtime — FMX editor and supporting units |
| SynEditFMXDD / SynEditFMXCD | FMX designtime — component registration |

Build order: Shared → VCL/FMX Runtime → VCL/FMX Designtime.

### FMX Demos

Three FMX demos are included in `Demos/FMX/`:

* **HighlighterDemo** — Browse 13 language highlighters (Delphi, C++, Java, Python, JavaScript, HTML, XML, CSS, SQL, JSON, C#, INI, Batch) with a consistent color scheme.
* **EditApp** — Single-document editor with menus (File/Edit), status bar, file I/O, clipboard, undo/redo, and automatic highlighter detection from file extension.
* **FeaturesDemo** — Comprehensive feature showcase with a controls panel, editor options toggles, search/replace dialogs, completion proposals, code folding, clipboard buttons, active line color picker, and a timestamped event log.

### Test Suites

Two DUnitX test suites run headless with `FailsOnNoAsserts` enabled and exact-value assertions throughout.

**FMX tests** (`Tests/FMX/FMXSynEditTests.dproj`) — **290 tests**, 25 fixtures:

| Fixture | Tests | Coverage area |
| :------ | ----: | :------------ |
| Buffer | 10 | Line add/delete/insert, text property |
| Caret | 10 | Positioning, SelectAll, GetTextRange |
| CodeFolding | 6 | Fold detection, collapse/uncollapse, levels |
| Commands | 14 | Char insert/delete, line break, tab, navigation |
| Content | 8 | Text get/set, multi-line, stream round-trip |
| Highlighter | 5 | Assignment, free notification, switching |
| Options | 10 | Default options, read-only, tab width, right edge |
| Search | 9 | Case, whole-word, regex, replace, replace-all |
| UndoRedo | 9 | Availability, restore, multiple undo/redo, redo caret |
| SpellCheck | 20 | Hunspell provider, suffix/prefix rules, suggest |
| WindowsSpellCheck | 11 | Windows spell-check COM provider |
| SpellCheckComponent | 17 | TSynSpellCheck component integration, selection dedup |
| BugFixes | 23 | Plugin registration, Modified, nil width, tabs, keyboard chain |
| AutoIndentTabs | 4 | Tab preservation, mixed whitespace, disable option |
| PixelToBufferCoord | 4 | Click mapping at/near char boundaries, gutter clamp |
| ScrollBarSizing | 3 | LinesInWindow/CharsInWindow delta, scrollbar hidden |
| Selection | 18 | ecSel* commands, accumulation, collapse, replace, line-boundary |
| Clipboard | 10 | Copy/cut/paste, read-only guards, undo support |
| Editing | 12 | Line joining, overwrite mode, OnChange, BeginUpdate |
| Renderer | 11 | TColorToAlphaColor byte-swap, SysNone, metrics |
| CompletionProposal | 12 | AddItem/ClearList, filtering, position, MoveLine |
| DelphiFolding | 26 | Procedure/class/record/interface folding, class var exclusion |
| HTMLFolding | 11 | Tag pairs, void elements, comments, multi-line tags |
| XMLFolding | 14 | Elements, namespaces, PI/CDATA/DOCTYPE, multi-line tags |
| CSSFolding | 13 | Brace folding, nested @media, comments, strings |

**VCL tests** (`Tests/VCL/VCLSynEditTests.dproj`) — **47 tests**, 3 fixtures:

| Fixture | Tests | Coverage area |
| :------ | ----: | :------------ |
| SynSpellCheck | 20 | Hunspell provider, suffix/prefix rules, suggest |
| WindowsSpellCheck | 11 | Windows spell-check COM provider |
| SpellCheckComponent | 16 | TSynSpellCheck component integration |

---

## **TSynDelphiSyn & IDE Settings Importer**

A new updated Delphi syntax highlighter for SynEdit (TSynDelphiSyn) and a design-time tool (SynDelphiIDEImporter) to synchronize your SynEdit components with your current Delphi IDE configuration.

## **TSynDelphiSyn Component**

**Unit:** `Source\Highlighters\SynHighlighterDelphi.pas`

**Inheritance:** `TSynCustomCodeFoldingHighlighter`

`TSynDelphiSyn` is a specialized syntax highlighter designed to support the modern Object Pascal dialect used in Delphi 13 (Athens). It is based on the standard Pascal highligher `TSynPascalSyn`, but extends it with support for new language features, and expanded keywords.

### **Key Features**

* **Delphi 13 Language Support**: Includes support for modern keywords such as reference, helper, operator, strict, sealed, final, delayed, and more.  
* **Multiline String Literals**: Native support for Delphi's triple-quoted strings (''' ... ''').  
* **Code Folding**: Built-in folding logic for:
  * {$REGION} / {$ENDREGION} directives.
  * interface / implementation sections (each folds independently).
  * Classes, Records, and Methods (procedure, function, constructor, destructor fold from the header line).
  * Standard blocks (begin..end, case, try).
  * `class var`, `class function`, `class procedure`, `class constructor`, `class destructor`, and `class operator` are correctly excluded from opening spurious folds.  
* **Performance**: Uses a binary search algorithm for fast keyword lookups and class-cached Regular Expressions for folding logic.

### **Attributes**

The component exposes standard `TSynHighlighterAttributes` for customization:

| Property | Description |
| :---- | :---- |
| AsmAttri | Assembler blocks (asm ... end). |
| CommentAttri | Comments (//, { }, (\* \*)). |
| DirectiveAttri | Compiler directives (e.g., {$IFDEF}). |
| IdentifierAttri | Variable, method, and class names. |
| KeyAttri | Reserved words and keywords. |
| NumberAttri | Integer literals. |
| FloatAttri | Floating-point literals. |
| HexAttri | Hexadecimal values (e.g., $FF). |
| StringAttri | String literals, including multiline strings. |
| CharAttri | Character literals (e.g., \#13). |
| SymbolAttri | Operators and punctuation (:, \=, \+). |
| SpaceAttri | Whitespace. |

---

## **2\. Import IDE Settings**

**Unit:** `SynDelphiIDEImporter.pas`

This feature is a **Design-Time Component Editor** that allows you to instantly apply your active Delphi IDE preferences to your SynEdit components. This ensures your editor control looks and behaves exactly like the Delphi code editor you are using.

***Notice:*** These settings are imported from the *registry*, so if you changed your IDE settings and haven't closed the IDE the imported values may be out of date.

### **Usage:**

1. Open the Form Designer in Delphi.  
2. Select a TSynEdit or TSynDelphiSyn component.  
3. **Right-click** the component to open the context menu.  
4. Select **"Import IDE Settings"**.

### **Importing on TSynEdit:**

Imports general editor behavior and layout settings:

* **Font**: Imports Name and Size.  
* **Gutter**: Visibility, Color, and Line Number visibility.  
* **Margins**: Right Edge position and visibility.  
* **Tabs & Indentation**: Tab Width, Auto Indent, Smart Tabs, and Tabs-to-Spaces settings.  
* **Scroll**: "Cursor Beyond EOF" settings (mapped to eoScrollPastEol/eoScrollPastEof).  
* **Colors**: Main Background color, Active Line color, and Right Edge color.

#### Settings imported

| Registry Key Path (Relative to Base) | Registry Value | Component Property | Notes |
| :---- | :---- | :---- | :---- |
| Editor\\Options | Editor Font | Font.Name |  |
| Editor\\Options | Font Size | Font.Size |  |
| Editor\\Options | Insert | InsertMode | True/False string converted to Boolean. |
| Editor\\Options | Visible Gutter | Gutter.Visible |  |
| Editor\\Options | Show Line Numbers | Gutter.ShowLineNumbers |  |
| Editor\\Options | Right Margin | RightEdge | Only set if Visible Right Margin is also true; otherwise set to 0\. |
| Editor\\Options | Group Undo | Options | Adds/Removes eoGroupUndo. |
| Editor\\Options | Highlight Brace Pairs | Options | Adds/Removes eoBracketsHighlight. |
| Editor\\Options | Drop Files | Options | Adds/Removes eoDropFiles. |
| Editor\\Options | Cursor Beyond EOF | ScrollOptions | Adds/Removes both eoScrollPastEol and eoScrollPastEof. |
| Editor\\Source Options\\Borland.EditOptions.Pascal | Tab Stops | TabWidth | Defaults to 4 if invalid. |
| Editor\\Source Options\\Borland.EditOptions.Pascal | Auto Indent | Options | Adds/Removes eoAutoIndent. |
| Editor\\Source Options\\Borland.EditOptions.Pascal | Smart Tab | Options | Adds/Removes eoSmartTabs. |
| Editor\\Source Options\\Borland.EditOptions.Pascal | Tab Character | Options | Inverted logic: If True, eoTabsToSpaces is removed (use real tabs). If False, eoTabsToSpaces is added. |

### Colors Imported

| Registry Key Path | Registry Value | Component Property | Description |
| :---- | :---- | :---- | :---- |
| Editor\\Highlight\\Whitespace | Background Color New | Color | Uses the "Whitespace" background as the main editor background. |
| Editor\\Highlight\\Line Highlight | Background Color New | ActiveLineColor | Color of the current line highlight. |
| Editor\\Highlight\\Right margin | Foreground Color New | RightEdgeColor | Color of the vertical right margin line. |
| Editor\\Highlight\\Line Number | Background Color New | Gutter.Color | Background color of the gutter area. |
| Editor\\Highlight\\Line Number | Foreground Color New | Gutter.Font.Color | Color of the line numbers. |
| Editor\\Highlight\\Marked block | Background Color New | SelectedColor.Background | Background color of selected text. |
| Editor\\Highlight\\Marked block | Foreground Color New | SelectedColor.Foreground | Foreground color of selected text. |

### **Importing on TSynDelphiSyn:**

Imports syntax highlighting colors and styles from the active IDE Theme:

* **Colors**: Foreground and Background colors for all supported attributes.  
* **Styles**: Bold, Italic, and Underline formatting.  
* **Mappings**: Automatically maps Delphi's internal attribute names (e.g., "Reserved word", "Assembler") to the corresponding SynEdit properties.

#### Import Details:

**Registry Root:** `Editor\Highlight\`

For every row below, the importer reads:

* Foreground Color New \-\> Attribute.Foreground  
* Background Color New \-\> Attribute.Background  
* Bold \-\> Adds fsBold to Style  
* Italic \-\> Adds fsItalic to Style  
* Underline \-\> Adds fsUnderline to Style

| Registry Key Name | TSynDelphiSyn Attribute |
| :---- | :---- |
| Assembler | AsmAttri |
| Comment | CommentAttri |
| Preprocessor | DirectiveAttri |
| Identifier | IdentifierAttri |
| Reserved word | KeyAttri |
| Number | NumberAttri |
| Float | FloatAttri |
| Hex | HexAttri |
| Whitespace | SpaceAttri |
| String | StringAttri |
| Character | CharAttri |
| Symbol | SymbolAttri |