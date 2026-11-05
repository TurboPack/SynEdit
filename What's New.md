# **TSynDelphiSyn & IDE Settings Importer**

A new updated Delphi syntax highlighter for SynEdit (TSynDelphiSyn) and a design-time tool (SynDelphiIDEImporter) to synchronize your SynEdit components with your current Delphi IDE configuration.

## **1\. TSynDelphiSyn Component**

**Unit:** `Source\Highlighters\SynHighlighterDelphi.pas`

**Inheritance:** `TSynCustomCodeFoldingHighlighter`

`TSynDelphiSyn` is a specialized syntax highlighter designed to support the modern Object Pascal dialect used in Delphi 13 (Florence). It is based on the standard Pascal highligher `TSynPascalSyn`, but extends it with support for new language features, and expanded keywords.

### **Key Features**

* **Delphi 13 Language Support**: Includes support for modern keywords such as reference, helper, operator, strict, sealed, final, delayed, and more.  
* **Multiline String Literals**: Native support for Delphi's triple-quoted strings (''' ... ''').  
* **Code Folding**: Built-in folding logic for:  
  * {$REGION} / {$ENDREGION} directives.  
  * implementation sections.  
  * Classes, Records, and Methods (procedure, function, etc.).  
  * Standard blocks (begin..end, case, try).  
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

***Notice:*** The import **overwrites** the current font, colors, gutter, selection, tab and highlighting settings of the component, and cannot be undone. For this reason it is only available through the context menu verb described below, it is **not** triggered by double-clicking the component, and it always asks for a confirmation before proceeding.

### **Usage:**

1. Open the Form Designer in Delphi.  
2. Select a TSynEdit or TSynDelphiSyn component.  
3. **Right-click** the component to open the context menu.  
4. Select **"Import IDE Settings"**.  
5. Confirm the warning dialog (it defaults to **No**, so pressing Enter or Esc cancels the import).

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

---

# **TSynAsmX86\_64**

A syntax highlighter for x86-64 assembler, aimed at disassembler and inline
assembler views (Keystone / Capstone / iced based tools) where the existing
`TSynAsmSyn` is too coarse.

**Unit:** `Source\Highlighters\SynHighlighterAsmX86_64.pas`

**Inheritance:** `TSynCustomHighlighter`

**Language name:** `x86-64Assembly` - distinct from `TSynAsmSyn`, which keeps
`x86Assembly`, so both highlighters can be registered side by side.

### **Key Features**

* **Separate token classes instead of one keyword colour**: general mnemonics,
  general purpose registers, float/vector registers, operand size specifiers,
  jumps, conditional instructions and AVX instructions each get their own
  attribute, so a disassembly listing stays readable without reading it.
* **Labels**: a trailing colon marks a declaration; a bare identifier is shown
  as a label only when a matching declaration exists in the text.
* **Literals**: decimal, hexadecimal, octal, float and character literals have
  their own attributes.
* **Keyword lookup**: a single case-insensitive `TDictionary`, the same
  mechanism `TSynAsmSyn` uses. 1801 keywords across the seven classes.

### **Attributes**

| Property | Description |
| :---- | :---- |
| KeyAttri | General mnemonics (mov, add, call, ...). |
| RegisterAttri | General purpose registers (rax, r8d, ...). |
| FloatRegisterAttri | Float and vector registers (xmm0, ymm1, st0, ...). |
| TypeSizeAttri | Operand size specifiers (byte, word, dword, qword). |
| JumpAttri | Jump instructions (jmp, je, loop, ...). |
| ConditionalAttri | Conditional instructions (cmovz, setne, ...). |
| AVXAttri | AVX / AVX-512 instructions (vaddps, vpternlogd, ...). |
| LabelDeclarationAttri | A label declaration (name followed by a colon). |
| LabelAttri | A reference to a declared label. |
| CommentAttri | Comments (; and // style). |
| IdentifierAttri | Everything else that is an identifier. |
| NumberAttri | Decimal literals. |
| HexAttri | Hexadecimal literals. |
| OctalAttri | Octal literals. |
| FloatAttri | Floating-point literals. |
| CharAttri | Character literals. |
| StringAttri | String literals. |
| MathAttri | Arithmetic operators. |
| BracketAttri | Brackets around memory operands. |
| SymbolAttri | Remaining punctuation. |
| SpaceAttri | Whitespace. |
