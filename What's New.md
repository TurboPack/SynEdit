# **TSynDelphiSyn & IDE Settings Importer**

A new updated Delphi syntax highlighter for SynEdit (TSynDelphiSyn) and a design-time tool (SynDelphiIDEImporter) to synchronize your SynEdit components with your current Delphi IDE configuration.

## **1\. TSynDelphiSyn Component**

**Unit:** `Source\Highlighters\SynHighlighterDelphi.pas`

**Inheritance:** `TSynCustomCodeFoldingHighlighter`

`TSynDelphiSyn` is a specialized syntax highlighter designed to support the modern Object Pascal dialect used in Delphi 13 (Athens). It is based on the standard Pascal highligher `TSynPascalSyn`, but extends it with support for new language features, and expanded keywords.

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