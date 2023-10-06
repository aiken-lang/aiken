hljs.registerLanguage("aiken", function (hljs) {
  const KEYWORDS =
    "as when is const validator fn if else let use opaque pub expect trace todo error type pure fail";
  const COMMAS = {
    scope: "ponctuation",
    begin: "[, ]+",
    relevance: 0,
  };
  const NUMBER = {
    scope: "number",
    variants: [
      {
        // binary
        begin: "\\b0[bB](?:_?[01]+)+",
      },
      {
        // octal
        begin: "\\b0[oO](?:_?[0-7]+)+",
      },
      {
        // hex
        begin: "\\b0[xX](?:_?[0-9a-fA-F]+)+",
      },
      {
        // dec, float
        begin: "\\b\\d(?:_?\\d+)*(?:\\.(?:\\d(?:_?\\d+)*)*)?",
      },
    ],
    relevance: 0,
  };
  const STRING = {
    scope: "string",
    variants: [{ begin: /@"/, end: /"/ }],
    contains: [hljs.BACKSLASH_ESCAPE],
    relevance: 0,
  };
  const BYTESTRING = {
    scope: "string",
    variants: [{ begin: /#?"/, end: /"/ }],
    contains: [hljs.BACKSLASH_ESCAPE],
    relevance: 0,
  };
  const BYTEARRAY = {
    scope: "string",
    variants: [{ begin: /#\[/, end: /\]/ }],
    contains: [
      { ...NUMBER, scope: "string" },
      COMMAS,
    ],
    relevance: 0,
  };
  const IMPORTS = {
    scope: "title",
    variants: [{ begin: "/\.{/", end: "}" }],
    contains: [
      {
        scope: "title",
        begin: "[A-Za-z][A-Za-z0-9_]*\\w*",
        relevance: 0,
      },
      COMMAS,
    ],
    relevance: 0,
  };
  const NAME = {
    scope: "variable",
    begin: "\\b[a-z][a-z0-9_]*\\b",
    relevance: 0,
  };
  const LABEL = {
    begin: [/\b[a-z][a-z0-9_]*/, ":"],
    beginScope: { 1: "symbol", 2: "ponctuation" },
    relevance: 1,
  };
  const DISCARD_NAME = {
    scope: "comment",
    begin: "\\b_[a-z][a-z0-9_]*\\b",
    relevance: 0,
  };

  return {
    name: "Aiken",
    aliases: ["aiken"],
    contains: [
      hljs.C_LINE_COMMENT_MODE,
      STRING,
      BYTESTRING,
      BYTEARRAY,
      {
        scope: "function",
        beginKeywords: "fn",
        end: "\\(",
        excludeEnd: true,
        contains: [
          {
            scope: "title.function",
            begin: "[a-z][a-z0-9_]*\\w*",
            relevance: 0,
          },
        ],
      },
      {
	begin: [/[a-z][a-z0-9_]*/, /[\n ]*\(/],
	beginScope: { 1: "title.function.invoke" },
      },
      {
        scope: "keyword",
        beginKeywords: "use",
        end: " ",
        excludeEnd: true,
        contains: [],
      },
      {
        scope: "keyword",
        beginKeywords: KEYWORDS,
      },
      {
        scope: "title",
        begin: "\\b[A-Z][A-Za-z0-9_]*\\b",
        relevance: 0,
      },
      {
        scope: "operator",
        begin: "[+\\-*/%!=<>&|.]+",
        relevance: 0,
      },
      IMPORTS,
      LABEL,
      DISCARD_NAME,
      NUMBER,
    ],
  };
});
