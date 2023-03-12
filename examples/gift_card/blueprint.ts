export type Preamble = {
  title: string;
  description?: string;
  version: string;
  plutusVersion: string;
  license?: string;
};

export type Argument = {
  title?: string;
  description?: string;
  // schema: Record<string, unknown>;
};

export type Validator = {
  title: string;
  description?: string;
  datum?: Argument;
  redeemer: Argument;
  parameters?: Argument[];
  compiledCode: string;
  hash: string;
};

export type Blueprint = {
  preamble: Preamble;
  validators: Validator[];
};
