unit uconstants;

{$mode objfpc}{$H+}

interface

const
  ClpiDebugMode: Boolean = false;
  ClpiDebugReportOnlyFirstError: Boolean = true;
  ClpiInterpreterMaxVariables = 30; // amount of allowed unique variable names
  ClpiInterpreterMaxLoopCycles: Integer = 1000; // how many cycles each loop is allowed to run...

  CvarInvalid = -1; // invalid variable id

  CtokenIdentifier = 1;
  CtokenOperator = 2;
  CtokenString = 3;
  CtokenNumber = 4;
  CtokenComment = 5;
  CtokenSingle = 6;

  CopNOP = 0;
  CopAssign = 1;
  CopCommandBlock = 2;
  CopCondition = 3;
  CopLoopWhile = 4;
  CopLoopRepeatUntil = 5;
  CopLoopForTo = 6;
  CopLogicTrue = 7;
  CopLogicFalse = 8;
  CopLogicOr = 9;
  CopLogicAnd = 10;
  CopLogicNot = 11;
  CopCompareEqual = 12;
  CopCompareUnequal = 13;
  CopCompareGreater = 14;
  CopCompareSmaller = 15;
  CopCompareGreaterOrEqual = 16;
  CopCompareSmallerOrEqual = 17;
  CopMathAdd = 18;
  CopMathSub = 19;
  CopMathMul = 20;
  CopMathDiv = 21;
  CopMathDivInt = 22;
  CopMathMod = 23;
  CopVariable = 24;
  CopString = 25;
  CopNumber = 26;

  // Math (add more if needed...)
  CopRound = 50;

  // General Purpose and I/O (add more if needed...)
  CopWriteln = 100;

  CopInvalid = 255;

implementation

end.

