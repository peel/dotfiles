{ config, pkgs, ... }:
{
  peel.keybindings.mappings  = {
    "C-l" = "centerSelectionInVisibleArea:";
    "C-/" = "undo:";
    "C- " = "setMark:";
    "C-w" = "deleteToMark:";
    "M-f" = "moveWordForward:";
    "M-b" = "moveWordBackward:";
    "M-<" = "moveToBeginningOfDocument:";
    "M->" = "moveToEndOfDocument:";
    "M-v" = "pageUp:";
    "M-/" = "complete:";
    "M-c" = [ "capitalizeWord:" "moveForward:" "moveForward:"];
    "M-u" = [ "uppercaseWord:" "moveForward:" "moveForward:"];
    "M-l" = [ "lowercaseWord:" "moveForward:" "moveForward:"];
    "M-d" = "deleteWordForward:";
    "C-M-h" = "deleteWordBackward:";
    "M-Bksp" = "deleteWordBackward:";
    "M-t" = "transposeWords:";
    "M-\@" = ["setMark:" "moveWordForward:" "swapWithMark"];
    "M-h" = ["setMark:" "moveToEndOfParagraph:" "swapWithMark"];
    "C-x" = {
      "h" = "selectAll:";
      "k" = "performClose:";
      "u" = "undo:";
      "C-f" = "openDocument:";
      "C-x" = "swapWithMark:";
      "C-m" = "selectToMark:";
      "C-s" = "saveDocument:";
      "C-w" = "saveDocumentAs:";
    };
  };
}
