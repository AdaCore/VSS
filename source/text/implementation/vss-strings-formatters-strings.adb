--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

package body VSS.Strings.Formatters.Strings is

   ------------
   -- Format --
   ------------

   overriding function Format
     (Self   : Formatter;
      Format : VSS.Strings.Formatters.Format_Information)
      return VSS.Strings.Virtual_String is
   begin
      return Self.Value;
   end Format;

   -----------
   -- Image --
   -----------

   function Image (Item : VSS.Strings.Virtual_String) return Formatter is
   begin
      return (Value => Item);
   end Image;

end VSS.Strings.Formatters.Strings;
