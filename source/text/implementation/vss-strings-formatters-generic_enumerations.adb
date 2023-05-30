--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

package body VSS.Strings.Formatters.Generic_Enumerations is

   ------------
   -- Format --
   ------------

   overriding function Format
     (Self   : Formatter;
      Format : VSS.Strings.Formatters.Format_Information)
      return VSS.Strings.Virtual_String
   is
      Buffer : constant Wide_Wide_String :=
        Enumeration_Type'Wide_Wide_Image (Self.Value);

   begin
      return VSS.Strings.To_Virtual_String (Buffer);
   end Format;

   -----------
   -- Image --
   -----------

   function Image (Item : Enumeration_Type) return Formatter is
   begin
      return (Name => <>, Value => Item);
   end Image;

   -----------
   -- Image --
   -----------

   function Image
     (Name : VSS.Strings.Virtual_String;
      Item : Enumeration_Type) return Formatter is
   begin
      return (Name => Name, Value => Item);
   end Image;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Self : Formatter) return VSS.Strings.Virtual_String is
   begin
      return Self.Name;
   end Name;

end VSS.Strings.Formatters.Generic_Enumerations;
