--
--  Copyright (C) 2020, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Data package for testing.

with VSS.Strings;

package Hello_World_Data is

   type Language is
     (Arabic,
      Chinese,
      English,
      French,
      German,
      Greek,
      Hebrew,
      Japanse,
      Korean,
      Russian,
      Thai,
      Turkish);

   function Name (Self : Language) return String;
   --  Return name of the language.

   function Name (Self : Language) return VSS.Strings.Virtual_String;
   --  Return name of the language.

   function Hello (Self : Language) return String;
   --  Return "Hello, world" translated to given language.

   function Hello (Self : Language) return VSS.Strings.Virtual_String;
   --  Return "Hello, world" translated to given language.

end Hello_World_Data;
