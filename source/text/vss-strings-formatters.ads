--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  This package provides base type for formatters, used by string templates
--  processor. Children packages provides implementations of formatters for
--  some types, applications can define own formatters too.

package VSS.Strings.Formatters is

   pragma Preelaborate;

   type Field_Alignment is (Default, Left, Center, Right, Fill);

   type Format_Information is record
      Width     : VSS.Strings.Grapheme_Cluster_Count := 0;
      Alignment : Field_Alignment                    := Default;
      Format    : VSS.Strings.Virtual_String;
   end record;

   type Abstract_Formatter is limited interface;

   not overriding function Format
     (Self   : Abstract_Formatter;
      Format : Format_Information)
      return VSS.Strings.Virtual_String is abstract;

   --  type Float_Formatter (Value : Long_Float) is
   --    new Abstract_Formatter with private;

   --  type Float_Formatter is new Abstract_Formatter with private;
   --
   --  function Image (Value : Long_Float) return Float_Formatter;
   --
   --  function Format
   --    (Template    : VSS.Strings.Virtual_String;
   --     Parameter_1 : Abstract_Formatter'Class)
   --     return VSS.Strings.Virtual_String;

private

   --  type Float_Formatter (Value : Long_Float) is
   --    new Abstract_Formatter with null record;

   --  type Float_Formatter is new Abstract_Formatter with record
   --     Value : Long_Float;
   --  end record;
   --
   --  overriding function Format
   --    (Self   : Float_Formatter;
   --     Format : Format_Information) return VSS.Strings.Virtual_String;
   --
   --  type Virtual_String_Template is tagged record
   --     null;
   --  end record;

end VSS.Strings.Formatters;
