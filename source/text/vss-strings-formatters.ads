--
--  Copyright (C) 2023-2025, AdaCore
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
   --  Format value using given format specification.

   not overriding function Name
     (Self : Abstract_Formatter)
      return VSS.Strings.Virtual_String is abstract;
   --  Name of the parameter.
   --  @private

end VSS.Strings.Formatters;
