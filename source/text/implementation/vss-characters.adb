------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with VSS.Implementation.UCD_Core;
with VSS.Unicode;

package body VSS.Characters is

   GC_To_General_Ccategory : constant
     array (VSS.Implementation.UCD_Core.GC_Values) of General_Category :=
     (VSS.Implementation.UCD_Core.GC_Cc => Control,
      VSS.Implementation.UCD_Core.GC_Cf => Format,
      VSS.Implementation.UCD_Core.GC_Cn => Unassigned,
      VSS.Implementation.UCD_Core.GC_Co => Private_Use,
      VSS.Implementation.UCD_Core.GC_Cs => Surrogate,
      VSS.Implementation.UCD_Core.GC_Ll => Lowercase_Letter,
      VSS.Implementation.UCD_Core.GC_Lm => Modifier_Letter,
      VSS.Implementation.UCD_Core.GC_Lo => Other_Letter,
      VSS.Implementation.UCD_Core.GC_Lt => Titlecase_Letter,
      VSS.Implementation.UCD_Core.GC_Lu => Uppercase_Letter,
      VSS.Implementation.UCD_Core.GC_Mc => Spacing_Mark,
      VSS.Implementation.UCD_Core.GC_Me => Enclosing_Mark,
      VSS.Implementation.UCD_Core.GC_Mn => Nonspacing_Mark,
      VSS.Implementation.UCD_Core.GC_Nd => Decimal_Number,
      VSS.Implementation.UCD_Core.GC_Nl => Letter_Number,
      VSS.Implementation.UCD_Core.GC_No => Other_Number,
      VSS.Implementation.UCD_Core.GC_Pc => Connector_Punctuation,
      VSS.Implementation.UCD_Core.GC_Pd => Dash_Punctuation,
      VSS.Implementation.UCD_Core.GC_Pe => Close_Punctuation,
      VSS.Implementation.UCD_Core.GC_Pf => Final_Punctuation,
      VSS.Implementation.UCD_Core.GC_Pi => Initial_Punctuation,
      VSS.Implementation.UCD_Core.GC_Po => Other_Punctuation,
      VSS.Implementation.UCD_Core.GC_Ps => Open_Punctuation,
      VSS.Implementation.UCD_Core.GC_Sc => Currency_Symbol,
      VSS.Implementation.UCD_Core.GC_Sk => Modifier_Symbol,
      VSS.Implementation.UCD_Core.GC_Sm => Math_Symbol,
      VSS.Implementation.UCD_Core.GC_So => Other_Symbol,
      VSS.Implementation.UCD_Core.GC_Zl => Line_Separator,
      VSS.Implementation.UCD_Core.GC_Zp => Paragraph_Separator,
      VSS.Implementation.UCD_Core.GC_Zs => Space_Separator);

   --------------------------
   -- Get_General_Category --
   --------------------------

   function Get_General_Category
     (Self : Virtual_Character) return General_Category
   is
      use type VSS.Implementation.UCD_Core.Core_Offset;
      use type VSS.Unicode.Code_Point;

      Block : constant VSS.Implementation.UCD_Core.Core_Index :=
        VSS.Implementation.UCD_Core.Core_Index
          (VSS.Unicode.Code_Point (Virtual_Character'Pos (Self))
           / VSS.Unicode.Code_Point (VSS.Implementation.UCD_Core.Block_Size));
      Offset : constant VSS.Implementation.UCD_Core.Core_Offset :=
        Virtual_Character'Pos (Self)
          mod VSS.Implementation.UCD_Core.Block_Size;

   begin
      return
        GC_To_General_Ccategory
          (VSS.Implementation.UCD_Core.Core_Data_Table
             (VSS.Implementation.UCD_Core.Core_Index_Table (Block)
              + Offset).GC);
   end Get_General_Category;

end VSS.Characters;
