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

with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
use  Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;

with UCD.Case_Folding_Loader;
with UCD.Characters;
with UCD.Derived_Core_Properties_Loader;
with UCD.Derived_Normalization_Props_Loader;
with UCD.Emoji_Data_Loader;
with UCD.Grapheme_Break_Property_Loader;
with UCD.Hangul_Syllable_Type_Loader;
with UCD.Prop_List_Loader;
with UCD.Property_Aliases_Loader;
with UCD.Property_Value_Aliases_Loader;
with UCD.Special_Casing_Loader;
with UCD.Unicode_Data_Loader;
with UCD.Word_Break_Property_Loader;

with Gen_UCD.Casing;
with Gen_UCD.Core_Properties;
with Gen_UCD.Normalization;

procedure Gen_UCD.Driver is
begin
   if Ada.Command_Line.Argument_Count /= 2 then
      raise Program_Error;
   end if;

   declare
      UCD_Root : constant Wide_Wide_String := Decode (Argument (1));

   begin
      UCD.Property_Aliases_Loader.Load (UCD_Root);
      UCD.Property_Value_Aliases_Loader.Load (UCD_Root);

      UCD.Characters.Initialize_Character_Database;

      UCD.Unicode_Data_Loader.Load (UCD_Root);
      UCD.Prop_List_Loader.Load (UCD_Root);
      UCD.Derived_Core_Properties_Loader.Load (UCD_Root);
      UCD.Grapheme_Break_Property_Loader.Load (UCD_Root);
      UCD.Word_Break_Property_Loader.Load (UCD_Root);
      UCD.Derived_Normalization_Props_Loader.Load (UCD_Root);
      UCD.Hangul_Syllable_Type_Loader.Load (UCD_Root);
      UCD.Special_Casing_Loader.Load (UCD_Root);
      UCD.Case_Folding_Loader.Load (UCD_Root);
      UCD.Emoji_Data_Loader.Load (UCD_Root);
   end;

   Put_Line ("Processing...");
   Gen_UCD.Core_Properties.Build;
   Gen_UCD.Casing.Build;
   Gen_UCD.Normalization.Build;

   declare
      Ada_File : File_Type;

   begin
      Put_Line ("Generating...");

      Create (Ada_File, Out_File, Argument (2));

      Gen_UCD.Core_Properties.Generate (Ada_File);
      Gen_UCD.Casing.Generate (Ada_File);
      Gen_UCD.Normalization.Generate (Ada_File);

      Close (Ada_File);
   end;
end Gen_UCD.Driver;
