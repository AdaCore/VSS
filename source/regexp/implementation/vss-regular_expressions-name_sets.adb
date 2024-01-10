--
--  Copyright (C) 2022-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with VSS.Transformers.Casing;

with VSS.Regular_Expressions.Category_Maps;

package body VSS.Regular_Expressions.Name_Sets is

   Map : VSS.Regular_Expressions.Category_Maps.Maps.Map;

   -----------
   -- "not" --
   -----------

   function "not" (Left : General_Category_Set) return General_Category_Set is
     (Data => not Left.Data);

   ----------
   -- "or" --
   ----------

   function "or"
     (Left, Right : General_Category_Set) return General_Category_Set is
       (Data => Left.Data or Right.Data);

   -------------
   -- Include --
   -------------

   procedure Include
     (Self  : in out General_Category_Set;
      Value : VSS.Characters.General_Category) is
   begin
      Self.Data (Value) := True;
   end Include;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      VSS.Regular_Expressions.Category_Maps.Init (Map);
   end Initialize;

   -----------------------------
   -- To_General_Category_Set --
   -----------------------------

   procedure To_General_Category_Set
     (Name  : VSS.Strings.Virtual_String;
      Value : out General_Category_Set;
      Ok    : out Boolean)
   is
      Cursor : Category_Maps.Maps.Cursor;
   begin
      if Map.Is_Empty then
         Initialize;
      end if;

      Cursor :=
        Map.Find
          (VSS.Transformers.Casing.To_Simple_Lowercase.Transform (Name));

      if Category_Maps.Maps.Has_Element (Cursor) then
         Value := Category_Maps.Maps.Element (Cursor);
         Ok := True;
      else
         Ok := False;
      end if;
   end To_General_Category_Set;

end VSS.Regular_Expressions.Name_Sets;
