------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                     Copyright (C) 2020-2021, AdaCore                     --
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

with VSS.Strings.Internals;
with VSS.Implementation.Strings;

package body VSS.String_Vectors is

   use type VSS.Implementation.String_Vectors.String_Vector_Data_Access;

   ---------
   -- "=" --
   ---------

   overriding function "="
     (Left  : Virtual_String_Vector;
      Right : Virtual_String_Vector) return Boolean
   is
      use type VSS.Implementation.Strings.String_Data;
   begin
      if Left.Length = Right.Length then
         for J in 1 .. Left.Length loop
            if Left.Data.Data (J) /= Right.Data.Data (J) then
               return False;
            end if;
         end loop;

         return True;

      else
         return False;
      end if;
   end "=";

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out Virtual_String_Vector) is
   begin
      VSS.Implementation.String_Vectors.Reference (Self.Data);
   end Adjust;

   ------------
   -- Append --
   ------------

   procedure Append
     (Self : in out Virtual_String_Vector'Class;
      Item : VSS.Strings.Virtual_String'Class) is
   begin
      VSS.Implementation.String_Vectors.Append
        (Self.Data, VSS.Strings.Internals.Data_Access_Constant (Item).all);
   end Append;

   -------------
   -- Element --
   -------------

   function Element
     (Self  : Virtual_String_Vector'Class;
      Index : Positive) return VSS.Strings.Virtual_String is
   begin
      if Self.Data /= null and then Index <= Self.Data.Last then
         return
           VSS.Strings.Internals.To_Virtual_String (Self.Data.Data (Index));

      else
         return VSS.Strings.Empty_Virtual_String;
      end if;
   end Element;

   -------------
   -- Element --
   -------------

   function Element
     (Self     : Virtual_String_Vector'Class;
      Position : Cursor) return VSS.Strings.Virtual_String is
   begin
      return Self.Element (Position.Index);
   end Element;

   -----------
   -- First --
   -----------

   overriding function First (Self : Reversible_Iterator) return Cursor is
   begin
      return (Index => (if Self.Last > 0 then 1 else 0));
   end First;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Virtual_String_Vector) is
   begin
      VSS.Implementation.String_Vectors.Unreference (Self.Data);
   end Finalize;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Self : Virtual_String_Vector'Class) return Boolean is
   begin
      return Self.Data = null or else Self.Data.Last = 0;
   end Is_Empty;

   -------------
   -- Iterate --
   -------------

   function Iterate
     (Self : Virtual_String_Vector'Class) return Reversible_Iterator is
   begin
      return (Last => Self.Length);
   end Iterate;

   ----------------
   -- Join_Lines --
   ----------------

   function Join_Lines
     (Self           : Virtual_String_Vector'Class;
      Terminator     : VSS.Strings.Line_Terminator;
      Terminate_Last : Boolean := True)
      return VSS.Strings.Virtual_String
   is
      Data : VSS.Implementation.Strings.String_Data;

   begin
      VSS.Implementation.String_Vectors.Join_Lines
        (Self.Data, Data, Terminator, Terminate_Last);

      return Result : constant VSS.Strings.Virtual_String :=
        VSS.Strings.Internals.To_Virtual_String (Data)
      do
         VSS.Implementation.Strings.Unreference (Data);
      end return;
   end Join_Lines;

   ----------
   -- Last --
   ----------

   overriding function Last (Self : Reversible_Iterator) return Cursor is
   begin
      return (Index => Self.Last);
   end Last;

   ------------
   -- Length --
   ------------

   function Length (Self : Virtual_String_Vector'Class) return Natural is
   begin
      return (if Self.Data = null then 0 else Self.Data.Last);
   end Length;

   ----------
   -- Next --
   ----------

   overriding function Next
     (Self     : Reversible_Iterator;
      Position : Cursor) return Cursor
   is
      Index : constant Natural :=
        (if Position.Index < Self.Last then Position.Index + 1 else 0);
   begin
      return (Index => Index);
   end Next;

   --------------
   -- Previous --
   --------------

   overriding function Previous
     (Self     : Reversible_Iterator;
      Position : Cursor) return Cursor
   is
      pragma Unreferenced (Self);
   begin
      return (Index => (if Position.Index > 0 then Position.Index - 1 else 0));
   end Previous;

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Self   : out Virtual_String_Vector) is
   begin
      raise Program_Error;
   end Read;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (Self  : in out Virtual_String_Vector'Class;
      Index : Positive;
      Item  : VSS.Strings.Virtual_String'Class) is
   begin
      if Self.Data /= null and then Index <= Self.Data.Last then
         VSS.Implementation.String_Vectors.Replace
           (Self.Data,
            Index,
            VSS.Strings.Internals.Data_Access_Constant (Item).all);
      end if;
   end Replace;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Self   : Virtual_String_Vector) is
   begin
      raise Program_Error;
   end Write;

end VSS.String_Vectors;
