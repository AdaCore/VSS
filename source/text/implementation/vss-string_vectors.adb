--
--  Copyright (C) 2020-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

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

   ------------
   -- Append --
   ------------

   procedure Append
     (Self : in out Virtual_String_Vector'Class;
      Item : Virtual_String_Vector'Class) is
   begin
      if Item.Data /= null and then Item.Data.Last /= 0 then
         for J in Item.Data.Data'First .. Item.Data.Last loop
            VSS.Implementation.String_Vectors.Append
              (Self.Data, Item.Data.Data (J));
         end loop;
      end if;
   end Append;

   -------------------------
   -- Append_Syntax_Sugar --
   -------------------------

   procedure Append_Syntax_Sugar
     (Self : in out Virtual_String_Vector;
      Item : VSS.Strings.Virtual_String) is
   begin
      VSS.Implementation.String_Vectors.Append
        (Self.Data, VSS.Strings.Internals.Data_Access_Constant (Item).all);
   end Append_Syntax_Sugar;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out Virtual_String_Vector'Class) is
   begin
      VSS.Implementation.String_Vectors.Unreference (Self.Data);
   end Clear;

   --------------
   -- Contains --
   --------------

   function Contains
     (Self             : Virtual_String_Vector'Class;
      Item             : VSS.Strings.Virtual_String;
      Case_Sensitivity : VSS.Strings.Case_Sensitivity :=
        VSS.Strings.Case_Sensitive)
      return Boolean
   is
      use type VSS.Strings.Case_Sensitivity;

   begin
      if Case_Sensitivity /= VSS.Strings.Case_Sensitive then
         raise Program_Error;
      end if;

      return
        VSS.Implementation.String_Vectors.Contains
          (Self.Data,
           VSS.Strings.Internals.Data_Access_Constant (Item).all);
   end Contains;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Self  : in out Virtual_String_Vector'Class;
      Index : Positive) is
   begin
      if Self.Data /= null and then Index <= Self.Data.Last then
         VSS.Implementation.String_Vectors.Delete (Self.Data, Index);
      end if;
   end Delete;

   ------------------
   -- Delete_First --
   ------------------

   function Delete_First
     (Self : Virtual_String_Vector'Class) return Virtual_String_Vector is
   begin
      return Result : Virtual_String_Vector :=
        (Ada.Finalization.Controlled with Data => Self.Data)
      do
         VSS.Implementation.String_Vectors.Reference (Result.Data);

         if Result.Data /= null and then Result.Data.Last /= 0 then
            VSS.Implementation.String_Vectors.Delete (Result.Data, 1);
         end if;
      end return;
   end Delete_First;

   ------------------
   -- Delete_First --
   ------------------

   procedure Delete_First (Self : in out Virtual_String_Vector'Class) is
   begin
      if Self.Data /= null and then Self.Data.Last /= 0 then
         VSS.Implementation.String_Vectors.Delete (Self.Data, 1);
      end if;
   end Delete_First;

   -----------------
   -- Delete_Last --
   -----------------

   procedure Delete_Last (Self : in out Virtual_String_Vector'Class) is
   begin
      if Self.Data /= null and then Self.Data.Last /= 0 then
         VSS.Implementation.String_Vectors.Delete (Self.Data, Self.Data.Last);
      end if;
   end Delete_Last;

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

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Virtual_String_Vector) is
   begin
      VSS.Implementation.String_Vectors.Unreference (Self.Data);
   end Finalize;

   -----------
   -- First --
   -----------

   overriding function First (Self : Reversible_Iterator) return Cursor is
   begin
      return (Index => (if Self.Last > 0 then 1 else 0));
   end First;

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

   ----------
   -- Join --
   ----------

   function Join
     (Self      : Virtual_String_Vector'Class;
      Separator : VSS.Characters.Virtual_Character)
      return VSS.Strings.Virtual_String
   is
      First_Segment : Boolean := True;

   begin
      return Result : VSS.Strings.Virtual_String do
         for Item of Self loop
            if First_Segment then
               First_Segment := False;

            else
               Result.Append (Separator);
            end if;

            Result.Append (Item);
         end loop;
      end return;
   end Join;

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

   ------------------
   -- Last_Element --
   ------------------

   function Last_Element
     (Self : Virtual_String_Vector'Class) return VSS.Strings.Virtual_String is
   begin
      if Self.Data /= null and then Self.Data.Last /= 0 then
         return
           VSS.Strings.Internals.To_Virtual_String
             (Self.Data.Data (Self.Data.Last));

      else
         return VSS.Strings.Empty_Virtual_String;
      end if;
   end Last_Element;

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

   -------------
   -- Prepend --
   -------------

   procedure Prepend
     (Self : in out Virtual_String_Vector'Class;
      Item : VSS.Strings.Virtual_String'Class) is
   begin
      VSS.Implementation.String_Vectors.Prepend
        (Self.Data, VSS.Strings.Internals.Data_Access_Constant (Item).all);
   end Prepend;

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
