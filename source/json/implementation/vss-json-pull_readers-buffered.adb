--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

package body VSS.JSON.Pull_Readers.Buffered is

   use all type VSS.JSON.Streams.JSON_Stream_Element_Kind;

   ------------
   -- At_End --
   ------------

   overriding function At_End
     (Self : JSON_Buffered_Pull_Reader) return Boolean is
   begin
      if Self.Current = 0 then
         return Self.Reader.At_End;
      end if;

      return False;
   end At_End;

   -------------------
   -- Boolean_Value --
   -------------------

   overriding function Boolean_Value
     (Self : JSON_Buffered_Pull_Reader) return Boolean is
   begin
      if Self.Current = 0 then
         return Self.Reader.Boolean_Value;
      end if;

      declare
         Aux : constant VSS.JSON.Streams.JSON_Stream_Element :=
           Self.Buffer (Self.Current);

      begin
         if Aux.Kind = Boolean_Value then
            return Aux.Boolean_Value;

         else
            return False;
         end if;
      end;
   end Boolean_Value;

   -----------
   -- Clear --
   -----------

   overriding procedure Clear (Self : in out JSON_Buffered_Pull_Reader) is
   begin
      raise Program_Error;
   end Clear;

   ------------------
   -- Element_Kind --
   ------------------

   overriding function Element_Kind
     (Self : JSON_Buffered_Pull_Reader)
      return VSS.JSON.Streams.JSON_Stream_Element_Kind is
   begin
      if Self.Current = 0 then
         return Self.Reader.Element_Kind;
      end if;

      return Self.Buffer (Self.Current).Kind;
   end Element_Kind;

   -----------
   -- Error --
   -----------

   overriding function Error
     (Self : JSON_Buffered_Pull_Reader) return JSON_Reader_Error is
   begin
      if Self.Current = 0 then
         return Self.Reader.Error;
      end if;

      return No_Error;
   end Error;

   -------------------
   -- Error_Message --
   -------------------

   overriding function Error_Message
     (Self : JSON_Buffered_Pull_Reader) return VSS.Strings.Virtual_String is
   begin
      if Self.Current = 0 then
         return Self.Reader.Error_Message;
      end if;

      return VSS.Strings.Empty_Virtual_String;
   end Error_Message;

   --------------
   -- Key_Name --
   --------------

   overriding function Key_Name
     (Self : JSON_Buffered_Pull_Reader) return VSS.Strings.Virtual_String is
   begin
      if Self.Current = 0 then
         return Self.Reader.Key_Name;
      end if;

      declare
         Aux : constant VSS.JSON.Streams.JSON_Stream_Element :=
           Self.Buffer (Self.Current);

      begin
         if Aux.Kind = Key_Name then
            return Aux.Key_Name;

         else
            return VSS.Strings.Empty_Virtual_String;
         end if;
      end;
   end Key_Name;

   ----------
   -- Mark --
   ----------

   procedure Mark (Self : in out JSON_Buffered_Pull_Reader'Class) is
      Aux : constant VSS.JSON.Streams.JSON_Stream_Element :=
        Self.Reader.Element;

   begin
      if Self.Current = 0 then
         Self.Buffer.Clear;

         if Aux.Kind in VSS.JSON.Streams.Valid_JSON_Stream_Element_Kind then
            Self.Buffer.Append (Aux);
         end if;

      else
         Self.Buffer.Delete
           (Self.Buffer.First_Index,
            Ada.Containers.Count_Type
              (Self.Current - Self.Buffer.First_Index));
         Self.Current := Self.Buffer.First_Index;
      end if;

      Self.Store := True;
   end Mark;

   ------------------
   -- Number_Value --
   ------------------

   overriding function Number_Value
     (Self : JSON_Buffered_Pull_Reader) return VSS.JSON.JSON_Number is
   begin
      if Self.Current = 0 then
         return Self.Reader.Number_Value;
      end if;

      declare
         Aux : constant VSS.JSON.Streams.JSON_Stream_Element :=
           Self.Buffer (Self.Current);

      begin
         if Aux.Kind = Number_Value then
            return Aux.Number_Value;

         else
            return (Kind => None);
         end if;
      end;
   end Number_Value;

   -----------------
   -- Raise_Error --
   -----------------

   overriding procedure Raise_Error
     (Self    : in out JSON_Buffered_Pull_Reader;
      Message : VSS.Strings.Virtual_String) is
   begin
      raise Program_Error;
   end Raise_Error;

   ---------------
   -- Read_Next --
   ---------------

   overriding function Read_Next
     (Self : in out JSON_Buffered_Pull_Reader)
      return VSS.JSON.Streams.JSON_Stream_Element_Kind is
   begin
      if Self.Current /= 0 then
         Self.Current := @ + 1;

         if Self.Current <= Self.Buffer.Last_Index then
            return Self.Buffer (Self.Current).Kind;
         end if;

         Self.Current := 0;

         if not Self.Store then
            Self.Buffer.Clear;
         end if;
      end if;

      return Result : constant VSS.JSON.Streams.JSON_Stream_Element_Kind :=
        Self.Reader.Read_Next
      do
         if Self.Store
           and then Result in VSS.JSON.Streams.Valid_JSON_Stream_Element_Kind
         then
            Self.Buffer.Append (Self.Reader.Element);
         end if;
      end return;
   end Read_Next;

   -----------
   -- Reset --
   -----------

   procedure Reset (Self : in out JSON_Buffered_Pull_Reader'Class) is
   begin
      Self.Current := Self.Buffer.First_Index;
   end Reset;

   ------------------------
   -- Skip_Current_Array --
   ------------------------

   overriding procedure Skip_Current_Array
     (Self : in out JSON_Buffered_Pull_Reader) is
   begin
      pragma Assert (Self.Is_Start_Array);

      Self.Read_Next;

      while not Self.Is_End_Array loop
         Self.Skip_Current_Value;
      end loop;

      Self.Read_Next;  --  Skip End_Array
   end Skip_Current_Array;

   -------------------------
   -- Skip_Current_Object --
   -------------------------

   overriding procedure Skip_Current_Object
     (Self : in out JSON_Buffered_Pull_Reader) is
   begin
      pragma Assert (Self.Is_Start_Object);

      Self.Read_Next;

      while not Self.Is_End_Object loop
         pragma Assert (Self.Is_Key_Name);

         Self.Read_Next;
         Self.Skip_Current_Value;
      end loop;

      Self.Read_Next;  --  Skip End_Object
   end Skip_Current_Object;

   ------------------------
   -- Skip_Current_Value --
   ------------------------

   overriding procedure Skip_Current_Value
     (Self : in out JSON_Buffered_Pull_Reader) is
   begin
      case Self.Element_Kind is
         when VSS.JSON.Streams.None =>
            raise Program_Error;
         when VSS.JSON.Streams.Invalid =>
            raise Program_Error;
         when VSS.JSON.Streams.Start_Document =>
            raise Program_Error;
         when VSS.JSON.Streams.End_Document =>
            raise Program_Error;
         when VSS.JSON.Streams.Comment =>
            Self.Read_Next;
         when VSS.JSON.Streams.Start_Array =>
            Self.Skip_Current_Array;
         when VSS.JSON.Streams.End_Array =>
            raise Program_Error;
         when VSS.JSON.Streams.Start_Object =>
            Self.Skip_Current_Object;
         when VSS.JSON.Streams.End_Object =>
            raise Program_Error;
         when VSS.JSON.Streams.Key_Name =>
            raise Program_Error;
         when VSS.JSON.Streams.String_Value =>
            Self.Read_Next;
         when VSS.JSON.Streams.Number_Value =>
            Self.Read_Next;
         when VSS.JSON.Streams.Boolean_Value =>
            Self.Read_Next;
         when VSS.JSON.Streams.Null_Value =>
            Self.Read_Next;
      end case;
   end Skip_Current_Value;

   ------------------
   -- String_Value --
   ------------------

   overriding function String_Value
     (Self : JSON_Buffered_Pull_Reader) return VSS.Strings.Virtual_String is
   begin
      if Self.Current = 0 then
         return Self.Reader.String_Value;
      end if;

      declare
         Aux : constant VSS.JSON.Streams.JSON_Stream_Element :=
           Self.Buffer (Self.Current);

      begin
         if Aux.Kind = String_Value then
            return Aux.String_Value;

         else
            return (VSS.Strings.Empty_Virtual_String);
         end if;
      end;
   end String_Value;

   ------------
   -- Unmark --
   ------------

   procedure Unmark (Self : in out JSON_Buffered_Pull_Reader'Class) is
   begin
      if Self.Current = 0 then
         Self.Buffer.Clear;

      else
         Self.Buffer.Delete
           (Self.Buffer.First_Index,
            Ada.Containers.Count_Type
              (Self.Current - Self.Buffer.First_Index));
         Self.Current := Self.Buffer.First_Index;
      end if;

      Self.Store := False;
   end Unmark;

end VSS.JSON.Pull_Readers.Buffered;
