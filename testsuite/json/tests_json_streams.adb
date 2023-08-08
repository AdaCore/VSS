--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

package body Tests_JSON_Streams is

   use all type VSS.JSON.Streams.JSON_Stream_Element_Kind;

   ------------
   -- At_End --
   ------------

   overriding function At_End (Self : Replay_Pull_Reader) return Boolean is
   begin
      if Self.Current in Self.Data.First_Index .. Self.Data.Last_Index then
         return False;

      elsif Self.Current = 0 then
         return False;

      else
         return True;
      end if;
   end At_End;

   -------------------
   -- Boolean_Value --
   -------------------

   overriding function Boolean_Value
     (Self : Replay_Pull_Reader) return Boolean is
   begin
      if Self.Current in Self.Data.First_Index .. Self.Data.Last_Index
        and then Self.Data (Self.Current).Kind = Boolean_Value
      then
         return Self.Data (Self.Current).Boolean_Value;

      else
         return False;
      end if;
   end Boolean_Value;

   -----------
   -- Clear --
   -----------

   overriding procedure Clear (Self : in out Replay_Pull_Reader) is
   begin
      raise Program_Error;
   end Clear;

   ------------------
   -- Element_Kind --
   ------------------

   overriding function Element_Kind
     (Self : Replay_Pull_Reader)
      return VSS.JSON.Streams.JSON_Stream_Element_Kind is
   begin
      if Self.Current in Self.Data.First_Index .. Self.Data.Last_Index then
         return Self.Data (Self.Current).Kind;

      else
         return VSS.JSON.Streams.Invalid;
      end if;
   end Element_Kind;

   -----------
   -- Error --
   -----------

   overriding function Error
     (Self : Replay_Pull_Reader)
      return VSS.JSON.Pull_Readers.JSON_Reader_Error is
   begin
      raise Program_Error;
      return VSS.JSON.Pull_Readers.No_Error;
   end Error;

   -------------------
   -- Error_Message --
   -------------------

   overriding function Error_Message
     (Self : Replay_Pull_Reader) return VSS.Strings.Virtual_String is
   begin
      raise Program_Error;
      return VSS.Strings.Empty_Virtual_String;
   end Error_Message;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : in out Replay_Pull_Reader) is
   begin
      Self.Current := Self.Data.First_Index;
   end Initialize;

   --------------
   -- Key_Name --
   --------------

   overriding function Key_Name
     (Self : Replay_Pull_Reader) return VSS.Strings.Virtual_String is
   begin
      if Self.Current in Self.Data.First_Index .. Self.Data.Last_Index
        and then Self.Data (Self.Current).Kind = Key_Name
      then
         return Self.Data (Self.Current).Key_Name;

      else
         return VSS.Strings.Empty_Virtual_String;
      end if;
   end Key_Name;

   ------------------
   -- Number_Value --
   ------------------

   overriding function Number_Value
     (Self : Replay_Pull_Reader) return VSS.JSON.JSON_Number is
   begin
      if Self.Current in Self.Data.First_Index .. Self.Data.Last_Index
        and then Self.Data (Self.Current).Kind = Number_Value
      then
         return Self.Data (Self.Current).Number_Value;

      else
         return (Kind => VSS.JSON.None);
      end if;
   end Number_Value;

   -----------------
   -- Raise_Error --
   -----------------

   overriding procedure Raise_Error
     (Self    : in out Replay_Pull_Reader;
      Message : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String) is
   begin
      raise Program_Error;
   end Raise_Error;

   ---------------
   -- Read_Next --
   ---------------

   overriding function Read_Next
     (Self : in out Replay_Pull_Reader)
      return VSS.JSON.Streams.JSON_Stream_Element_Kind is
   begin
      if Self.Current = 0 then
         raise Program_Error;

      elsif Self.Current in Self.Data.First_Index .. Self.Data.Last_Index then
         Self.Current := @ + 1;

         if Self.Current > Self.Data.Last_Index then
            return VSS.JSON.Streams.None;
         end if;

      else
         return VSS.JSON.Streams.None;
      end if;

      return Self.Data (Self.Current).Kind;
   end Read_Next;

   ------------------------
   -- Skip_Current_Array --
   ------------------------

   overriding procedure Skip_Current_Array
     (Self : in out Replay_Pull_Reader) is
   begin
      raise Program_Error;
   end Skip_Current_Array;

   -------------------------
   -- Skip_Current_Object --
   -------------------------

   overriding procedure Skip_Current_Object
     (Self : in out Replay_Pull_Reader) is
   begin
      raise Program_Error;
   end Skip_Current_Object;

   ------------------------
   -- Skip_Current_Value --
   ------------------------

   overriding procedure Skip_Current_Value
     (Self : in out Replay_Pull_Reader) is
   begin
      raise Program_Error;
   end Skip_Current_Value;

   ------------------
   -- String_Value --
   ------------------

   overriding function String_Value
     (Self : Replay_Pull_Reader) return VSS.Strings.Virtual_String is
   begin
      if Self.Current in Self.Data.First_Index .. Self.Data.Last_Index
        and then Self.Data (Self.Current).Kind = String_Value
      then
         return Self.Data (Self.Current).String_Value;

      else
         return VSS.Strings.Empty_Virtual_String;
      end if;
   end String_Value;

end Tests_JSON_Streams;
