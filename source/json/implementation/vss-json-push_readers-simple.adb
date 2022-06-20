--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body VSS.JSON.Push_Readers.Simple is

   -------------------
   -- Error_Message --
   -------------------

   function Error_Message
     (Self : JSON_Simple_Push_Reader'Class)
      return VSS.Strings.Virtual_String is
   begin
      return Self.Message;
   end Error_Message;

   ---------------
   -- Has_Error --
   ---------------

   function Has_Error (Self : JSON_Simple_Push_Reader'Class) return Boolean is
      use type VSS.JSON.Pull_Readers.JSON_Reader_Error;

   begin
      return Self.Error /= VSS.JSON.Pull_Readers.No_Error;
   end Has_Error;

   -----------
   -- Parse --
   -----------

   procedure Parse (Self : in out JSON_Simple_Push_Reader'Class) is

      use type VSS.JSON.Content_Handlers.JSON_Content_Handler_Access;

      procedure Process_Custom_Error;
      --  Process error reported by the handlers if any.

      Success : Boolean := True;

      --------------------------
      -- Process_Custom_Error --
      --------------------------

      procedure Process_Custom_Error is
      begin
         if not Success then
            Self.Error   := VSS.JSON.Pull_Readers.Custom_Error;
            Self.Message := Self.Reader.Error_Message;
         end if;
      end Process_Custom_Error;

   begin
      while Success loop
         case Self.Reader.Read_Next is
            when VSS.JSON.Pull_Readers.No_Token =>
               --  Initial state, should not appear, because parsing has been
               --  started by call of Read_Next.

               null;

            when VSS.JSON.Pull_Readers.Invalid =>
               case Self.Reader.Error is
                  when VSS.JSON.Pull_Readers.No_Error =>
                     --  Must never happen.

                     null;

                  when VSS.JSON.Pull_Readers.Custom_Error
                     | VSS.JSON.Pull_Readers.Not_Valid
                  =>
                     Self.Error   := Self.Reader.Error;
                     Self.Message := Self.Reader.Error_Message;

                     exit;

                  when
                       VSS.JSON.Pull_Readers.Premature_End_Of_Document
                  =>
                     --  It is normal case for non-blocking parsing, nothing
                     --  do to.

                     exit;
               end case;

            when VSS.JSON.Pull_Readers.Start_Document =>
               if Self.Content /= null then
                  Self.Content.Start_Document (Success);
                  Process_Custom_Error;
               end if;

            when VSS.JSON.Pull_Readers.End_Document =>
               if Self.Content /= null then
                  Self.Content.End_Document (Success);
                  Process_Custom_Error;
               end if;

            when VSS.JSON.Pull_Readers.Start_Array =>
               if Self.Content /= null then
                  Self.Content.Start_Array (Success);
                  Process_Custom_Error;
               end if;

            when VSS.JSON.Pull_Readers.End_Array =>
               if Self.Content /= null then
                  Self.Content.End_Array (Success);
                  Process_Custom_Error;
               end if;

            when VSS.JSON.Pull_Readers.Start_Object =>
               if Self.Content /= null then
                  Self.Content.Start_Object (Success);
                  Process_Custom_Error;
               end if;

            when VSS.JSON.Pull_Readers.End_Object =>
               if Self.Content /= null then
                  Self.Content.End_Object (Success);
                  Process_Custom_Error;
               end if;

            when VSS.JSON.Pull_Readers.Key_Name =>
               if Self.Content /= null then
                  Self.Content.Key_Name (Self.Reader.Key_Name, Success);
                  Process_Custom_Error;
               end if;

            when VSS.JSON.Pull_Readers.String_Value =>
               if Self.Content /= null then
                  Self.Content.String_Value
                    (Self.Reader.String_Value, Success);
                  Process_Custom_Error;
               end if;

            when VSS.JSON.Pull_Readers.Number_Value =>
               if Self.Content /= null then
                  Self.Content.Number_Value
                    (Self.Reader.Number_Value, Success);
                  Process_Custom_Error;
               end if;

            when VSS.JSON.Pull_Readers.Boolean_Value =>
               if Self.Content /= null then
                  Self.Content.Boolean_Value
                    (Self.Reader.Boolean_Value, Success);
                  Process_Custom_Error;
               end if;

            when VSS.JSON.Pull_Readers.Null_Value =>
               if Self.Content /= null then
                  Self.Content.Null_Value (Success);
                  Process_Custom_Error;
               end if;
         end case;
      end loop;
   end Parse;

   -------------------------
   -- Set_Content_Handler --
   -------------------------

   overriding procedure Set_Content_Handler
     (Self : in out JSON_Simple_Push_Reader;
      To   : VSS.JSON.Content_Handlers.JSON_Content_Handler_Access) is
   begin
      Self.Content := To;
   end Set_Content_Handler;

   ----------------
   -- Set_Stream --
   ----------------

   procedure Set_Stream
     (Self   : in out JSON_Simple_Push_Reader'Class;
      Stream : not null VSS.Text_Streams.Input_Text_Stream_Access) is
   begin
      Self.Reader.Set_Stream (Stream);
   end Set_Stream;

end VSS.JSON.Push_Readers.Simple;
