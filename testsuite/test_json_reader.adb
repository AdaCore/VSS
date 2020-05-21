------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
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

with Ada.Calendar;
with Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Interfaces;

with Magic.Strings.Conversions;
with Magic.JSON.Streams.Readers.Simple;

with Memory_Text_Streams;

procedure Test_JSON_Reader is

   use all type Magic.JSON.Streams.Readers.JSON_Event_Kind;
   use all type Magic.JSON.Streams.Readers.JSON_Reader_Error;

   Input       : aliased Memory_Text_Streams.Memory_UTF8_Input_Stream;
   Reader      : Magic.JSON.Streams.Readers.Simple.JSON_Simple_Reader;
   Count       : Natural := 0;
   Perfomance  : Boolean := False;
   Incremental : Boolean := False;

begin
   if Ada.Command_Line.Argument_Count /= 2 then
      raise Program_Error with "incorrect command line arguments";
   end if;

   if Ada.Command_Line.Argument (1) = "s" then
      Incremental := False;
      Perfomance := False;

   elsif Ada.Command_Line.Argument (1) = "sp" then
      Incremental := False;
      Perfomance := True;

   elsif Ada.Command_Line.Argument (1) = "i" then
      Incremental := True;
      Perfomance := False;

   elsif Ada.Command_Line.Argument (1) = "ip" then
      Incremental := True;
      Perfomance := True;

   else
      raise Program_Error with "incorrect parsing mode";
   end if;

   declare
      File : Ada.Streams.Stream_IO.File_Type;
      Aux  : Ada.Streams.Stream_Element_Array (1 .. 1_024);
      Last : Ada.Streams.Stream_Element_Offset;

   begin
      Ada.Streams.Stream_IO.Open
        (File, Ada.Streams.Stream_IO.In_File, Ada.Command_Line.Argument (2));

      while not Ada.Streams.Stream_IO.End_Of_File (File) loop
         Ada.Streams.Stream_IO.Read (File, Aux, Last);

         for J in Aux'First .. Last loop
            Input.Buffer.Append (Aux (J));
         end loop;
      end loop;

      Ada.Streams.Stream_IO.Close (File);
   end;

   declare
      use type Ada.Calendar.Time;

      Start : Ada.Calendar.Time := Ada.Calendar.Clock;

   begin
      Input.Set_Incremental (Incremental);
      Reader.Set_Stream (Input'Unchecked_Access);

      while not Reader.At_End loop
         Reader.Read_Next;

         case Reader.Event_Kind is
            when Invalid =>
               if Reader.Error /= Premature_End_Of_Document then
                  Ada.Text_IO.Put_Line
                    (Magic.JSON.Streams.Readers.JSON_Event_Kind'Image
                       (Reader.Event_Kind)

                     & ' '
                     & Magic.JSON.Streams.Readers.JSON_Reader_Error'Image
                         (Reader.Error)
                     & " """
                     & Magic.Strings.Conversions.To_UTF_8_String
                         (Reader.Error_Message)
                     & '"');

                  if Reader.Error /= Not_Valid then
                     raise Program_Error;
                  end if;

               elsif not Incremental then
                  raise Program_Error;

               else
                  Count := Count + 1;

                  if Count > 1_000 then
                     raise Program_Error;
                  end if;
               end if;

            when Key_Name =>
               Count := 0;

               if not Perfomance then
                  Ada.Text_IO.Put_Line
                    (Magic.JSON.Streams.Readers.JSON_Event_Kind'Image
                       (Reader.Event_Kind)
                     & " """
                     & Magic.Strings.Conversions.To_UTF_8_String
                       (Reader.Key_Name)
                     & '"');
               end if;

            when String_Value =>
               Count := 0;

               if not Perfomance then
                  Ada.Text_IO.Put_Line
                    (Magic.JSON.Streams.Readers.JSON_Event_Kind'Image
                       (Reader.Event_Kind)
                     & " """
                     & Magic.Strings.Conversions.To_UTF_8_String
                       (Reader.String_Value)
                     & '"');
               end if;

            when Number_Value =>
               Count := 0;

               if not Perfomance then
                  Ada.Text_IO.Put_Line
                    (Magic.JSON.Streams.Readers.JSON_Event_Kind'Image
                       (Reader.Event_Kind)
                     & ' '
                     & Magic.JSON.JSON_Number_Kind'Image
                       (Reader.Number_Value.Kind)
                     & ' '
                     & (case Reader.Number_Value.Kind is
                          when Magic.JSON.None => "",
                          when Magic.JSON.Out_Of_Range =>
                            Magic.Strings.Conversions.To_UTF_8_String
                              (Reader.String_Value),
                          when Magic.JSON.JSON_Integer =>
                            Interfaces.Integer_64'Image
                              (Reader.Number_Value.Integer_Value),
                          when Magic.JSON.JSON_Float =>
                            Interfaces.IEEE_Float_64'Image
                              (Reader.Number_Value.Float_Value)));
               end if;

            when Boolean_Value =>
               Count := 0;

               if not Perfomance then
                  Ada.Text_IO.Put_Line
                    (Magic.JSON.Streams.Readers.JSON_Event_Kind'Image
                       (Reader.Event_Kind)
                     & " "
                     & Boolean'Image (Reader.Boolean_Value));
               end if;

            when others =>
               Count := 0;

               if not Perfomance then
                  Ada.Text_IO.Put_Line
                    (Magic.JSON.Streams.Readers.JSON_Event_Kind'Image
                       (Reader.Event_Kind));
               end if;
         end case;
      end loop;

      if Perfomance then
         Ada.Text_IO.Put_Line (Duration'Image (Ada.Calendar.Clock - Start));
      end if;
   end;
end Test_JSON_Reader;
