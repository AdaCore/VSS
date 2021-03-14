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

with Ada.Unchecked_Deallocation;

with VSS.Stream_Element_Vectors.Internals;
with VSS.Strings.Converters.Decoders.UTF8;

package body VSS.Strings.Converters.Decoders is

   procedure Free is
     new Ada.Unchecked_Deallocation (Abstract_Decoder'Class, Decoder_Access);

   ------------
   -- Decode --
   ------------

   function Decode
     (Self : in out Virtual_String_Decoder'Class;
      Data : VSS.Stream_Element_Vectors.Stream_Element_Vector)
      return VSS.Strings.Virtual_String
   is
      use type Ada.Streams.Stream_Element_Offset;

      Length  : Ada.Streams.Stream_Element_Count;
      Storage :
        VSS.Stream_Element_Vectors.Internals.Stream_Element_Array_Access;

   begin
      VSS.Stream_Element_Vectors.Internals.Data_Constant_Access
        (Data, Length, Storage);

      return Result : VSS.Strings.Virtual_String do
         if Length /= 0 and Self.Decoder /= null then
            Self.Decoder.Decode (Storage (1 .. Length), Result.Data);
         end if;
      end return;
   end Decode;

   ------------
   -- Decode --
   ------------

   function Decode
     (Self : in out Virtual_String_Decoder'Class;
      Data : Ada.Streams.Stream_Element_Array)
      return VSS.Strings.Virtual_String is
   begin
      return Result : VSS.Strings.Virtual_String do
         if Data'Length /= 0 and Self.Decoder /= null then
            Self.Decoder.Decode (Data, Result.Data);
         end if;
      end return;
   end Decode;

   -------------------
   -- Error_Message --
   -------------------

   function Error_Message
     (Self : Virtual_String_Decoder'Class) return VSS.Strings.Virtual_String is
   begin
      if Self.Decoder /= null then
         return Self.Decoder.Error_Message;

      else
         return VSS.Strings.Empty_Virtual_String;
      end if;
   end Error_Message;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Virtual_String_Decoder) is
   begin
      Free (Self.Decoder);
   end Finalize;

   ---------------
   -- Has_Error --
   ---------------

   function Has_Error (Self : Virtual_String_Decoder'Class) return Boolean is
   begin
      if Self.Decoder /= null then
         return Self.Decoder.Has_Error;

      else
         return False;
      end if;
   end Has_Error;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self     : in out Virtual_String_Decoder'Class;
      Encoding : VSS.Strings.Virtual_String;
      Flags    : Converter_Flags := Default_Converter_Flags) is
   begin
      Free (Self.Decoder);

      if Encoding = "utf-8" then
         Self.Decoder := new VSS.Strings.Converters.Decoders.UTF8.UTF8_Decoder;
         Self.Decoder.Initialize (Flags);
      end if;
   end Initialize;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Self : Virtual_String_Decoder'Class) return Boolean is
   begin
      return Self.Decoder /= null;
   end Is_Valid;

   -----------------
   -- Reset_State --
   -----------------

   procedure Reset_State (Self : in out Virtual_String_Decoder'Class) is
   begin
      if Self.Decoder /= null then
         Self.Decoder.Reset_State;
      end if;
   end Reset_State;

end VSS.Strings.Converters.Decoders;
