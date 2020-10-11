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

with VSS.Strings.Internals;

package body VSS.String_Vectors is

   use type VSS.Implementation.String_Vectors.String_Vector_Data_Access;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out Virtual_String_Vector) is
   begin
      VSS.Implementation.String_Vectors.Reference (Self.Data);
   end Adjust;

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

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Virtual_String_Vector) is
   begin
      VSS.Implementation.String_Vectors.Unreference (Self.Data);
   end Finalize;

   ------------
   -- Length --
   ------------

   function Length (Self : Virtual_String_Vector'Class) return Natural is
   begin
      return (if Self.Data = null then 0 else Self.Data.Last);
   end Length;

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Self   : out Virtual_String_Vector) is
   begin
      raise Program_Error;
   end Read;

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
