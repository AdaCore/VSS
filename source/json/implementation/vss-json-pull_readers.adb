--
--  Copyright (C) 2020-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

package body VSS.JSON.Pull_Readers is

   ---------------
   -- Has_Error --
   ---------------

   function Has_Error (Self : JSON_Pull_Reader'Class) return Boolean is
   begin
      return Self.Error /= No_Error;
   end Has_Error;

   ---------------
   -- Read_Next --
   ---------------

   procedure Read_Next (Self : in out JSON_Pull_Reader'Class) is
      Dummy : constant VSS.JSON.Streams.JSON_Stream_Element_Kind :=
        Self.Read_Next;

   begin
      null;
   end Read_Next;

end VSS.JSON.Pull_Readers;
