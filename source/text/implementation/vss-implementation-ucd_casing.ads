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
--  Utilities package for Unicode Casing Algorithm.

package VSS.Implementation.UCD_Casing is

   pragma Preelaborate;

   --  Data types to track 'Before C' expressions of the casing context.
   --  These types are bit packed to reduce cost of the casing context
   --  tracking to just few CPU instructions.
   --
   --  Casing_Context_Change type is used in generated code, thus any changes
   --  must be synchronized with code generator.

   type Casing_Context is record
      Final_Sigma       : Boolean := False;
      After_Soft_Dotted : Boolean := False;
      After_I           : Boolean := False;
   end record;
   for Casing_Context'Size use 6;
   for Casing_Context use record
      Final_Sigma       at 0 range 0 .. 0;
      After_Soft_Dotted at 0 range 2 .. 2;
      After_I           at 0 range 4 .. 4;
   end record;

   type Casing_Context_Change is record
      Enter_Final_Sigma          : Boolean := False;
      Continue_Final_Sigma       : Boolean := False;
      Enter_After_Soft_Dotted    : Boolean := False;
      Continue_After_Soft_Dotted : Boolean := False;
      Enter_After_I              : Boolean := False;
      Continue_After_I           : Boolean := False;
   end record;
   for Casing_Context_Change'Size use 6;
   for Casing_Context_Change use record
      Enter_Final_Sigma          at 0 range 0 .. 0;
      Continue_Final_Sigma       at 0 range 1 .. 1;
      Enter_After_Soft_Dotted    at 0 range 2 .. 2;
      Continue_After_Soft_Dotted at 0 range 3 .. 3;
      Enter_After_I              at 0 range 4 .. 4;
      Continue_After_I           at 0 range 5 .. 5;
   end record;

   procedure Apply
     (Context : in out Casing_Context;
      Change  : Casing_Context_Change) with Inline_Always;
   --  Apply given changes to given casing context.

end VSS.Implementation.UCD_Casing;
