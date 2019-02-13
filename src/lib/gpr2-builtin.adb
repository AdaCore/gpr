------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
--                                                                          --
-- This is  free  software;  you can redistribute it and/or modify it under --
-- terms of the  GNU  General Public License as published by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details.  You should have received  a copy of the  GNU  --
-- General Public License distributed with GNAT; see file  COPYING. If not, --
-- see <http://www.gnu.org/licenses/>.                                      --
--                                                                          --
------------------------------------------------------------------------------

with GNAT.String_Split;

package body GPR2.Builtin is

   --------------
   -- External --
   --------------

   function External
     (Context       : GPR2.Context.Object;
      Variable      : Name_Type;
      Default_Value : Source_Reference.Value.Object :=
                        Source_Reference.Value.Undefined)
      return Source_Reference.Value.Object is
   begin
      if Context.Contains (Variable) then
         return Source_Reference.Value.Object
           (Source_Reference.Value.Create
              (Source_Reference.Object (Default_Value), Context (Variable)));

      elsif Default_Value.Is_Defined then
         return Default_Value;

      else
         raise Project_Error
           with "undefined external reference """ & String (Variable) & '"';
      end if;
   end External;

   ----------------------
   -- External_As_List --
   ----------------------

   function External_As_List
     (Context   : GPR2.Context.Object;
      Variable  : Name_Type;
      Separator : Name_Type) return Containers.Value_List
   is
      use GNAT.String_Split;

      Result : Containers.Value_List;
   begin
      if Context.Contains (Variable) then
         declare
            Str    : constant String := String'(Context (Variable));
            Slices : Slice_Set;
         begin
            Create (Slices, Str, String (Separator), Mode => Single);

            for K in 1 .. Slice_Count (Slices) loop
               declare
                  Value : constant String := Slice (Slices, K);
               begin
                  --  We ingnore empty values at the start or at the end
                  if Value /= "" or else K not in 1 | Slice_Count (Slices) then
                     Result.Append (Value);
                  end if;
               end;
            end loop;
         end;
      end if;

      return Result;
   end External_As_List;

   -----------
   -- Split --
   -----------

   function Split
     (Value     : Name_Type;
      Separator : Name_Type) return Containers.Value_List
   is
      use GNAT.String_Split;

      Result : Containers.Value_List;
      Slices : Slice_Set;
   begin
      Create (Slices, String (Value), String (Separator), Mode => Multiple);

      for K in 1 .. Slice_Count (Slices) loop
         declare
            Value : constant String := Slice (Slices, K);
         begin
            if Value /= "" then
               Result.Append (Value);
            end if;
         end;
      end loop;

      return Result;
   end Split;

end GPR2.Builtin;
