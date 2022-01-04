------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                    Copyright (C) 2019-2022, AdaCore                      --
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

with GNAT.String_Split;

package body GPR2.Builtin is

   --------------
   -- External --
   --------------

   function External
     (Context       : GPR2.Context.Object;
      Variable      : Name_Type;
      Default_Value : Source_Reference.Value.Object :=
                        Source_Reference.Value.Undefined;
      Sloc          : Source_Reference.Object :=
                        Source_Reference.Undefined)
      return Source_Reference.Value.Object is
   begin
      if Context.Contains (Variable) then
         return Source_Reference.Value.Object
           (Source_Reference.Value.Create
              ((if Source_Reference.Object (Default_Value).Is_Defined
                then Source_Reference.Object (Default_Value)
                else Sloc),
               Context (Variable)));

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
      Separator : Value_Not_Empty) return Containers.Value_List
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

end GPR2.Builtin;
