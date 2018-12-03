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

with Ada.Text_IO;

with GPR2.Source_Reference.Identifier;

package body GPR2.Source.Registry is

   ------------
   -- Shared --
   ------------

   protected body Shared is

      ---------
      -- Get --
      ---------

      function Get (Object : Source.Object) return Data is
      begin
         return Store (Object.Pathname);
      end Get;

      -----------------
      -- Print_Store --
      -----------------

      function Print_Store return Boolean is
         use Ada.Text_IO;
      begin
         if not GPR2.Debug then
            return False;
         end if;

         Put_Line ("Store:");
         for Cur in Store.Iterate loop
            declare
               K : constant GPR2.Path_Name.Object := Source_Store.Key (Cur);
               E : constant Data := Source_Store.Element (Cur);
            begin
               Put_Line ("  Key " & K.Value);
               Put_Line ("    Is_Ada_Source = " & E.Is_Ada_Source'Image);
               Put_Line ("    Language      = " & To_String (E.Language));
               Put_Line ("    Other_Part    = " &
                         (if E.Other_Part.Is_Defined then E.Other_Part.Value
                            else ""));
               if E.Is_Ada_Source then
                  Put_Line ("    Parsed        = " & E.Parsed'Image);
                  Put_Line ("    Compil units  = ");
                  for CU of E.CU_List loop
                     Put_Line ("     #" & CU.Index'Image);
                     Put_Line ("      name     = " & String (CU.Unit_Name));
                     Put_Line ("      kind     = " & CU.Kind'Image);
                     Put_Line ("      withed   = ");
                     for W of CU.Withed_Units loop
                        Put_Line ("         ("
                                  & String (W.Text) & ","
                                  & W.Line'Image & ","
                                  & W.Column'Image & ")");
                     end loop;
                     if CU.Is_Separate then
                        Put_Line ("      sep from = "
                                  & String (CU.Is_Separate_From));
                     end if;
                  end loop;
               end if;
            end;
         end loop;
         New_Line;

         return True;
      end Print_Store;

      --------------
      -- Register --
      --------------

      procedure Register (Def : Data) is
      begin
         if Store.Contains (Def.Path_Name) then
            --  Increase the ref-counter
            declare
               D : Data := Store (Def.Path_Name);
            begin
               D.Ref_Count := D.Ref_Count + 1;
               Store (Def.Path_Name) := D;
            end;

         else
            Store.Insert (Def.Path_Name, Def);
         end if;
      end Register;

      ---------
      -- Set --
      ---------

      procedure Set (Object : Source.Object; Def : Data) is
      begin
         Store (Object.Pathname) := Def;
      end Set;

      --------------------
      -- Set_Other_Part --
      --------------------

      procedure Set_Other_Part (Object1, Object2 : Object) is
         P1   : constant Source_Store.Cursor := Store.Find (Object1.Pathname);
         P2   : constant Source_Store.Cursor := Store.Find (Object2.Pathname);
         Def1 : Data := Store (P1);
         Def2 : Data := Store (P2);
      begin
         Def1.Other_Part := Object2.Pathname;
         Def2.Other_Part := Object1.Pathname;

         Store.Replace_Element (P1, Def1);
         Store.Replace_Element (P2, Def2);
      end Set_Other_Part;

      ----------------
      -- Unregister --
      ----------------

      procedure Unregister (Object : in out Source.Object) is
         D : Data := Get (Object);
      begin
         D.Ref_Count := D.Ref_Count - 1;

         if D.Ref_Count = 0 then
            D.CU_List.Clear;
            D.CU_Map.Clear;

            Store.Delete (Object.Pathname);

            Object := Undefined;

         else
            Store (Object.Pathname) := D;
         end if;
      end Unregister;

   end Shared;

end GPR2.Source.Registry;
