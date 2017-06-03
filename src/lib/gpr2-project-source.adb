------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--         Copyright (C) 2016-2017, Free Software Foundation, Inc.          --
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

with GPR2.Message;
with GPR2.Project.Definition;
with GPR2.Project.Source.Set;
with GPR2.Project.Tree;
with GPR2.Source_Reference.Set;
with GPR2.Source_Reference.Identifier;

package body GPR2.Project.Source is

   ------------
   -- Create --
   ------------

   function Create
     (Source : GPR2.Source.Object;
      View   : Project.View.Object) return Object is
   begin
      return Object'(Source, View);
   end Create;

   ------------------
   -- Dependencies --
   ------------------

   function Dependencies
     (Self : Object;
      Mode : Dependency := Direct) return GPR2.Project.Source.Set.Object
   is
      procedure Insert
        (Deps : in out GPR2.Project.Source.Set.Object;
         Unit : Definition.Unit) with Inline;
      --  ???

      procedure To_Analyse (Src : GPR2.Source.Object);
      --  Record Src's withed units to be analysed (insert into Buf)

      ------------
      -- Insert --
      ------------

      procedure Insert
        (Deps : in out GPR2.Project.Source.Set.Object;
         Unit : Definition.Unit)
      is
         procedure Insert
           (Deps : in out GPR2.Project.Source.Set.Object;
            Src  : GPR2.Project.Source.Object) with Inline;
         --  Insert Src into the Deps if it does not exists

         ------------
         -- Insert --
         ------------

         procedure Insert
           (Deps : in out GPR2.Project.Source.Set.Object;
            Src  : GPR2.Project.Source.Object) is
         begin
            if not Deps.Contains (Src) then
               Deps.Insert (Src);
            end if;
         end Insert;

      begin
         Insert (Deps, Object'(Unit.Spec, Self.View));

         for B of Unit.Bodies loop
            Insert (Deps, Object'(B, Self.View));
         end loop;
      end Insert;

      Data : constant Definition.Data := Definition.Get (Self.View);

      Buf  : Source_Reference.Set.Object := Self.Source.Withed_Units;
      --  Buf contains units to be checked, this list is extended when looking
      --  for the full-closure. Using this list we avoid a recursive call.

      Done : Source_Reference.Set.Object;
      --  Fast look-up table to avoid analysing the same unit multiple time and
      --  more specifically avoid circularities.

      Deps : GPR2.Project.Source.Set.Object;
      --  The resulting source set

      ----------------
      -- To_Analyse --
      ----------------

      procedure To_Analyse (Src : GPR2.Source.Object) is
      begin
         if Src /= GPR2.Source.Undefined then
            for W of Src.Withed_Units loop
               if not Done.Contains (W) then
                  Buf.Insert (W);
               end if;
            end loop;
         end if;
      end To_Analyse;

   begin
      --  For Unit or Closure add dependencies from the other part

      if Mode in Unit | Closure then
         To_Analyse (Self.Source.Other_Part);
      end if;

      For_Every_Unit : loop
         exit For_Every_Unit when Buf.Is_Empty;

         declare
            W  : constant Source_Reference.Identifier.Object :=
                   Source_Reference.Identifier.Object (Buf.First_Element);
            SU : Definition.Unit;
         begin
            --  Remove the unit just taken from the list

            Buf.Delete_First;

            if not Done.Contains (W) then
               Done.Include (W);

               if Data.Units.Contains (W.Identifier) then
                  SU := Data.Units (W.Identifier);

                  --  At least the dependencies are the spec and body of the
                  --  withed unit.

                  Insert (Deps, SU);

                  --  Finaly, for the Closure mode add the dependencies of
                  --  withed unit from the direct withed spec and bodies.

                  if Mode = Closure then
                     To_Analyse (SU.Spec);

                     for B of SU.Bodies loop
                        To_Analyse (B);
                     end loop;
                  end if;

               else
                  Data.Tree.Log_Messages.Append
                    (Message.Create
                       (Message.Warning,
                        "withed unit " & String (W.Identifier) & " not found",
                        W));
               end if;
            end if;
         end;
      end loop For_Every_Unit;

      return Deps;
   end Dependencies;

   ------------
   -- Source --
   ------------

   function Source (Self : Object) return GPR2.Source.Object is
   begin
      return Self.Source;
   end Source;

   ----------
   -- View --
   ----------

   function View (Self : Object) return Project.View.Object is
   begin
      return Self.View;
   end View;

end GPR2.Project.Source;
