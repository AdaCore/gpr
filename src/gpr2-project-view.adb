------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--            Copyright (C) 2016, Free Software Foundation, Inc.            --
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

with GPR2.Project.Definition;

package body GPR2.Project.View is

   ----------------
   -- Attributes --
   ----------------

   function Attributes
     (Self  : Object;
      Name  : String := "";
      Index : String := "") return Attribute.Set.Object is
   begin
      return Definition.Get (Self).Attrs.Filter (Name, Index);
   end Attributes;

   -------------
   -- Context --
   -------------

   function Context (Self : Object) return GPR2.Context.Object is

      function Recursive_Context
        (Self   : Object;
         Status : Definition.Relation_Status) return GPR2.Context.Object;
      --  Recursively get the context for the view. This properly handle
      --  the context given by an aggregate project through the External
      --  attribute.

      -----------------------
      -- Recursive_Context --
      -----------------------

      function Recursive_Context
        (Self   : Object;
         Status : Definition.Relation_Status) return GPR2.Context.Object
      is
         Data : constant Definition.Data := Definition.Get (Self);

         function Get_Context return GPR2.Context.Object;

         -----------------
         -- Get_Context --
         -----------------

         function Get_Context return GPR2.Context.Object is
            use type Definition.Relation_Status;

            Context : GPR2.Context.Object := Data.Context;
         begin
            if Status = Definition.Aggregated then
               for C in Data.A_Context.Iterate loop
                  Context.Include
                    (GPR2.Context.Key_Value.Key (C),
                     GPR2.Context.Key_Value.Element (C));
               end loop;
            end if;

            return Context;
         end Get_Context;

      begin
         if Data.Context_View = Undefined then
            return (if Data.Has_Context
                    then Get_Context
                    else GPR2.Context.Empty);

         else
            return Ctx : GPR2.Context.Object :=
              Recursive_Context (Data.Context_View, Status)
            do
               --  And override by our definition if any
               if Data.Has_Context then
                  for C in Get_Context.Iterate loop
                     Ctx.Include
                       (GPR2.Context.Key_Value.Key (C),
                        GPR2.Context.Key_Value.Element (C));
                  end loop;
               end if;
            end return;
         end if;
      end Recursive_Context;

   begin
      return Recursive_Context (Self, Definition.Get (Self).Status);
   end Context;

   -------------
   -- From_Id --
   -------------

   function From_Id (Id : View.Id) return Object is
   begin
      return Object'(Id => Id);
   end From_Id;

   --------------------
   -- Has_Attributes --
   --------------------

   function Has_Attributes
     (Self  : Object;
      Name  : String := "";
      Index : String := "") return Boolean is
   begin
      if Name = "" and then Index = "" then
         return not Definition.Get (Self).Attrs.Is_Empty;

      elsif Index = "" then
         return Definition.Get (Self).Attrs.Contains (Name);

      else
         return not Attributes (Self, Name, Index).Is_Empty;
      end if;
   end Has_Attributes;

   -----------------
   -- Has_Context --
   -----------------

   function Has_Context (Self : Object) return Boolean is

      function Recursive_Has_Context
        (Self   : Object;
         Status : Definition.Relation_Status) return Boolean;
      --  Recursively check that the view has a context or not. This handles
      --  aggregated project context.

      -----------------------
      -- Recursive_Context --
      -----------------------

      function Recursive_Has_Context
        (Self   : Object;
         Status : Definition.Relation_Status) return Boolean
      is
         Data : constant Definition.Data := Definition.Get (Self);

         function Has_Context return Boolean;

         -----------------
         -- Get_Context --
         -----------------

         function Has_Context return Boolean is
            use type Definition.Relation_Status;
         begin
            return Data.Has_Context
              and then (not Data.Context.Is_Empty
                        or else (Status = Definition.Aggregated
                                 and then not Data.A_Context.Is_Empty));
         end Has_Context;

      begin
         if Data.Context_View = Undefined then
            return Data.Has_Context and then Has_Context;

         else
            return Has_Context
              or else Recursive_Has_Context (Data.Context_View, Status);
         end if;
      end Recursive_Has_Context;

   begin
      return Recursive_Has_Context (Self, Definition.Get (Self).Status);
   end Has_Context;

   -----------------
   -- Has_Imports --
   -----------------

   function Has_Imports (Self : Object) return Boolean is
      use type Ada.Containers.Count_Type;
   begin
      return not Definition.Get (Self).Trees.Imports.Is_Empty;
   end Has_Imports;

   ------------------
   -- Has_Packages --
   ------------------

   function Has_Packages (Self : Object) return Boolean is
   begin
      return not Definition.Get (Self).Packs.Is_Empty;
   end Has_Packages;

   -------------------
   -- Has_Variables --
   -------------------

   function Has_Variables
     (Self : Object;
      Name : String := "") return Boolean is
   begin
      if Name = "" then
         return not Definition.Get (Self).Vars.Is_Empty;
      else
         return Definition.Get (Self).Vars.Contains (Name);
      end if;
   end Has_Variables;

   ----------
   -- Kind --
   ----------

   function Kind (Self : Object) return Project_Kind is
   begin
      --  ?? for now just return the qualifier
      return Definition.Get (Self).Trees.Project.Qualifier;
   end Kind;

   ----------
   -- Name --
   ----------

   function Name (Self : Object) return Name_Type is
   begin
      return Definition.Get (Self).Trees.Project.Name;
   end Name;

   --------------
   -- Packages --
   --------------

   function Packages (Self : Object) return Pack.Set.Object is
   begin
      return Definition.Get (Self).Packs;
   end Packages;

   ---------------
   -- Path_Name --
   ---------------

   function Path_Name (Self : Object) return Path_Name_Type is
   begin
      return Definition.Get (Self).Trees.Project.Path_Name;
   end Path_Name;

   ---------------
   -- Qualifier --
   ---------------

   function Qualifier (Self : Object) return Project_Kind is
   begin
      return Definition.Get (Self).Trees.Project.Qualifier;
   end Qualifier;

   ---------------
   -- Signature --
   ---------------

   function Signature (Self : Object) return GPR2.Context.Binary_Signature is
   begin
      return Definition.Get (Self).Sig;
   end Signature;

   ---------------
   -- Variables --
   ---------------

   function Variables
     (Self : Object;
      Name : String := "") return Variable.Set.Object is
   begin
      if Name = "" then
         return Definition.Get (Self).Vars;

      else
         return Result : Variable.Set.Object do
            Result.Insert (Name, Definition.Get (Self).Vars (Name));
         end return;
      end if;
   end Variables;

end GPR2.Project.View;
