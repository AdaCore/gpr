--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Actions.Ada_Bind;
with GPR2.Build.Artifacts.Files;
with GPR2.Build.Tree_Db;
with Ada.Containers.Indefinite_Holders;

package body GPR2.Build.Actions.Ada_Compile.Pre_Bind is

   package Actions renames GPR2.Build.Actions;

   package Action_Holder is
      new Ada.Containers.Indefinite_Holders (Actions.Object'Class);

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Self : in out Object; Src : GPR2.Build.Compilation_Unit.Object)
   is
   begin
      Actions.Ada_Compile.Object (Self).Initialize (Src);
      Self.Traces := Create ("ACTION_ADA_COMPILE_PRE_BIND");
   end Initialize;

   ------------------
   -- Post_Command --
   ------------------

   overriding procedure Post_Command (Self : in out Object) is
      use Action_Holder;

      Bind_Holder : Action_Holder.Holder;
   begin
      GPR2.Build.Actions.Ada_Compile.Object (Self).Post_Command;

      for Successor of Self.Tree.Successors
         (Artifacts.Files.Create (Self.Ali_File))
      loop
         if Successor in Actions.Ada_Bind.Object'Class then
            Bind_Holder := To_Holder (Successor);
            exit;
         end if;
      end loop;

      if not Bind_Holder.Is_Empty then
         Actions.Ada_Bind.Object'Class
           (Reference (Bind_Holder).Element.all).Parse_Ali (Self.Ali_File);

         Self.Tree.Action_Id_To_Reference (Reference (Bind_Holder).UID) :=
           Reference (Bind_Holder).Element.all;
      end if;
   end Post_Command;

   ---------
   -- UID --
   ---------

   overriding function UID (Self : Object) return Actions.Action_Id'Class is
      Result : constant Ada_Compile_Pre_Bind_Id :=
                 (Name_Len  => Ada.Strings.Unbounded.Length (Self.Unit_Name),
                  Unit_Name => Name_Type (To_String (Self.Unit_Name)),
                  Ctxt      => Self.Ctxt);
   begin
      return Result;
   end UID;

end GPR2.Build.Actions.Ada_Compile.Pre_Bind;
